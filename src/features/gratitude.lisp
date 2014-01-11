;;; Copyright 2012-2013 CommonGoods Network, Inc.
;;;
;;; This file is part of Kindista.
;;;
;;; Kindista is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Affero General Public License as published
;;; by the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Kindista is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public
;;; License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with Kindista.  If not, see <http://www.gnu.org/licenses/>.

(in-package :kindista)

(defun new-gratitude-notice-handler ()
  (send-gratitude-notification-email (getf (cddddr *notice*) :id)))

(defun create-gratitude (&key author subjects text)
  (let* ((time (get-universal-time))
         (people-list (mailbox-ids subjects))
         (people (mapcar #'(lambda (mailbox)
                             (cons mailbox :unread))
                         people-list))
         (message-folders (list :inbox (remove-duplicates (mapcar #'car people-list))))
         (gratitude (insert-db `(:type :gratitude
                                 :author ,author
                                 :subjects ,subjects
                                 :people ,people
                                 :message-folders ,message-folders
                                 :text ,text
                                 :created ,time))))
    (unless (getf *user* :pending)
      (notice :new-gratitude :time time
                             :id gratitude))
    gratitude))

(defun index-gratitude (id data)
  (let* ((author-id (getf data :author))
         (author (db author-id))
         (pending (getf author :pending))
         (created (getf data :created))
         (subjects (getf data :subjects))
         (people (cons (getf data :author) subjects))
         (result (make-result :latitude (getf author :lat)
                              :longitude (getf author :long)
                              :people people
                              :time created
                              :type :gratitude
                              :id id)))

    (cond
      (pending
       (with-locked-hash-table (*pending-person-items-index*)
        (push id (gethash author-id *pending-person-items-index*))))

      (t
       (index-message id data)

       (with-locked-hash-table (*db-results*)
         (setf (gethash id *db-results*) result))

       (with-locked-hash-table (*gratitude-index*)
         (push id (gethash author-id *gratitude-index*)))

       (with-locked-hash-table (*profile-activity-index*)
         (dolist (person people)
           (asetf (gethash person *profile-activity-index*)
                  (sort (push result it) #'> :key #'result-time))))

       ;; unless gratitude is older than 180 days
       (unless (< (result-time result) (- (get-universal-time) 15552000))

         (geo-index-insert *activity-geo-index* result)

         ;;unless gratitude is older than 30 days
         (unless (< (result-time result) (- (get-universal-time) 2592000))
           (with-mutex (*recent-activity-mutex*)
             (push result *recent-activity-index*)))

         (with-locked-hash-table (*gratitude-results-index*)
           (dolist (subject subjects)
             (let* ((user (db subject))
                    (location (getf user :location))
                    (result (make-result :type :gratitude
                                         :latitude (getf user :lat)
                                         :longitude (getf user :long)
                                         :people people
                                         :id id
                                         :time created)))
               (push result (gethash id *gratitude-results-index*))
               (when location (geo-index-insert *activity-geo-index* result))))))))))

(defun parse-subject-list (subject-list &key remove)
  (delete-duplicates
    (iter (for subject in (split #\, subject-list))
          (unless (equalp subject remove)
            (acond
              ((scan +number-scanner+ subject)
               (setf subject (parse-integer subject))
               (awhen (db subject)
                 (when (or (eq (getf it :type) :person)
                           (eq (getf it :type) :group))
                   (collect subject at beginning))))
              ((gethash subject *username-index*)
               (collect it at beginning))
              ((gethash subject *email-index*)
               (collect it at beginning)))))))

(defun modify-gratitude (id text)
  (let ((result (gethash id *db-results*))
        (now (get-universal-time)))
    (refresh-item-time-in-indexes id :time now)
    (modify-db id :text text :edited now)))

(defun delete-gratitude (id)
  (let* ((result (gethash id *db-results*))
         (data (db id))
         (people (cons (getf data :author) (getf data :subjects)))
         (images (getf data :images)))

    (dolist (image-id images)
      (delete-image image-id))

    (with-locked-hash-table (*db-results*)
      (remhash id *db-results*))

    (with-locked-hash-table (*gratitude-results-index*)
      (dolist (result (gethash id *gratitude-results-index*))
        (geo-index-remove *activity-geo-index* result))
      (remhash id *gratitude-results-index*))

    (with-mutex (*recent-activity-mutex*)
      (asetf *recent-activity-index* (remove id it :key #'result-id)))

    (delete-comments id)

    (with-locked-hash-table (*profile-activity-index*)
      (dolist (person people)
        (asetf (gethash person *profile-activity-index*)
               (remove result it))))

    (when result (geo-index-remove *activity-geo-index* result))
    (remove-from-db id)))

(defun gratitude-compose (&key subjects text next existing-url single-recipient groupid)
  (if subjects
    (standard-page
     (if existing-url "Edit your statement of gratitude" "Express gratitude")
     (html
       (str (pending-disclaimer "statement of gratitude"))
       (:div :class "item"
        (:h2 (str (if existing-url "Edit your statement of gratitude"
                                   "Express gratitude")))
       (:div :class "item"
        (:form :method "post"
               :action (or existing-url "/gratitude/new")
               :class "recipients"
          (:label "About:")
          (:menu :type "toolbar" :class "recipients"
            (unless subjects
              (htm (:li (:em "nobody yet"))))
            (dolist (subject subjects)
              (htm
                (:li
                  (str (db subject :name))
                  (unless (or single-recipient existing-url)
                    (htm
                      (:button :class "text large x-remove" :type "submit" :name "remove" :value subject " ⨯ "))))))
            (unless (or single-recipient existing-url)
              (htm
                (:li :class "recipients" (:button :type "submit" :class "text" :name "add" :value "new" "+ Add a person or group")))))

          (when subjects
            (htm (:input :type "hidden" :name "subject" :value (format nil "~{~A~^,~}" subjects))))
          (when next
            (htm (:input :type "hidden" :name "next" :value next)))

          (unless existing-url
            (awhen (groups-with-user-as-admin)
              (htm
                (:div :class "clear"
                  (:label :class "from" "From:")
                  (str (identity-selection-html (or groupid *userid*)
                                              it
                                              :class "identity recipients profile-gratitude"))))))
          (:textarea :rows "8" :name "text" (str text))
          (:p
            (:button :type "submit" :class "cancel" :name "cancel" "Cancel")
            (:button :class "yes" :type "submit" 
                     :name "create" 
                     (str (if existing-url "Save" "Create"))))))))
     :selected "people")
    (gratitude-add-subject :text text :next next)))

(defun gratitude-add-subject (&key subjects text next (results 'none) groupid)
  (standard-page
    "Express gratitude"
    (html
      (:div :class "item"
       (str (pending-disclaimer "statement of gratitude"))
       (:h2 "Who would you like to write about?")
       (:h3 "Search for a person")
       (:form :method "post" :class "new-gratitude" :action "/gratitude/new"
         (:input :type "text" :name "name")
         (:button :type "submit" :class "yes input-height" :name "search" "Search")

         (if (eq results 'none)
           (progn
             (htm
               (:h3 "Or, select one of your contacts")
               (:menu :type "toolbar"
                 (dolist (contact (contacts-alphabetically *user*))
                   (htm (:li (:button :class "text" :type "submit" :value (car contact) :name "add" (str (cadr contact)))))))))
           (progn
             (htm
               (:h3 "Search results")
               (dolist (group (car results))
                 (str (id-button (car group) "add")))
               (dolist (person (cdr results))
                 (str (id-button (car person) "add" (cdr person)))))))

         (:input :type "submit" :class "cancel" :value "Back")

         (when groupid
           (htm (:input :type "hidden" :name "subject" :value groupid)))
         (when subjects
           (htm (:input :type "hidden" :name "subject" :value (format nil "~{~A~^,~}" subjects))))
         (when next
           (htm (:input :type "hidden" :name "next" :value next)))

         (when text
           (htm (:input :type "hidden" :name "text" :value (escape-for-html text))))

         )))
    :selected "people"))

(defun get-gratitudes-new ()
  (require-user
    (gratitude-compose :subjects (parse-subject-list (get-parameter "subject")))))

(defun post-gratitudes-new ()
  (require-active-user
    (let* ((groupid (when (scan +number-scanner+
                                (post-parameter "identity-selection"))
                     (parse-integer (post-parameter "identity-selection"))))
           (adminp (group-admin-p groupid)))
      (cond
        ((post-parameter "cancel")
         (see-other (or (post-parameter "next") "/home")))

        ((not (confirmed-location))
         (flash "You must set your street address on your settings page before you can post gratitude about someone." :error t)
         (see-other (or (post-parameter "next") "/home")))

        ((post-parameter "create")
         (let* ((subjects (parse-subject-list (post-parameter "subject")
                                              :remove (write-to-string *userid*)))
                (text (post-parameter "text"))
                (author (if adminp groupid *userid*))
                (new-id (create-gratitude :author author
                                          :subjects (remove author subjects)
                                          :text text)))
           (cond
             ((and subjects text)
              (if (getf *user* :pending)
                (progn
                  new-id
                  (flash "Your item has been recorded. It will be posted after we have a chance to review your initial account activity. In the meantime, please consider posting additional offers, requests, or statements of gratitude. Thank you for your patience.")
                  (see-other (or (post-parameter "next") "/home")))
                (see-other (format nil "/gratitude/~A" new-id))))
             (subjects
              "no text")
             ((post-parameter "text")
              "no subject")
             (t
              "totally blank"))))
        ((post-parameter "add")
         (if (string= (post-parameter "add") "new")
           (gratitude-add-subject :subjects (parse-subject-list (post-parameter "subject"))
                                  :groupid (when adminp groupid)
                                  :text (post-parameter "text")
                                  :next (post-parameter "next"))
           (gratitude-compose
             :single-recipient (post-parameter "single-recipient")
             :groupid (when adminp groupid)
             :next (post-parameter "next")
             :text (post-parameter "text")
             :subjects (parse-subject-list
                         (format nil "~A,~A" (post-parameter "add") (post-parameter "subject"))))))

        ((post-parameter "search")
         (gratitude-add-subject :subjects (parse-subject-list (post-parameter "subject"))
                                :text (post-parameter "text")
                                :groupid (when adminp groupid)
                                :next (post-parameter "next")
                                :results (cons (search-groups (post-parameter "name"))
                                               (search-people (post-parameter "name")))))
        (t
         (gratitude-compose
           :text (post-parameter "text")
           :groupid (when adminp groupid)
           :subjects (parse-subject-list
                       (post-parameter "subject")
                       :remove (post-parameter "remove"))))))))


(defun get-gratitude (id)
  (setf id (parse-integer id))
  (aif (db id)
    (require-user
      (standard-page
        "Gratitude"
        (html
          (:div :class "gratitude item"
            (str (gratitude-activity-item (make-result :id id
                                                     :time (getf it :created)
                                                     :people (cons (getf it :author)
                                                                   (getf it :subjects))))))
          (str (item-images-html id)))))
    (not-found)))

(defun post-gratitude (id)
  (require-active-user
    (setf id (parse-integer id))
    (aif (db id)
      (cond
        ((and (post-parameter "love")
              (member (getf it :type) '(:gratitude :offer :request)))
         (love id)
         (see-other (or (post-parameter "next") (referer))))
        ((and (post-parameter "unlove")
              (member (getf it :type) '(:gratitude :offer :request)))
         (unlove id)
         (see-other (or (post-parameter "next") (referer))))

        (t
         (require-test ((or (eql *userid* (getf it :author))
                            (group-admin-p (getf it :author))
                            (getf *user* :admin))
                       (s+ "You can only edit your own statatements of gratitude."))
           (cond
             ((post-parameter "delete")
              (confirm-delete :url (script-name*)
                              :type "gratitude"
                              :text (getf it :text)
                              :next-url (referer)))
             ((post-parameter "really-delete")
              (delete-gratitude id)
              (flash "Your statement of gratitude has been deleted!")
              (see-other (or (post-parameter "next") "/home")))
             ((post-parameter "edit")
              (see-other (strcat "/gratitude/" id "/edit")))))))
      (not-found))))

(defun get-gratitude-edit (id)
  (require-user
    (let* ((gratitude (db (parse-integer id)))
           (author (getf gratitude :author))
           (adminp (group-admin-p author)))
      (require-test ((or (eql *userid* author) adminp)
                     "You can only edit gratitudes you have written.")
        (gratitude-compose :subjects (getf gratitude :subjects)
                           :text (getf gratitude :text)
                           :existing-url (s+ "/gratitude/" id "/edit"))))))

(defun post-gratitude-edit (id)
  (require-user
    (let* ((gratitude (db (parse-integer id)))
           (author (getf gratitude :author)))
      (require-test ((or (eql *userid* author)
                         (group-admin-p author))
                     "You can only edit statements of gratitude that you have written.")
        (cond

          ((post-parameter "cancel")
           (see-other (or (post-parameter "next") "/home")))

          ((post-parameter "create")
             (if (post-parameter "text")
               (progn
                 (modify-gratitude (parse-integer id) (post-parameter "text"))
                 (see-other (s+ "/gratitude/" id)))
               "no text"))

          (t
           (gratitude-compose :subjects (getf gratitude :subjects)
                              :text (getf gratitude :text)
                              :existing-url (s+ "/gratitude/" id "/edit"))))))))
