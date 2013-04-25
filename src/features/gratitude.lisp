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
         (gratitude (insert-db `(:type :gratitude
                                       :author ,author
                                       :subjects ,subjects
                                       :text ,text
                                       :created ,time))))
    (notice :new-gratitude :time time
                           :id gratitude)
    gratitude))

(defun index-gratitude (id data)
  (let* ((author (db (getf data :author)))
         (created (getf data :created))
         (subjects (getf data :subjects))
         (people (cons (getf data :author) subjects))
         (result (make-result :latitude (getf author :lat)
                              :longitude (getf author :long)
                              :people people
                              :time created
                              :type :gratitude
                              :id id)))

    (with-locked-hash-table (*db-results*)
      (setf (gethash id *db-results*) result))

    (with-locked-hash-table (*activity-person-index*)
      (dolist (person people)
        (asetf (gethash person *activity-person-index*)
               (sort (push result it) #'> :key #'result-time))))

    (unless (< (result-time result) (- (get-universal-time) 15552000))

      (geo-index-insert *activity-geo-index* result) 

      (unless (< (result-time result) (- (get-universal-time) 2592000))
        (with-mutex (*recent-activity-mutex*)
          (push result *recent-activity-index*))) 

      (dolist (subject subjects)
        (let ((user (db subject)))
          (geo-index-insert *activity-geo-index* (make-result :latitude (getf user :lat)
                                                              :longitude (getf user :long)
                                                              :people people
                                                              :id id
                                                              :time created)))))))

(defun parse-subject-list (subject-list &key remove)
  (delete-duplicates
    (iter (for subject in (split #\, subject-list))
          (unless (equalp subject remove)
            (acond
              ((scan +number-scanner+ subject)
               (setf subject (parse-integer subject))
               (awhen (db subject)
                 (when (or (eq (getf it :type) :person)
                           (eq (getf it :type) :project))
                   (collect subject at beginning))))
              ((gethash subject *username-index*)
               (collect it at beginning))
              ((gethash subject *email-index*)
               (collect it at beginning)))))))

(defun modify-gratitude (id text)
  (let ((result (gethash id *db-results*))
        (now (get-universal-time)))
    (setf (result-time result) now)
    (modify-db id :text text :edited now)))

(defun delete-gratitude (id)
  (let* ((result (gethash id *db-results*))
         (data (db id))
         (people (cons (getf data :author) (getf data :subjects))))

    (with-locked-hash-table (*db-results*)
      (remhash id *db-results*))

    (delete-comments id)

    (with-locked-hash-table (*activity-person-index*)
      (dolist (person people)
        (asetf (gethash person *activity-person-index*)
               (remove result it))))

    (geo-index-remove *activity-geo-index* result)
    (remove-from-db id)))

(defun gratitude-compose (&key subjects text next existing-url single-recipient)
  (if subjects
    (standard-page
     (if existing-url "Edit your statement of gratitude" "Express gratitude")
     (html
       (:div :class "item"
        (:h2 (str (if existing-url "Edit your statement of gratitude"
                                   "Express gratitude")))
       (:div :class "item"
        (:form :method "post" 
               :action (or existing-url "/gratitude/new") 
               :class "recipients"
          (:label "About:")
          (:menu :class "recipients"
           (unless subjects
             (htm (:li (:em "nobody yet"))))
           (dolist (subject subjects)
             (htm
               (:li 
                 (str (getf (db subject) :name)) 
                 (unless (or single-recipient existing-url) 
                   (htm 
                     (:button :class "text large x-remove" :type "submit" :name "remove" :value subject " ⨯ "))))
                (unless (or single-recipient existing-url)
                  (htm
                    (:li (:button :type "submit" :class "text" :name "add" :value "new" "+ Add a person or project"))))
               )))
          (when subjects
            (htm (:input :type "hidden" :name "subject" :value (format nil "~{~A~^,~}" subjects))))
          (when next
            (htm (:input :type "hidden" :name "next" :value next)))
          (:textarea :rows "8" :name "text" (str text))
          (:p  
            (:button :type "submit" :class "cancel" :name "cancel" "Cancel")
            (:button :class "yes" :type "submit" 
                     :name "create" 
                     (str (if existing-url "Save" "Create"))))))))
     :selected "people")
    (gratitude-add-subject :text text :next next)))

(defun gratitude-add-subject (&key subjects text next (results 'none))
  (standard-page
    "Express gratitude"
    (html
      (:div :class "item"
       (:h2 "Who would you like to write about?")
       (:h3 "Search for a person")
       (:form :method "post" :action "/gratitude/new"
         (:input :type "text" :name "name")
         (:input :type "submit" :class "submit" :name "search" :value "Search")

         (if (eq results 'none)
           (progn
             (htm
               (:h3 "Select one of your contacts")
               (:menu
                 (dolist (contact (contacts-alphabetically *user*))
                   (htm (:li (:button :class "text" :type "submit" :value (car contact) :name "add" (str (cadr contact)))))))))
           (progn
             (htm
               (:h3 "Search results")
               (dolist (person results)
                 (str (person-button (car person) (cdr person) "add"))))))

         (:input :type "submit" :class "cancel" :value "Back")

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
    (cond
      ((post-parameter "cancel")
       (see-other (or (post-parameter "next") "/home")))
      ((post-parameter "create")
       (let ((subjects (parse-subject-list (post-parameter "subject") :remove (write-to-string *userid*))))
         (cond
           ((and subjects (post-parameter "text"))
            (see-other (format nil (or (post-parameter "next") 
                                       "/gratitude/~A")
                               (create-gratitude :author *userid*
                                                 :subjects subjects
                                                 :text (post-parameter "text")))))
           (subjects
            "no text")
           ((post-parameter "text")
            "no subject")
           (t
            "totally blank"))))
      ((post-parameter "add")
       (if (string= (post-parameter "add") "new")
         (gratitude-add-subject :subjects (parse-subject-list (post-parameter "subject"))
                                :text (post-parameter "text")
                                :next (post-parameter "next"))
         (gratitude-compose
           :single-recipient (post-parameter "single-recipient")
           :next (post-parameter "next")
           :text (post-parameter "text")
           :subjects (parse-subject-list
                       (format nil "~A,~A" (post-parameter "add") (post-parameter "subject"))))))

      ((post-parameter "search")
       (gratitude-add-subject :subjects (parse-subject-list (post-parameter "subject"))
                              :text (post-parameter "text")
                              :next (post-parameter "next")
                              :results (search-people (post-parameter "name"))))
      (t
       (gratitude-compose
         :text (post-parameter "text")
         :subjects (parse-subject-list
                     (post-parameter "subject")
                     :remove (post-parameter "remove")))))))


(defun get-gratitude (id)
  (setf id (parse-integer id))
  (aif (db id)
    (require-user
      (standard-page
        "Gratitude"
        (html
          (str (gratitude-activity-item (make-result :id id
                                                     :time (getf it :created)
                                                     :people (cons (getf it :author) (getf it :subjects)))
                                        :next-url (script-name*))))))
    (not-found)))

(defun post-gratitude (id)
  (require-active-user
    (setf id (parse-integer id))
    (aif (db id)
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
        ((and (post-parameter "love")
              (member (getf it :type) '(:gratitude :offer :request)))
         (love id)
         (see-other (or (post-parameter "next") (referer))))
        ((and (post-parameter "unlove")
              (member (getf it :type) '(:gratitude :offer :request)))
         (unlove id)
         (see-other (or (post-parameter "next") (referer)))))
      (not-found))))

(defun get-gratitude-edit (id)
  (require-user
    (let* ((gratitude (db (parse-integer id))))
      (require-test ((eql *userid* (getf gratitude :author))
                     "You can only edit gratitudes you have written.")
        (gratitude-compose :subjects (getf gratitude :subjects)
                           :text (getf gratitude :text)
                           :existing-url (s+ "/gratitude/" id "/edit"))))))

(defun post-gratitude-edit (id)
  (require-user
    (let* ((gratitude (db (parse-integer id))))
      (require-test ((eql *userid* (getf gratitude :author))
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
