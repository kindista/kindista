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

(defun new-pending-offer-notice-handler ()
  (send-pending-offer-notification-email (getf (cddddr *notice*) :id)))

(defun create-inventory-item (&key type (by *userid*) text tags privacy)
  (insert-db (list :type type
                   :by by
                   :privacy privacy ;a list of groups who can see the item
                   :text text
                   :tags tags
                   :created (get-universal-time))))

(defun index-inventory-item (id data)
  (let* ((by (getf data :by))
         (type (getf data :type))
         (pending (db by :pending))
         (result (make-result :latitude (or (getf data :lat) (db by :lat))
                              :longitude (or (getf data :long) (db by :long))
                              :id id
                              :type type
                              :people (list by)
                              :privacy (getf data :privacy)
                              :time (or (getf data :edited) (getf data :created))
                              :tags (getf data :tags)))
         (locationp (and (result-latitude result)
                        (result-longitude result))))

    (with-locked-hash-table (*db-results*)
      (setf (gethash id *db-results*) result))

    (cond
      (pending
       (with-locked-hash-table (*pending-person-items-index*)
         (push id (gethash by *pending-person-items-index*))))

      (t
       (with-locked-hash-table (*profile-activity-index*)
         (asetf (gethash by *profile-activity-index*)
                (sort (push result it) #'> :key #'result-time)))

       (if (eq type :offer)
         (with-locked-hash-table (*offer-index*)
           (push id (gethash by *offer-index*)))
         (with-locked-hash-table (*request-index*)
           (push id (gethash by *request-index*))))

       (when locationp

         (let ((stems (stem-text (getf data :text))))
           (if (eq type :offer)
             (with-locked-hash-table (*offer-stem-index*)
               (dolist (stem stems)
                 (push result (gethash stem *offer-stem-index*))))
             (with-locked-hash-table (*request-stem-index*)
               (dolist (stem stems)
                 (push result (gethash stem *request-stem-index*))))))

         (if (eq type :offer)
           (geo-index-insert *offer-geo-index* result)
           (geo-index-insert *request-geo-index* result))

         ;; unless item is older than 180 days
         (unless (< (result-time result) (- (get-universal-time) 15552000))
           ;; unless item is older than 30 days
           (unless (< (result-time result) (- (get-universal-time) 2592000))
             (with-mutex (*recent-activity-mutex*)
               (push result *recent-activity-index*)))
           (geo-index-insert *activity-geo-index* result)))

       (when (or (getf data :match-any-terms)
                 (getf data :match-all-terms))
         (index-matchmaker id data))))))

(defun modify-inventory-item (id &key text tags privacy latitude longitude)
  (let* ((result (gethash id *db-results*))
         (type (result-type result))
         (data (db id))
         (by (getf data :by))
         (now (get-universal-time)))

    (when text
      (let* ((oldstems (stem-text (getf data :text)))
             (newstems (stem-text text))
             (common (intersection oldstems newstems :test #'string=)))

        (flet ((commonp (stem)
                 (member stem common :test #'string=)))

          (setf oldstems (delete-if #'commonp oldstems))
          (setf newstems (delete-if #'commonp newstems))

          (when (eq type :offer)
            (with-locked-hash-table (*offer-stem-index*)
              (dolist (stem oldstems)
                (asetf (gethash stem *offer-stem-index*)
                       (remove result it))))
              (dolist (stem newstems)
                (push result (gethash stem *offer-stem-index*))))

          (when (eq type :request)
            (with-locked-hash-table (*request-stem-index*)
              (dolist (stem oldstems)
                (asetf (gethash stem *request-stem-index*)
                       (remove result it))))
              (dolist (stem newstems)
                (push result (gethash stem *request-stem-index*)))))))

    (unless (equal tags (getf data :tags))
      (setf (result-tags result) tags))

    (when (and latitude
               longitude
               (or (not (eql latitude (getf data :lat)))
                   (not (eql longitude (getf data :long)))))

      (if (eq type :offer)
        (geo-index-remove *offer-geo-index* result)
        (geo-index-remove *request-geo-index* result))
      (when (getf data :notify-matches)
        (geo-index-remove *matchmaker-requests-geo-index* result))
      (geo-index-remove *activity-geo-index* result)
      (setf (result-latitude result) latitude)
      (setf (result-longitude result) longitude)
      (if (eq type :offer)
        (geo-index-insert *offer-geo-index* result)
        (geo-index-insert *request-geo-index* result))
      (when (getf data :notify-matches)
        (geo-index-insert *matchmaker-requests-geo-index* result)))

    (setf (result-privacy result) privacy)

    (if (and (getf *user* :admin)
             (not (group-admin-p by))
             (not (eql *userid* by)))

      (if (and latitude longitude)
        (modify-db id :text text :tags tags :privacy privacy :lat latitude :long longitude)
        (modify-db id :text text :tags tags :privacy privacy))

      (progn
        (refresh-item-time-in-indexes id :time now)
        (modify-db id :text text :tags tags :privacy privacy :lat latitude :long longitude :edited now)))

    (when (eql (result-type result) :offer)
      (update-matchmaker-offer-data id))))

(defun delete-inventory-item (id)
  (let* ((result (gethash id *db-results*))
         (type (result-type result))
         (data (db id))
         (images (getf data :images))
         (type-index (case type
                       (:offer *offer-index*)
                       (:request *request-index*)))
         (stem-index (case type
                       (:offer *offer-stem-index*)
                       (:request *request-stem-index*)
                       (:event *event-stem-index*)))
         (geo-index (case type
                      (:offer *offer-geo-index*)
                      (:request *request-geo-index*)
                      (:event *event-geo-index*)))
         (stems (if (eq type :event)
                  (stem-text (s+ (getf data :title) " " (getf data :details)))
                  (stem-text (getf data :text)))))

    (when result
      (with-locked-hash-table (stem-index)
        (dolist (stem stems)
          (asetf (gethash stem stem-index)
                 (remove result it))))

      (dolist (image-id images)
        (delete-image image-id))

      (geo-index-remove geo-index result)

      (if (eq type :event)
        (with-mutex (*event-mutex*)
          (asetf *event-index* (remove result it)))
        (with-locked-hash-table (type-index)
          (asetf (gethash (getf data :by) type-index)
                 (remove id it))))

      (unless (eq type :event)
        (with-locked-hash-table (*profile-activity-index*)
          (asetf (gethash (getf data :by) *profile-activity-index*)
                 (remove result it)))
        (geo-index-remove *activity-geo-index* result)
        (with-mutex (*recent-activity-mutex*)
          (asetf *recent-activity-index* (remove id it :key #'result-id))))

      (with-locked-hash-table (*love-index*)
        (dolist (person-id (gethash id *love-index*))
          (amodify-db person-id :loves (remove id it))))

      (with-locked-hash-table (*db-results*)
        (remhash id *db-results*)))

    (delete-comments id)
    (remove-from-db id)))

(defun delete-pending-inventory-item (id)
  (let ((data (db id)))
    (with-locked-hash-table (*pending-person-items-index*)
      (asetf (gethash (getf data :by) *pending-person-items-index*)
             (remove id it)))
    (remove-from-db id)))

(defun deleted-invalid-item-reply-text (to-name from-name type &optional explanation)
  (strcat "Greetings " to-name ","
        #\linefeed
        #\linefeed
        "Your " type
        " violated Kindista's Terms of Use and we had to remove it. "
        "Please list multiple offers and requests separately (not in the same item). "
        "Kindista is for giving and receiving freely; please avoid any language which implies that you are expecting barter or money in exchange for your offer or request. "
        #\linefeed
        (aif explanation
          (strcat #\linefeed it #\linefeed)
          "")
        #\linefeed
        "To ensure that this doesn't happen again, please review Kindista's Sharing Guidelines before posting any additional offers or requests:"
        #\linefeed
        "https://kindista.org/faq#sharing-guidelines"
        #\linefeed
        #\linefeed
        "Please let me know if you have any questions with this policy."
        #\linefeed
        #\linefeed
        "In gratitude,"
        #\linefeed
        from-name ", Kindista"))

(defun create-reply (&key on text pending-deletion (user *userid*))
  (let* ((time (get-universal-time))
         (on-item (db on))
         (by (getf on-item :by))
         (participants (list user by))
         (senders (mailbox-ids (list user)))
         (bys (mailbox-ids (list by)))
         (sender-boxes (mapcar #'(lambda (mailbox)
                                   (cons mailbox :read))
                               senders))
         (by-boxes (mapcar #'(lambda (mailbox)
                                   (cons mailbox :unread))
                               bys))
         (people (append by-boxes sender-boxes))
         (people-ids (mapcar #'car (remove-duplicates (append senders bys))))
         (message-folders (list :inbox people-ids
                                :unread (remove user people-ids)))
         (id (insert-db (if pending-deletion
                          (list :type :reply
                                :on on
                                :deleted-item-text (getf on-item :text)
                                :deleted-item-type (getf on-item :type)
                                :by user
                                :participants participants
                                :message-folders message-folders
                                :people people
                                :created time)
                          (list :type :reply
                                :on on
                                :by user
                                :participants participants
                                :message-folders message-folders
                                :people people
                                :created time)))))

    (create-comment :on id :by (list user) :text text)

    id))

(defun post-new-inventory-item (type &key url)
  (require-active-user
    (let* ((tags (post-parameter-string-list "tag"
                                             #'(lambda (tag)
                                                 (scan *tag-scanner* tag))))
           (groups-selected (post-parameter-integer-list "groups-selected"))
           (groupid (post-parameter-integer "groupid"))
           (identity-selection (post-parameter-integer "identity-selection"))
           ;reset to public when changing identity
           (restrictedp (and (equalp (post-parameter "privacy-selection")
                                      "restricted")
                              (or (not identity-selection)
                                  (eql identity-selection
                                       (post-parameter-integer "prior-identity")))))
           (adminp (group-admin-p groupid))
           (text (when (scan +text-scanner+ (post-parameter "text"))
                   (post-parameter "text"))))

      (iter (for tag in (tags-from-string (post-parameter "tags")))
            (setf tags (cons tag tags)))

      (flet ((inventory-text ()
               (enter-inventory-text :type type
                                     :text text
                                     :action url
                                     :restrictedp restrictedp
                                     :next (or (post-parameter "next") (referer))
                                     :identity-selection identity-selection
                                     :groupid groupid
                                     :groups-selected groups-selected
                                     :tags tags
                                     :selected (s+ type "s")))
             (inventory-tags (&key error)
               (enter-inventory-tags :title (s+ "Preview your " type)
                                     :text text
                                     :next (post-parameter "next")
                                     :action url
                                     :restrictedp restrictedp
                                     :identity-selection identity-selection
                                     :groupid groupid
                                     :groups-selected groups-selected
                                     :tags tags
                                     :error error
                                     :button-text (s+ "Post " type)
                                     :selected (s+ type "s"))))

        (cond
         ((post-parameter "cancel")
          (see-other (or (post-parameter "next") "/home")))

         ((not (confirmed-location))
          (flash "You must set your street address on your settings page before you can post an offer or a request." :error t)
          (see-other (or (post-parameter "next") "/home")))

         ((and groupid (not adminp))
          (permission-denied))

         ((not text)
          (flash (s+ "Please enter a better description of your " type ".")
                 :error t)
          (inventory-text))

         ((> (length text) 1000)
          (flash (s+ "Please shorten your description. Offers and Requests must be no longer than 1000 characters including line breaks."))
          (inventory-text))

         ((and (not (post-parameter "create"))
               text)
           (inventory-tags))

         ((and restrictedp
               (post-parameter "create")
               (not groups-selected))
          (inventory-tags :error (s+ "Please allow at least one group to see this " type)))

         ((and (post-parameter "create")
               text)
          (if (intersection tags *top-tags* :test #'string=)
            (let ((new-id (create-inventory-item
                            :type (if (string= type "request") :request
                                                               :offer)
                            :by (if adminp groupid *userid*)
                            :privacy groups-selected
                            :text text
                            :tags tags)))
              (if (getf *user* :pending)
                (progn
                  new-id
                  (contact-opt-out-flash (list *userid*) :item-type type)
                  (when (string= type "offer")
                    (notice :new-pending-offer :id new-id))
                  (flash "Your item has been recorded. It will be posted after we have a chance to review your initial account activity. In the meantime, please consider posting additional offers, requests, or statements of gratitude. Thank you for your patience.")
                  (see-other "/home"))
                (progn
                  (contact-opt-out-flash (list *userid*) :item-type type)
                  (when (string= type "offer")
                    (update-matchmaker-offer-data new-id))
                  (see-other (format nil (strcat "/" type "s/" new-id))))))

            (inventory-tags :error "You must select at least one keyword")))

         (t (inventory-tags)))))))

(defun post-existing-inventory-item (type &key id url)
  (require-user
    (let* ((id (parse-integer id))
           (item (db id))
           (by (getf item :by))
           (adminp (group-admin-p by))
           (next (post-parameter "next")))

      (cond
        ((and (not (eql by *userid*))
              (item-view-denied (result-privacy (gethash id *db-results*))))
         (permission-denied))

        ((post-parameter "cancel")
         (see-other (or next "/home")))

        ((post-parameter "reply")
         (flet ((reply-html (type)
                  (inventory-item-reply type id item :next next)))
           (case (getf item :type)
             (:offer (reply-html "offer"))
             (:request (reply-html "request"))
             (t (not-found)))))

        ((post-parameter "reply-text")
         (create-reply :on id :text (post-parameter "reply-text"))
         (flash "Your reply has been sent.")
         (contact-opt-out-flash (list by (unless (eql *userid* by) *userid*)))
         (see-other (or next (script-name*))))

        ((post-parameter "love")
         (love id)
         (see-other (or (post-parameter "next") (referer))))

        ((post-parameter "unlove")
         (unlove id)
         (see-other (or (post-parameter "next") (referer))))

        (t
         (require-test ((or (eql *userid* (getf item :by))
                            adminp
                            (getf *user* :admin))
                      (s+ "You can only edit your own " type "s."))
           (let* ((tags (post-parameter-string-list "tag"
                                                    #'(lambda (tag)
                                                        (scan *tag-scanner*
                                                              tag))))
                  (groups-selected (or (post-parameter-integer-list "groups-selected")
                                       (getf item :privacy)))
                  (restrictedp (when
                                 (aif (post-parameter "privacy-selection")
                                   (string= "restricted" it)
                                   (getf item :privacy))
                                  t))
                  (new-text (post-parameter "text")))

             (iter (for tag in (tags-from-string (post-parameter "tags")))
                   (setf tags (cons tag tags)))

             (flet ((inventory-tags (&key error)
                      (enter-inventory-tags :title (s+ "Edit your " type)
                                            :action url
                                            :text (or new-text (getf item :text))
                                            :tags (or tags (getf item :tags))
                                            :groups-selected groups-selected
                                            :restrictedp restrictedp
                                            :next (or (post-parameter "next") (referer))
                                            :existingp t
                                            :error error
                                            :button-text (s+ "Save " type)
                                            :selected (s+ type "s"))))

               (cond

                ((> (length new-text) 1000)
                 (inventory-tags :error  "Please shorten your description. Offers and Requests must be no longer than 1000 characters including line breaks."))

                ((post-parameter "edit")
                 (inventory-tags))

                ((post-parameter "delete")
                 (confirm-delete :url url
                                 :type type
                                 :text (getf item :text)
                                 :next-url (if (string= (referer) (strcat "/" type "/" id))
                                             "/home"
                                             (referer))))

                ((and (post-parameter "really-delete")
                      (not (post-parameter "delete-inappropriate-item")))
                 (delete-inventory-item id)
                 (flash (s+ "Your " type " has been deleted!"))
                 (if (equal (fourth (split "/" (post-parameter "next") :limit 4)) (subseq (script-name*) 1))
                   (see-other "/home")
                   (see-other (or (post-parameter "next") "/home"))))

                ((post-parameter "delete-pending-item")
                 (require-admin
                   (delete-pending-inventory-item id))
                 (flash (strcat (string-capitalize type) " " id " has been deleted."))
                 (see-other (script-name*)))

                ((post-parameter "inappropriate-item")
                 (require-admin
                   (confirm-delete :url url
                                   :next-url (referer)
                                   :type type
                                   :text (getf item :text)
                                   :inappropriate-item t)))

                ((post-parameter "delete-inappropriate-item")
                 (require-admin
                   (create-reply :on id
                                 :pending-deletion t
                                 :text (post-parameter "explanation"))
                   (if (db by :pending)
                     (delete-pending-inventory-item id)
                     (delete-inventory-item id))
                   (flash (strcat (string-capitalize type) " " id " has been deleted."))
                   (see-other (post-parameter "next"))))

                ((and (post-parameter "create")
                      new-text)

                 (if (intersection tags *top-tags* :test #'string=)
                   (progn
                     (modify-inventory-item id :text (post-parameter "text")
                                               :tags tags
                                               :privacy (when restrictedp
                                                          groups-selected))
                     (see-other (or (post-parameter "next") (strcat "/" type "s/" id))))

                   (inventory-tags :error "You must select at least one keyword")))

                (t
                  (inventory-tags)))))))))))

(defun simple-inventory-entry-html (preposition type &key groupid)
  (html
    (:div :class "item"
      (:h4 (str (s+ "post " preposition " " type)))
      (:form :method "post" :action (s+ "/" type "s/new")
        (:input :type "hidden" :name "next" :value (script-name*))
        (awhen groupid
          (htm (:input :type "hidden" :name "groupid" :value it)))
        (:table :class "post"
          (:tr
            (:td (:textarea :cols "150" :rows "4" :name "text"))
            (:td
              (:button :class "yes submit" :type "submit" :name "post" "Post"))))))))

(defun enter-inventory-text (&key type title text groupid action selected tags next restrictedp identity-selection groups-selected)
  (standard-page
    (or title (if (string= type "offer")
                  "Post an offer"
                  "Post a request"))
    (let ((type (or type (if (string= selected "offers")
                           "offer"
                           "request"))))
      (html
       (:div :class "item"
         (str (pending-disclaimer type))
         (when (getf *user* :pending)
           (htm (:p "Now that you have created a Kindista account, "
                 "please post some "
                 (str type)
                 "s for your local community. "
                 (:strong
                   (:em "When posting multiple items, please enter them in "
                     "separate " (str type) "s.")))
                (:br)))
         (:h2 (str (s+ "Please describe your " type)))
         (:form :method "post" :action action
           (when groupid
             (htm (:input :type "hidden" :name "groupid" :value groupid)))
           (awhen identity-selection
             (htm (:input :type "hidden" :name "identity-selection" :value it)))
           (when restrictedp
             (htm (:input :type "hidden" :name "privacy-selection" :value "restricted")))
           (:input :type "hidden"
                   :name "prior-identity"
                   :value (or identity-selection groupid *userid*))
           (when groups-selected
             (dolist (group groups-selected)
               (htm (:input :type "hidden" :name "groups-selected" :value group))))
           (dolist (tag tags)
             (htm (:input :type "hidden" :name "tag" :value tag)))
           (when next
             (htm (:input :type "hidden" :name "next" :value next)))
           (:textarea :cols "40" :rows "5" :name "text" (str text))
           (:p  (:button :class "cancel" :type "submit" :class "cancel" :name "cancel" "Cancel")
                (:button :class "yes" :type "submit" :class "submit" :name "post" "Next"))))))
    :right (sharing-guide-sidebar)
    :selected selected))

(defun enter-inventory-tags (&key title action text existingp groupid identity-selection restrictedp error tags button-text selected groups-selected next)
  (let ((suggested (or tags (get-tag-suggestions text))))
    (standard-page title
     (html
       (:div :class "item inventory-details" :id "edit-tags"
        (str (pending-disclaimer))
        (when error
          (htm
            (:p :class "error" (str error))))
        (:h2 (str title) )
        (:form :class "post-next"
               :method "post"
               :action action
          (awhen next
            (htm (:input :type "hidden" :name "next" :value it)))
          (awhen groupid
            (htm (:input :type "hidden" :name "groupid" :value it)))
          (:input :type "hidden"
                  :name "prior-identity"
                  :value (or identity-selection groupid *userid*))

          (:textarea :class "review-text"
                     :name "text"
                     :rows "5"
                     (str text))

          (unless (or groupid existingp)
            (awhen (groups-with-user-as-admin)
              (htm
                (:label (str (s+ (if (string= selected "offers")
                                   "Offered" "Requested")
                                 " by")))
                (str (identity-selection-html identity-selection it :onchange "this.form.submit()")))))

          (when (or (getf *user-group-privileges* :member)
                    (getf *user-group-privileges* :admin)
                    groups-selected)
            (str (privacy-selection-html
                   (if (string= selected "offers") "offer" "request")
                   restrictedp
                   (if (or (and identity-selection
                                (not (= identity-selection *userid*)))
                           groupid)
                     (list (aif identity-selection
                             (cons it (db it :name))
                             (cons groupid (db groupid :name))))
                     (append (groups-with-user-as-member)
                             (groups-with-user-as-admin)))
                   groups-selected
                   :onchange "this.form.submit()")))

          (:h3 "Select at least one keyword")
          (dolist (tag *top-tags*)
            (htm 
              (:div :class "tag"
                (:input :type "checkbox"
                        :name "tag"
                        :value tag
                        :checked (when (member tag suggested :test #'string=)
                                   (setf suggested (remove tag suggested :test #'string=))
                                   ""))
                   (:span (str tag)))))
          (:h3 "Additional keywords (optional)")
          (:input :type "text" :name "tags" :size 40
                  :placeholder "e.g. produce, bicycle, tai-chi"
                  :value (format nil "~{~a~^,~^ ~}" suggested))

          (:p
            (:strong :class "red" "Important: ")
            "You must read the "
            (:a :href "/faq#sharing-guidelines" 
                :target "_blank"
                "Guidelines for Sharing on Kindista")
            " before posting any offers or requests. "
            "Failure to adhere to these guidelines may result in account suspension.")

          (:p (:button :class "cancel" :type "submit" :class "cancel" :name "cancel" "Cancel")
              (:button :class "yes" :type "submit" :class "submit" :name "create" (str button-text))))))
     :right (sharing-guide-sidebar)
     :selected selected)))

; author
; creation date
; edited date
; text
; tags (at least 1)
; privacy ('all 'contacts or listname)

(defun nearby-inventory-items (type &key base (subtag-count 4) (distance 50) q)
  (with-location
    (let ((nearby (if q
                    (result-id-intersection
                      (geo-index-query (case type
                                         (:offer *offer-geo-index*)
                                         (t *request-geo-index*))
                                       *latitude*
                                       *longitude*
                                       distance)
                      (stem-index-query (case type
                                         (:offer *offer-stem-index*)
                                         (t *request-stem-index*))
                                        q))
                    (geo-index-query (case type
                                       (:offer *offer-geo-index*)
                                       (t *request-geo-index*))
                                       *latitude*
                                       *longitude*
                                     distance)))
          (items nil))
      (let ((tags (make-hash-table :test 'equalp)))
        (dolist (item nearby)
          (dolist (tag (result-tags item))
            (push item (gethash tag tags))))

        (if base
          ; get each base tag's list of items
          ; get intersection of those lists
          ; remove base tags from hashtable
          ; set all remaining tags lists to be intersection of tag list and previous intersection
          (progn
            (setf items (iter (for tag in base)
                              (reducing (gethash tag tags) by #'result-id-intersection)
                              (remhash tag tags)))
            (iter (for (tag tag-items) in-hashtable tags)
                  (let ((new-items (intersection tag-items items :key #'result-id)))
                    (if new-items
                      (setf (gethash tag tags) new-items)
                      (remhash tag tags)))))

          (setf items nearby))

        ; for each tag, number of contents + ordered list of subtags (up to 4)
        (values (iter (for (tag tag-items) in-hashtable tags)
                      (collect (list tag
                                     (length tag-items)
                                     (when (cdr tag-items)
                                       (let* ((subtags (sort
                                                         (iter (for (subtag subtag-items) in-hashtable tags)
                                                               (unless (string= tag subtag)
                                                                 (awhen (intersection tag-items subtag-items :key #'result-id)
                                                                   (collect (cons subtag (length it))))))
                                                         #'> :key #'cdr))
                                              (top-subtags (subseq subtags 0
                                                                   (min (length subtags) subtag-count))))
                                         (if (< subtag-count (length subtags))
                                           (append (sort (subseq top-subtags 0 (- subtag-count 1))
                                                         #'string< :key #'car)
                                                   (list
                                                     (cons
                                                       "more"
                                                       (reduce #'+ (subseq subtags (- subtag-count 1)) :key #'cdr))))
                                           (sort top-subtags #'string< :key #'car)))))))
                (sort items #'> :key #'inventory-rank))))))

(defun nearby-inventory-top-tags (type &key (count 9) (more t) base (subtag-count 4) q)
  (multiple-value-bind (nearby items)
      (nearby-inventory-items type :base base
                                   :subtag-count subtag-count
                                   :q q
                                   :distance (user-rdist))
    (let* ((tags (sort (if base
                         nearby
                         (remove-if-not #'top-tag-p nearby :key #'first))
                       #'> :key #'second))
           (top-tags (subseq tags 0 (min count (length tags)))))
      (cond
        ((and more (> (length tags) (+ count 1)))
         (values
           (append (sort top-tags #'string< :key #'first)
                   (list
                     (let* ((more-tags (subseq tags count))
                            (subtags (iter (for tag in (subseq more-tags 0
                                                               (min 6 (length more-tags))))
                                           (collect
                                             (cons (first tag) (second tag))))))
                       (list "etc"
                         (reduce #'+ more-tags :key #'second)
                         (if (< 6 (length more-tags))
                           (append (sort (subseq subtags 0 5) #'string< :key #'car)
                                   (list
                                     (cons "more" (reduce #'+ (subseq more-tags 5) :key #'second))))
                           (sort subtags #'string< :key #'car))))))
                 items))
        ((and more (= (length tags) (+ count 1)))
         (values (sort tags #'string< :key #'first) items))
        (t
         (values (sort top-tags #'string< :key #'first) items))))))

(defun inventory-body-html (type &key base q items start page preposition)
  (let ((base-url (s+ "/" type "s")))
    (html
      (:div :class "activity"
        (when *user*
          (str
            (menu-horiz "actions"
              (html (:a :href (s+ "/people/" (username-or-id) "/" type "s") (str (s+ "show my " type "s")))))))
        (:div :class "item"
          (unless (or (not *user*)
                      (eq (getf *user* :active) nil)
                      base
                      q)
            (str (simple-inventory-entry-html preposition type)))

          (when q
            (htm
              (:span (:strong :class "small" (str (s+ "showing " type "s matching \"")) (str q) (:strong "\"")))))

          (str (rdist-selection-html (url-compose base-url "q" q "kw" base)
                                     :class "inline"
                                     :text (if q " within "
                                                 "showing results within ")))
          (when (or base q)
            (htm
              (:p (:a :href base-url (str (s+"show all " type "s")))))))

        (iter (for i from 0 to (+ start 20))
              (cond
                ((< i start)
                 (pop items))

                ((and (>= i start) items)
                 (str (inventory-activity-item (pop items)
                                               :show-distance t
                                               :truncate t
                                               :show-tags t)))
                (t
                 (when (< (user-rdist) 100)
                   (htm
                     (:div :class "item small"
                      (:em "Increasing the ")(:strong "show results within")(:em " distance may yield more results."))))
                 (finish)))

              (finally
                (when (or (> page 0) (cdr items))
                  (htm
                    (:div :class "item"
                     (when (> page 0)
                       (htm
                         (:a :href (url-compose base-url "p" (- page 1) "kw" base) "< previous page")))
                     "&nbsp;"
                     (when (cdr items)
                       (htm
                         (:a :style "float: right;"
                             :href (url-compose base-url "p" (+ page 1) "kw" base) 
                             "next page >"))))))))))))

(defun browse-inventory-tags (type &key q base tags)
  (let ((base-url (s+ "/" type "s")))
    (html
      (when base
        (htm
          (:p (:strong "categories selected: "))
          (:ul :class "keywords"
            (dolist (tag base)
              (htm
                (:li
                  (:a :href (url-compose base-url "kw" tag "q" q) (str tag))
                  " "
                  (:a :href (url-compose base-url "kw" (remove tag base :test #'string=) "q" q)
                      "[x]")
                  ))))))
      (:h3 "filter by category")
      (dolist (tag tags)
        (if (string= (first tag) "etc")
          (htm
            (:div :class "category"
             (:h3 (:a :href (s+ base-url "/all")
                      (str (s+ "etc (" (write-to-string (second tag)) ")"))))
             (iter (for subtag in (third tag))
                   (for i downfrom (length (third tag)))
                   (htm
                     (:a :href (if (string= (first subtag) "more")
                                 (s+ base-url "/all")
                                 (url-compose "" "kw" (format nil "~{~a+~}~a" base (first subtag)) "q" q) )
                         (str (s+ (car subtag) " (" (write-to-string (cdr subtag)) ")")))
                     (unless (= i 1)
                       (str ", "))))))
          (htm
            (:div :class "category"
             (:h3 (:a :href (url-compose "" "kw" (format nil "~{~a+~}~a" base (first tag)) "q" q)
                      (str (s+ (first tag) " (" (write-to-string (second tag)) ")"))))
             (iter (for subtag in (third tag))
                   (for i downfrom (length (third tag)))
                   (htm
                     (:a :href (url-compose "" "kw"
                                            (if (string= (first subtag) "more")
                                              (format nil "~{~a+~}~a" base (first tag))
                                              (format nil "~{~a+~}~a+~a" base (first tag) (first subtag)))
                                            "q" q)
                         (str (s+ (car subtag) " (" (write-to-string (cdr subtag)) ")")))
                     (unless (= i 1)
                       (str ", "))))))))
      (unless base
        (htm
          (:div :class "category"
           (:h3 (:a :href (s+ base-url "/all") "show all keywords"))))))))

(defun browse-all-inventory-tags (preposition type &key base tags)
  (html
    (unless base
      (str (simple-inventory-entry-html preposition type)))
    (let ((base-url (s+ "/" type "s")))
      (htm
        (:div :class "item"
        (:h2 "browse by keyword")
        (when base
          (htm
            (:p (:a :href base-url (str (s+ "show all " type "s"))))
            (:p (:strong "keywords selected: "))
            (:ul :class "keywords"
              (dolist (tag base)
                (htm
                  (:li
                    (:a :href (format nil (s+ base-url "?kw=~{~a~^+~}") (remove tag base :test #'string=))
                        "[x]")
                    " "
                    (:a :href (format nil (s+ base-url "?kw=~a") tag) (str tag))
                    ))))))
        (dolist (tag tags)
          (htm
            (:div :class "category"
             (:h3 (:a :href (format nil (s+ base-url "?kw=~{~a+~}~a") base (first tag))
                      (str (s+ (first tag) " (" (write-to-string (second tag)) ")"))))
             (iter (for subtag in (third tag))
                   (for i downfrom (length (third tag)))
                   (if (string= (car subtag) "more")
                     (htm
                       (:a :href (format nil (s+ base-url "?kw=~{~a+~}~a") base (first tag)) (str (strcat "more (" (second tag) ")"))))
                     (htm
                     (:a :href (format nil (s+ base-url "?kw=~{~a+~}~a+~a") base (first tag) (first subtag))
                         (str (s+ (car subtag) " (" (write-to-string (cdr subtag)) ")")))
                     (unless (= i 1)
                       (str ", ")))))))))))))

(defun inventory-item-reply (type id data &key next)
  (let ((next (or next (get-parameter "next"))))
    (if (item-view-denied (result-privacy (gethash id *db-results*)))
      (permission-denied)
      (standard-page
        "Reply"
        (html
          (:h2 "Reply to "
               (str (person-link (getf data :by) :possesive t))
               (str type))
          (:p (str (getf data :text)))
          (:h4 "Write your reply:")
          (:div :class "item"
            (:form :method "post" :action (strcat "/" type "s/" id)
              (:input :type "hidden" :name "next" :value next)
              (:table :class "post"
               (:tr
                 (:td (:textarea :cols "1000" :rows "4" :name "reply-text"))
                 (:td
                   (:button :class "yes" :type "submit" :class "submit" "Reply")))))))
        :selected (s+ type "s")))))
