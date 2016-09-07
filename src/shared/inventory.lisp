;;; Copyright 2012-2016 CommonGoods Network, Inc.
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

(defun create-inventory-item (&key type (by *userid*) title details tags privacy expires)
  (insert-db (list :type type
                   :active t
                   :by by
                   :privacy privacy ;a list of groups who can see the item
                   :title title
                   :details details
                   :tags tags
                   :expires expires
                   :created (get-universal-time))))

(defun inventory-item-result (id &key data by-id by)
  (or (gethash id *db-results*)
      (let* ((item (or data (db id)))
             (by-id (or by-id (getf data :by)))
             (by (or by (db by-id))))
        (when item
          (make-result :latitude (or (getf item :lat) (getf by :lat))
                       :longitude (or (getf item :long) (getf by :long))
                       :id id
                       :type (getf item :type)
                       :people (list by-id)
                       :privacy (getf item :privacy)
                       :time (apply #'max
                                    (remove nil
                                            (list (getf item :refreshed)
                                                  (getf item :edited)
                                                  (getf item :created))))
                       :tags (getf item :tags))))))

(defun index-inventory-item
  (id
   &optional (data (db id))
   &aux (by-id (getf data :by))
        (by (db by-id))
        (type (getf data :type))
        (pending (getf by :pending))
        (result (inventory-item-result id
                                       :data data
                                       :by-id by-id
                                       :by by))
        (locationp (and (result-latitude result)
                       (result-longitude result))))

  ;; other code (e.g. index transaction) requires results for inactive items
  (with-locked-hash-table (*db-results*)
    (setf (gethash id *db-results*) result))

  (cond
    (pending
     (with-locked-hash-table (*pending-person-items-index*)
       (let ((results (gethash by-id *pending-person-items-index*)))
         (cond
           ((or (not results)
                (and (eq type :offer)
                     (> (result-time result)
                        (result-time (car results)))))
            (pushnew result (gethash by-id *pending-person-items-index*)))
           ((not (find result (gethash by-id *pending-person-items-index*)))
            (push result (cdr (last (gethash by-id *pending-person-items-index*)))))))))

    ((getf data :active)
     (with-locked-hash-table (*profile-activity-index*)
       (asetf (gethash by-id *profile-activity-index*)
              (safe-sort (pushnew result it) #'> :key #'result-time)))

     (index-inventory-expiration id data)

     ;; when clause can be removed after May 12, 2016
     ;; this is in place to prevent a tital wave of automatic refreshes when
     ;; we launch the refresh functionality
     (when (> (result-time result)
              (- 3661624237 (* 30 +day-in-seconds+)))
       (index-inventory-refresh-time result))

     (if (eq type :offer)
       (with-locked-hash-table (*offer-index*)
         (pushnew id (gethash by-id *offer-index*)))
       (with-locked-hash-table (*request-index*)
         (pushnew id (gethash by-id *request-index*))))

     (unless (getf by :test-user)
       (when locationp
         (let ((title-stems (stem-text (getf data :title)))
               (details-stems (stem-text (getf data :details)))
               (tag-stems (stem-text (separate-with-spaces
                                       (getf data :tags)))))
           (if (eq type :offer)
             (with-locked-hash-table (*offer-stem-index*)
               (dolist (stem title-stems)
                 (pushnew result
                          (getf (gethash stem *offer-stem-index*) :title)))
               (dolist (stem details-stems)
                 (pushnew result
                          (getf (gethash stem *offer-stem-index*) :details)))
               (dolist (stem tag-stems)
                 (pushnew result (getf (gethash stem *offer-stem-index*) :tags))))

             (with-locked-hash-table (*request-stem-index*)
               (dolist (stem title-stems)
                 (pushnew result (getf (gethash stem *request-stem-index*) :title)))
               (dolist (stem details-stems)
                 (pushnew result (getf (gethash stem *request-stem-index*) :details)))
               (dolist (stem tag-stems)
                 (pushnew result (getf (gethash stem *request-stem-index*) :tags))))))
         (if (eq type :offer)
           (geo-index-insert *offer-geo-index* result)
           (geo-index-insert *request-geo-index* result))

         ;; unless item is older than 180 days
         (unless (< (result-time result) (- (get-universal-time) 15552000))
           ;; unless item is older than 30 days
           (unless (< (result-time result) (- (get-universal-time) 2592000))
             (with-mutex (*recent-activity-mutex*)
               (pushnew result *recent-activity-index*)))
           (geo-index-insert *activity-geo-index* result))))

     (when (eq type :request)
       (if (or (getf data :match-all-terms)
               (getf data :match-any-terms))
         (index-matchmaker id data)
         (with-mutex (*requests-without-matchmakers-mutex*)
           (safe-sort (pushnew result *requests-without-matchmakers-index*)
                      #'>
                      :key #'result-time)))))

    ((not (getf data :violates-terms))
     (if (eq type :offer)
       (with-locked-hash-table (*account-inactive-offer-index*)
          (asetf (gethash by-id *account-inactive-offer-index*)
                 (safe-sort (pushnew result it) #'> :key #'result-time)))
       (with-locked-hash-table (*account-inactive-request-index*)
          (asetf (gethash by-id *account-inactive-request-index*)
                 (safe-sort (pushnew result it) #'> :key #'result-time)))))))

(defun modify-inventory-item (id &key publish-facebook-p title details tags privacy expires)
  (let* ((result (gethash id *db-results*))
         (type (result-type result))
         (data (db id))
         (reactivate (not (getf data :active)))
         (fb-action-id (first (fb-object-actions-by-user id :data data)))
         (by (getf data :by))
         (now (get-universal-time)))

    (flet
      ((reindex-stems (new-text old-text data-type)
         (let* ((oldstems (stem-text old-text))
                (newstems (stem-text new-text))
                (common (intersection oldstems newstems :test #'string=)))

           (flet ((commonp (stem)
                    (member stem common :test #'string=)))

             (setf oldstems (delete-if #'commonp oldstems))
             (setf newstems (delete-if #'commonp newstems))

             (when (eq type :offer)
               (with-locked-hash-table (*offer-stem-index*)
                 (dolist (stem oldstems)
                   (asetf (getf (gethash stem *offer-stem-index*) data-type)
                          (remove result it))))
                 (dolist (stem newstems)
                   (push result (getf (gethash stem *offer-stem-index*)
                                      data-type))))

             (when (eq type :request)
               (with-locked-hash-table (*request-stem-index*)
                 (dolist (stem oldstems)
                   (asetf (getf (gethash stem *request-stem-index*) data-type)
                          (remove result it))))
                 (dolist (stem newstems)
                   (push result (getf (gethash stem *request-stem-index*)
                                      data-type))))))))

      (reindex-stems title (getf data :title) :title)
      (reindex-stems details (getf data :details) :details)
      (reindex-stems (separate-with-spaces tags)
                     (separate-with-spaces (getf data :tags))
                     :tags))

    (unless (equal tags (getf data :tags))
      (setf (result-tags result) tags))

    (setf (result-privacy result) privacy)

    (let ((new-data (list :title title
                          :details details
                          :active t
                          :tags tags
                          :expires expires
                          :privacy privacy)))

      (cond
        ((and publish-facebook-p (not fb-action-id))
         (if (current-fb-token-p)
           (progn (notice :new-facebook-action :item-id id)
                  (flash (s+ "Your "
                             (string-downcase (symbol-name type))
                             " has been published on Facebook")))
           (renew-fb-token :item-to-publish id
                           :next (resource-url id data))))

        ((and fb-action-id publish-facebook-p)
         (notice :new-facebook-action :object-modified t
                                      :userid nil
                                      :fb-object-id (getf data :fb-object-id)))

        ((and (not publish-facebook-p) fb-action-id)
         (delete-facebook-action fb-action-id)))

      (unless (and (getf *user* :admin)
                   (not (group-admin-p by))
                   (not (eql *userid* by)))
        (refresh-item-time-in-indexes id :time now)
        (append new-data (list :edited now)))

      (apply #'modify-db id new-data)
      (if reactivate
        (index-inventory-item id)
        (progn
          (deindex-inventory-expiration id)
          (index-inventory-expiration id))))

    (case (result-type result)
      (:offer (update-matchmaker-offer-data id))
      (:request (when (or (getf data :match-all-terms)
                          (getf data :match-any-terms))
                  (modify-matchmaker id)
                  (update-matchmaker-request-data id))))))

(defun deactivate-inventory-item (id &key violates-terms)
  (let* ((result (gethash id *db-results*))
         (type (result-type result))
         (data (db id))
         (fb-actions (getf data :fb-actions))
         (now (get-universal-time))
         (by-id (getf data :by))
         (by-group-p (eq (db by-id :type) :group))
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
         (stems (stem-text (s+ (getf data :title) " " (getf data :details)))))

    (when result
      (with-locked-hash-table (stem-index)
        (dolist (stem stems)
          (asetf (gethash stem stem-index)
                 (remove result it))))

      ;; delete matchmakers
      (case type
        (:offer
          (unmatch-offer-matches id
                                 by-id
                                 (copy-list
                                   (gethash id
                                           *offers-with-matching-requests-index*)))
          (with-locked-hash-table (*offers-with-matching-requests-index*)
            (remhash id *offers-with-matching-requests-index*)))

        (:request
          (when (or (getf data :match-all-terms)
                    (getf data :match-any-terms))
            (unmatch-request-matches id
                                   by-id
                                   (append (getf data :matching-offers)
                                           (getf data :hidden-matching-offers)))
            (remove-matchmaker-from-indexes id))))

      (geo-index-remove geo-index result)

      (if (eq type :event)
        (with-mutex (*event-mutex*)
          (asetf *event-index* (remove result it)))
        (with-locked-hash-table (type-index)
          (asetf (gethash by-id type-index)
                 (remove id it))))
      (case type
        (:request
          (with-locked-hash-table (*account-inactive-request-index*)
            (asetf (gethash by-id *account-inactive-request-index*)
                   (safe-sort (push result it) #'> :key #'result-time)) ))
        (:offer
          (with-locked-hash-table (*account-inactive-offer-index*)
             (asetf (gethash by-id *account-inactive-offer-index*)
                    (safe-sort (push result it) #'> :key #'result-time)))))

      (deindex-inventory-expiration id data)
      (deindex-inventory-refresh-time result)

      (with-mutex (*inventory-expiration-timer-mutex*)
        (asetf *inventory-expiration-timer-index*
               (remove (rassoc id it)
                       it
                       :test #'equal)))

      (dolist (transaction-id (gethash id *inventory-transactions-index*))
        (modify-transaction-log transaction-id
                                :deactivated
                                :party (cons *userid* (when by-group-p by-id))))

      (unless (eq type :event)
        (with-locked-hash-table (*profile-activity-index*)
          (asetf (gethash by-id *profile-activity-index*)
                 (remove result it)))
        (geo-index-remove *activity-geo-index* result)
        (with-mutex (*recent-activity-mutex*)
          (asetf *recent-activity-index* (remove id it :key #'result-id)))))

    (when fb-actions
      (dolist (action fb-actions)
        (delete-facebook-action (getf action :fb-action-id))))

    (modify-db id :active nil
                  :deleted-fb-actions fb-actions
                  :fb-actions nil
                  :deactivated now
                  :violates-terms violates-terms)))

(defun delete-pending-inventory-item (id)
  (let ((data (db id))
        (result (gethash id *db-results*)))
    (with-locked-hash-table (*pending-person-items-index*)
      (asetf (gethash (getf data :by) *pending-person-items-index*)
             (remove result it)))
    (remove-from-db id)))

(defun deleted-invalid-item-reply-text (to-name from-name type &optional explanation)
  (strcat* "Greetings " to-name ","
        #\linefeed
        #\linefeed
        "Your " type
        " violated Kindista's Terms of Use and we had to remove it. "
        "Please list multiple offers and requests separately (not in the same item). "
        "Kindista is for giving and receiving freely; please avoid any language which implies that you are expecting barter or money in exchange for your offer or request. "
        #\linefeed
        (awhen explanation (strcat #\linefeed it #\linefeed))
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

(defun post-new-inventory-item (type &key url)
  (require-user (:allow-test-user t :require-active-user t)
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
           (expiration-options (first (multiple-value-list
                                        (expiration-options))))
           (expiration-descriptor (or (post-parameter-string "expiration")
                                      "3-months"))
           (expiration-time (cdr (assoc expiration-descriptor
                                        expiration-options
                                        :test #'string=)))
           (publish-facebook (post-parameter "publish-facebook"))
           (adminp (group-admin-p (or groupid identity-selection)))
           (title (post-parameter-string "title"))
           (details (when (scan +text-scanner+ (post-parameter "details"))
                      (post-parameter "details"))))

      (iter (for tag in (tags-from-string (post-parameter "tags")))
            (setf tags (cons tag tags)))

      (flet ((inventory-details (&key error)
               (enter-inventory-item-details
                 :page-title (s+ "Post a new " type)
                 :item-title title
                 :details details
                 :next (post-parameter "next")
                 :action url
                 :publish-facebook publish-facebook
                 :restrictedp restrictedp
                 :identity-selection identity-selection
                 :groupid groupid
                 :groups-selected groups-selected
                 :tags tags
                 :expiration expiration-descriptor
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

         ((not title)
          (flash (s+ "Please enter a better title for your " type ".")
                 :error t)
          (inventory-details))

         ((> (length title) 140)
          (flash (s+ "Please shorten your title to 140 characters or less."))
          (inventory-details))

         ((> (length details) 1000)
          (flash (s+ "Please shorten your description. Offers and Requests must be no longer than 1000 characters including line breaks."))
          (inventory-details))

         ((and (not (post-parameter "create"))
               title)
           (inventory-details))

         ((and restrictedp
               (post-parameter "create")
               (not groups-selected))
          (inventory-details :error (s+ "Please allow at least one group to see this " type)))

         ((not (intersection tags *top-tags* :test #'string=))
           (inventory-details :error "You must select at least one category"))

         ((> (length (intersection tags *top-tags* :test #'string=))
             5)
           (inventory-details :error "You entered too many categories. Please choose only the most relevant ones."))

         ((> (length (set-difference tags *top-tags* :test #'string=))
             10)
           (inventory-details :error (s+ "You entered too many keywords. Please choose only the most relevant ones (up to 10). If you are trying to post multiple items at once, please create separate " type "s for each one.")))

         ((and (post-parameter "create") title)
          (let* ((new-id (create-inventory-item
                           :type (if (string= type "request") :request
                                                              :offer)
                           :by (if adminp
                                 (or groupid identity-selection)
                                 *userid*)
                           :privacy groups-selected
                           :expires expiration-time
                           :title title
                           :details details
                           :tags tags))
                 (new-url (strcat "/" type "s/" new-id)))

             (send-metric* (if (string= type "request")
                             :new-request
                             :new-offer)
                           new-id)

            (if (getf *user* :pending)
              (progn
                new-id
                (contact-opt-out-flash (list *userid*) :item-type type)
                (when (string= type "offer")
                  (notice :new-pending-offer :id new-id))
                (if (string= type "request")
                  (flash "Your item has been recorded. It will be posted after you post an offer and we have a chance to review it. In the meantime, please consider posting additional offers, requests, or statements of gratitude. Thank you for your patience.")
                  (flash "Your item has been recorded. It will be posted after we have a chance to review it. In the meantime, please consider posting additional offers, requests, or statements of gratitude. Thank you for your patience."))
                (see-other "/home"))
              (progn
                (contact-opt-out-flash (list *userid*) :item-type type)
                (flash
                  (s+ "Congratulations, your "
                      type
                      " has been posted! "
                      " You will "
                      (if (getf *user* :notify-message)
                        " will be notified by email "
                        " will receive a notification on Kindista ")
                      (s+ "when someone wants to "
                          (if (string= type "offer")
                            "receive this from you. "
                            "wants to share this with you. "))
                      " As a reminder, you are under no obligation to "
                      (if (string= type "offer")
                        "share with anyone"
                        "accept gifts from anyone")
                      "; you decide who you want to share with on Kindista."))
                (when (string= type "offer")
                  (update-matchmaker-offer-data new-id))
                (cond
                  ((or (not (getf *user* :fb-id))
                       (not publish-facebook))
                   (see-other new-url))
                  ((current-fb-token-p)
                   (notice :new-facebook-action :item-id new-id)
                   (flash (s+ "Your "
                              type
                              " has been published on Facebook"))
                   (see-other new-url))
                  (t (renew-fb-token :item-to-publish new-id
                                     :next new-url)))))))

         (t (inventory-details)))))))

(defun deactivated-item-error
  (type
   &optional (next (or (post-parameter "next") "/home")))
  (flash (s+ "Sorry, this " type " has been deactivated.") :error t)
  (see-other next))

(defun register-inventory-item-action
  (id
   action-type
   &key (item (db id))
        next
        (url (resource-url id item))
        reply-text
        reply
   &aux (type (getf item :type))
        (by (getf item :by))
        (groupid (awhen (post-parameter-integer "identity-selection")
                              (unless (= it *userid*) it)))
        (adminp (group-admin-p by)))

  (require-user (:require-email t)
    (cond
      ((and (or (and (eq type :offer)
                     (eq action-type :offered))
                (and (eq type :request)
                     (eq action-type :requested)))
            (nor (eql *userid* by)
                 adminp))
       ;; in case of wrong action-type post request. yes it has happened.
       (notice :error :on "User is trying to 'request' a :request or 'offer' an :offer"
                      :data (cons id item))
       (flash "The system encountered a problem. Please go to the item's page and try offering/requesting it again.  If the problem persists, please report it in Kindista's Help/Feedback section." :error t)
       (see-other (or next url)))

      (reply
       (flet ((reply-html (type)
                (inventory-item-reply
                  type
                  id
                  item
                  :text reply-text
                  :action-type action-type
                  :next next
                  :identity-selection groupid
                  :match (post-parameter-integer "match")
                  :error (when (and (not (post-parameter "reply"))
                                    (not reply-text)
                                    (not action-type))
                           :no-message))))
         (case (getf item :type)
           (:offer (reply-html "offer"))
           (:request (reply-html "request"))
           (t (not-found)))))

      ((or reply-text
           (and (eql type :request)
                (string= action-type "offer"))
           (and (eql type :offer)
                (string= action-type "request")))
         (aif (find-existing-transaction id)
           (post-transaction it)
           (progn
             (flash (s+ "Your "
                        (or action-type "reply")
                        " has been sent."))
             (contact-opt-out-flash (list by (unless (eql *userid* by)
                                               *userid*)))
             (create-transaction :on id
                                 :text reply-text
                                 :groupid groupid
                                 :action (cond
                                           ((string= action-type "offer")
                                            :offered)
                                           ((string= action-type "request")
                                            :requested))
                                 :match-id (post-parameter-integer "match"))
             (see-other (or next (strcat "/transactions/" id)))))))))

(defun post-existing-inventory-item (type &key id url edit deactivate)
  (require-user (:allow-test-user t :require-email t)
    (let* ((id (safe-parse-integer id))
           (item (db id))
           (by (getf item :by))
           (publish-facebook (post-parameter "publish-facebook"))
           (action-type (post-parameter-string "action-type"))
           (adminp (group-admin-p by))
           (next (post-parameter "next")))

      (cond
        ((and (or (post-parameter "reply-text")
                  (post-parameter "reply"))
              (getf *user* :pending))
         (pending-flash "contact other Kindista members")
         (see-other (or (referer) "/home")))

        ((or action-type
             (post-parameter "reply")
             (post-parameter-string "reply-text"))
         (register-inventory-item-action id
                                         action-type
                                         :item item
                                         :next next
                                         :reply (post-parameter "reply")
                                         :reply-text (post-parameter-string
                                                       "reply-text")))

        ((nor (getf item :active)
              (eql by *userid*)
              (getf *user* :admin))
         (deactivated-item-error (string-downcase (getf item :type))))

        ((and (not (eql by *userid*))
              (getf *user* :test-user))
         (test-users-prohibited))

        ((and (not (getf *user* :test-user))
              (db by :test-user))
         (disregard-test-data))

        ((and (not (eql by *userid*))
              (item-view-denied (result-privacy (gethash id *db-results*))))
         (permission-denied))

        ((post-parameter "cancel")
         (see-other (or next "/home")))

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
                  (expiration-options (multiple-value-list
                                        (expiration-options (getf item :expires))))
                  (expiration-descriptor
                    (or (post-parameter-string "expiration")
                        (car (last expiration-options))
                        "3-months"))
                  (expiration-time (cdr (assoc expiration-descriptor
                                               (first expiration-options)
                                               :test #'string=)))
                  (new-title (post-parameter-string "title"))
                  (title (or new-title (getf item :title)))
                  (new-details (post-parameter-string "details"))
                  (details (or new-details (getf item :details))))

             (iter (for tag in (tags-from-string (post-parameter "tags")))
                   (setf tags (cons tag tags)))

             (flet ((inventory-details
                      (&key error
                            (publish-facebook-p publish-facebook))
                      (enter-inventory-item-details
                        :page-title (s+ "Edit your " type)
                        :action url
                        :item-title title
                        :details details
                        :tags (or tags (getf item :tags))
                        :groups-selected groups-selected
                        :publish-facebook publish-facebook-p
                        :restrictedp restrictedp
                        :next (or (post-parameter "next") (referer))
                        :existingp t
                        :groupid (when adminp by)
                        :expiration expiration-descriptor
                        :error error
                        :button-text (s+ "Save " type)
                        :selected (s+ type "s"))))

               (cond
                ((or (post-parameter "edit") edit)
                 (inventory-details
                   :publish-facebook-p (when (fb-object-actions-by-user id :data item)
                                         t)))

                ((or deactivate (post-parameter "deactivate"))
                 (confirm-delete :url url
                                 :type type
                                 :confirmation-question
                                   (s+ "Are you sure you want to deactivate this " type "?")
                                 :text (or (getf item :title)
                                           (getf item :details))
                                 :next-url (if (string= (referer) (strcat "/" type "s/" id))
                                             "/home"
                                             (referer))))

                ((post-parameter "reactivate")
                  (see-other (s+ (url-compose (resource-url id)
                                              "edit" "t"
                                              "focus" "expiration")
                                 "#expiration")))

                ((and (post-parameter "really-delete")
                      (not (post-parameter "delete-inappropriate-item")))
                 (deactivate-inventory-item id)
                 (flash (s+ "Your " type " has been deactivated!"))
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
                                   :confirmation-question
                                     (s+ "Are you sure you want to deactivate this " type "?")
                                   :type type
                                   :text (or (getf item :title)
                                             (getf item :details))
                                   :item-id id
                                   :inappropriate-item t)))

                ((post-parameter "delete-inappropriate-item")
                 (require-admin
                   (create-transaction :on id
                                       :pending-deletion t
                                       :text (post-parameter "explanation"))
                   (deactivate-inventory-item id :violates-terms t)
                   (flash (strcat (string-capitalize type) " " id " has been deactivated."))
                   (see-other (if (string= (post-parameter "next")
                                           (strcat "/" type "s/" id))
                                "/home"
                                (post-parameter "next")))))

                ((and publish-facebook
                      (not (post-parameter "create"))
                      (not (fb-object-actions-by-user id)))
                 (if (current-fb-token-p)
                   (progn (notice :new-facebook-action :item-id id)
                          (flash (s+ "Your "
                                     type
                                     " has been published on Facebook"))
                          (see-other next))
                   (renew-fb-token :item-to-publish id
                                   :next next)))

                ((not title)
                 (flash (s+ "Please enter a title for your " type "."))
                 (inventory-details))

                ((and (post-parameter "title") (not new-title))
                 (flash (s+ "Please enter a better title for your " type ".")
                        :error t)
                 (inventory-details))

                ((> (length title) 140)
                 (flash (s+ "Please shorten your title to 140 characters or less."))
                 (inventory-details))

                ((> (length details) 1000)
                 (flash (s+ "Please shorten your description. Offers and Requests must be no longer than 1000 characters including line breaks."))
                 (inventory-details))

                ((not (intersection tags *top-tags* :test #'string=))
                  (inventory-details :error "You must select at least one category"))

                ((> (length (intersection tags *top-tags* :test #'string=))
                    5)
                  (inventory-details :error "You entered too many categories. Please choose only the most relevant ones."))

                ((> (length (set-difference tags *top-tags* :test #'string=))
                    10)
                  (inventory-details :error (s+ "You entered too many keywords. Please choose only the most relevant ones (up to 10). If you are trying to post multiple items at once, please create separate " type "s for each one.")))

                ((post-parameter "create")
                 (require-test ((not (getf item :violates-terms))
                                "This item violated Kindista's Terms of Use. It has been deactivated and cannot be modified.")
                   (modify-inventory-item id :title (post-parameter "title")
                                             :details (post-parameter "details")
                                             :tags tags
                                             :expires expiration-time
                                             :publish-facebook-p publish-facebook
                                             :privacy (when restrictedp
                                                      groups-selected)))
                 ;; new fb actions are redirected via
                 ;; modify-inventory-item to ensure current fb-token
                 (when (or (not publish-facebook)
                           (fb-object-actions-by-user id))
                   (see-other (strcat "/" type "s/" id))))

                (t
                  (inventory-details)))))))))))

(defun simple-inventory-entry-html (preposition type &key groupid)
  (html
    (:div :class "item simple-inventory-entry"
      (:form :method "post" :action (s+ "/" type "s/new")
        (:input :type "hidden"
                :name "next"
                :value (script-name*))
        (awhen groupid
          (htm (:input :type "hidden" :name "groupid" :value it)))

        (:label :for "title" :class "title"
          "Post " (str preposition) " " (str type) " ")
          (:span :class "inline-block"
            " ("
           (:span :id "title-count" "64")
           " characters available)")
        (:table :class "inventory-title"
          (:tr
            (:td :class "inventory-details"
              (:input :type "text"
                      :id "title"
                      :name "title"
                      :onkeyup (ps-inline
                                 (limit-characters this 64 "title-count"))))
            (:td :id "inventory-title-button" (:button :class "yes" :type "submit" :name "post" "Post")))))
      (:p :class "help-text"
        "Enter a "
        (:strong "short title for a single " (str type))
        ". You can add details after you click \"post.\""))))


(defun enter-inventory-item-details
  (&key page-title action item-title details existingp groupid identity-selection restrictedp error tags button-text selected groups-selected next publish-facebook expiration
   &aux (typestring (if (string= selected "offers") "offer" "request"))
        (suggested (or tags (get-tag-suggestions item-title))))

  (standard-page page-title
    (html
      (:div :class "item inventory-details" :id "enter-inventory-details"
       (str (pending-disclaimer))
       (when error
         (htm
           (:p :class "error" (str error))))
       (:h2 (str page-title) )
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


         (:div :class "help-text"
           (:strong (:span :id "title-count"
                     (str (aif item-title
                            (strcat (- 64 (length it)))
                            "64"))))
           " characters available")
         (:label :for "title"  "Enter a title ")
         (:input :type "text"
                 :id "title"
                 :name "title"
                 :onkeyup (ps-inline
                            (limit-characters this 64 "title-count"))
                 :value (awhen item-title
                          (str (escape-for-html it))))

         (:div :class "help-text"
           (:strong
             (:span :id "details-count" (str (aif details
                                               (strcat (- 1000 (length it)))
                                               "1000"))))
             " characters available")
         (:label :for "details" "Include a description (optional)")
         (:textarea :class "review-text"
                    :name "details"
                    :id "details"
                    :rows "5"
                    :onkeyup (ps-inline
                               (limit-characters this 1000 "details-count"))
                    (awhen details
                      (str (escape-for-html it))))

         (unless (or groupid existingp)
           (awhen (groups-with-user-as-admin)
             (htm
               (:label :for "identity-selection"
                 (str (s+ (string-capitalize typestring) "ed by")))
               (str (identity-selection-html identity-selection it :onchange "this.form.submit()")))))

         (when (or (getf *user-group-privileges* :member)
                   (getf *user-group-privileges* :admin)
                   groups-selected)
           (str (privacy-selection-html
                  typestring
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

         (when (and (getf *user* :fb-token)
                    (not restrictedp))
           (htm
             (:div :id "facebook"
               (:input :type "checkbox"
                       :id "publish-facebook"
                       :name "publish-facebook"
                       :checked (when (or (not existingp)
                                          publish-facebook)
                           ""))
               (str (icon "facebook" "facebook-icon"))
               (:label :for "publish-facebook"
                 (str (s+ "Publish this "
                        typestring
                        " on my Facebook timeline."))))))


         (:fieldset
           (:legend "Select 1-5 categories")
           (:p :class "small"
             "Please note: selecting irrelevant categories is considered spam.")
           (:div :id "categories"
             (dolist (tag *top-tags*)
               (htm
                 (:div :class "category"
                   (:input :type "checkbox"
                    :name "tag"
                    :value tag
                    :id tag
                    :checked (when (member tag suggested :test #'string=)
                               (setf suggested (remove tag suggested :test #'string=))
                               ""))
                  (:label :for tag (str tag))
                )))))
         (:label :for "tags" "Additional keywords (optional)")
         (:p :class "small"
          "Please note: adding irrelevant keywords is considered spam.")
         (:input :type "text" :id "tags" :name "tags" :size 40
                 :placeholder "e.g. produce, bicycle, tai-chi"
                 :value (format nil "~{~a~^,~^ ~}" suggested))

         (:label :for "expiration-selection"
                 :class (when (string= (get-parameter-string "focus")
                                       "expiration")
                          "red")
          "Expires in")
         (str (expiration-selection-html expiration))

         (:p
           (:strong :class "red" "Important: ")
           "You must read the "
           (:a :href "/faq#sharing-guidelines" 
               :target "_blank"
               "Guidelines for Sharing on Kindista")
           " before posting any offers or requests. "
           "Failure to adhere to these guidelines may result in account suspension.")

         (:p (:button :class "cancel" :type "submit" :class "cancel" :name "cancel" "Cancel")
             (:button :class "yes" :type "submit" :class "submit" :name "create" (str (or button-text "Post")))))))
   :right (sharing-guide-sidebar)
   :selected selected))

; author
; creation date
; edited date
; text
; tags (at least 1)
; privacy ('all 'contacts or listname)

(defun nearby-inventory-items (type &key base (subtag-count 4) (distance 50) q)
  (with-location
    (let* ((stem-results (when q
                           (inventory-stem-index-query
                             (case type
                               (:offer *offer-stem-index*)
                               (t *request-stem-index*))
                             q)))
           (nearby (geo-index-query (case type
                                        (:offer *offer-geo-index*)
                                        (t *request-geo-index*))
                                        *latitude*
                                        *longitude*
                                      distance))
           (nearby-stem-matches (when q
                                  (remove-if-not #'(lambda (stem-result)
                                                     (find stem-result nearby))
                                                 stem-results
                                                 :key #'car)))
           (nearby-matches (if q
                             (mapcar #'car nearby-stem-matches)
                             nearby))
           (items nil))

      (let ((tags (make-hash-table :test 'equalp)))
        (dolist (item nearby-matches)
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

          (setf items nearby-matches))

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
                (inventory-rank (if q
                                  (remove-if-not
                                    #'(lambda (result)
                                        (find result items))
                                    nearby-stem-matches
                                    :key #'car)
                                  (mapcar #'list items)))

               ;(safe-sort items #'> :key #'inventory-rank)
               )))))

(defun nearby-inventory-top-tags (type &key (count 9) (more t) base (subtag-count 4) q)
  (multiple-value-bind (nearby items)
      (nearby-inventory-items type :base base
                                   :subtag-count subtag-count
                                   :q q
                                   :distance (user-rdist))
    (let* ((tags (safe-sort (if base
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

(defun inventory-body-html (preposition type &key base q items start page)
  (let ((base-url (s+ "/" type "s")))
    (html
      (:div :class "activity"
        (when *user*
          (str
            (menu-horiz
              (html (:a :href (s+ "/people/" (username-or-id) "/" type "s") (str (s+ "show my " type "s")))))))

        (unless (or (not *user*)
                    (eq (getf *user* :active) nil)
                    base
                    q)
          (str (simple-inventory-entry-html preposition type)))

        (:div :class "item"
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
                         (:a :href (url-compose base-url
                                                "p" (- page 1)
                                                "kw" base
                                                "q" q)
                          "< previous page"
                          )))
                     "&nbsp;"
                     (when (cdr items)
                       (htm
                         (:a :style "float: right;"
                             :href (url-compose base-url
                                                "p" (+ page 1)
                                                "q" q
                                                "kw" base)
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

(defun inventory-item-reply
  (type
   id
   data
   &key action-type
        next
        match
        text
        error
        (identity-selection *userid*)
   &aux (next (or next (get-parameter "next")))
        (url (strcat "/" type "s/" id)))
  (if (item-view-denied (result-privacy (gethash id *db-results*)))
    (permission-denied)
    (standard-page
      "Reply"
      (html
        (when error
          (flash "Please enter a message to send." :error t))
        (htm
          (:h2
            (str (s+ (if action-type "Respond" "Reply")
                     " to "
                     (person-link (getf data :by) :possessive t)
                     type
                     ":"))))
        (:blockquote
          (:p
            (awhen (getf data :title)
              (htm (:strong (:a :href url (str (html-text it))))
                   (:br)))
            (awhen (getf data :details)
              (str (html-text it)))))
        (:div :class "reply item"
          (:form :method "post" :action url
            (:input :type "hidden" :name "next" :value next)
            (:input :type "hidden" :name "match" :value match)
            (:input :type "hidden" :name "action-type" :value action-type)
            (awhen (groups-with-user-as-admin)
              (htm
                (:label :for "identity-selection"
                  (str (if action-type
                         (s+ (string-capitalize action-type) "ed by:")
                         "Reply as:")))
                (str (identity-selection-html identity-selection it))))

            (:label :for "reply-text"
              (str (if action-type
                     "Include a message: (optional)"
                     "Write your reply:")))
            (:textarea :cols "1000" :rows "4" :id "reply-text" :name "reply-text" (str text))

            (:div
              (:button :type "submit" :class "cancel" :name "cancel" "Cancel")
              (:button :class "yes"
               :type "submit"
               :class "submit"
               (str (aif action-type
                      (s+ (string-capitalize it) " This")
                      "Reply")))))))

      :selected (s+ type "s")
      :class "inventory-reply")))
