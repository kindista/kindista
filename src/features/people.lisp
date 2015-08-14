;;; Copyright 2012-2015 CommonGoods Network, Inc.
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

(defun inactive-people ()
  "diagnostic tool for admins only. inefficient; should not be used in code."
  (set-difference
    (loop for id in (hash-table-keys *db-results*)
          when (eq (result-type (gethash id *db-results*)) :person)
          collect id)
    *active-people-index*
    :test #'equal))

(defun fix-unsubscribe-keys ()
  "To fix bad unsubscribe keys. Omitted , in older code mistakenly assigned unsubscribe keys to '(random-password 18)' instead of the result of calling that function."
  (dolist (id (hash-table-keys *db*))
    (let ((data (db id)))
      (when (and (or (eq (getf data :type) :person)
                     (eq (getf data :type) :deleted-person-account))
                 (not (equal (length (getf data :unsubscribe-key)) 18)))
        (modify-db id :unsubscribe-key (random-password 18))))))

(defun find-people-without-emails ()
  (let (ids)
    (dolist (id (hash-table-keys *db*))
      (let ((data (db id)))
        (when (eq (getf data :type) :person)
          (when (equal (getf data :emails) '(nil))
            (push id ids)
            ))))
    ids))

(defun reset-notification-settings (userid)
 (modify-db userid :notify-gratitude t
                   :notify-message t
                   :notify-reminders t
                   :notify-expired-invites t
                   :notify-blog t
                   :notify-inventory-digest t
                   :notify-kindista t))

(defun create-person (&key name email password host aliases pending)
  (let* ((person-id (insert-db (list :type :person
                                     :name name
                                     :aliases aliases
                                     :emails (list email)
                                     :host host
                                     :pending pending
                                     :active t
                                     :help t
                                     :pass (new-password password)
                                     :created (get-universal-time)
                                     :unsubscribe-key (random-password 18)
                                     :notify-gratitude t
                                     :notify-message t
                                     :notify-reminders t
                                     :notify-expired-invites t
                                     :notify-blog t
                                     :notify-inventory-digest t
                                     :notify-kindista t)))
         (person (db person-id)))

    ;; to monitor mystery bug that occasionally records these values incorrectly
    (unless (and (getf person :active)
                 (getf person :notify-gratitude)
                 (getf person :notify-message)
                 (getf person :notify-reminders)
                 (getf person :notify-expired-invites)
                 (getf person :notify-blog)
                 (getf person :notify-inventory-digest)
                 (getf person :notify-kindista))
      (notice :error
              :on "incorrect notification flags on new account creation"
              :userid person-id
              :url "/signup"
              :data person))

    person-id))

(defun index-person (id data)
  (let ((result (make-result :id id
                             :latitude (getf data :lat)
                             :longitude (getf data :long)
                             :type :person
                             :people (list id)
                             :time (getf data :created)))
        (names (cons (getf data :name)
                     (getf data :aliases))))

    (when (getf data :active)
      (with-mutex (*active-people-mutex*)
      (push id *active-people-index*)))

    (with-locked-hash-table (*db-results*)
      (setf (gethash id *db-results*) result))

    (when (and (getf data :emails)
               (not (getf data :banned)))
      (with-locked-hash-table (*email-index*)
        (dolist (email (getf data :emails))
          (when email ;some deleted accounts might have :emails (nil)
            (setf (gethash email *email-index*) id)))))

    (awhen (getf data :banned)
      (with-locked-hash-table (*banned-emails-index*)
        (dolist (email (getf data :emails))
          (setf (gethash email *banned-emails-index*) id))))

    (setf (gethash (getf data :username) *username-index*) id)

    (awhen (getf data :following)
      (with-locked-hash-table (*followers-index*)
        (dolist (person it)
          (push id (gethash person *followers-index*)))))

    (when (getf data :loves)
      (with-locked-hash-table (*love-index*)
        (dolist (item (getf data :loves))
          (unless (member id (gethash item *love-index*))
            (push id (gethash item *love-index*))))))

    (awhen (getf data :host)
      (with-locked-hash-table (*invited-index*)
        (pushnew id (gethash it *invited-index*))))

    (with-locked-hash-table (*profile-activity-index*)
      (asetf (gethash id *profile-activity-index*)
             (safe-sort (push result it) #'> :key #'result-time)))

    (unless (< (result-time result) (- (get-universal-time) 2592000))
      (with-mutex (*recent-activity-mutex*)
        (push result *recent-activity-index*)))

    (when (and (getf data :created)
               (getf data :active))
      (metaphone-index-insert names result)

      (when (and (getf data :lat)
                  (getf data :long)
                  (getf data :created)
                  (getf data :active))

         (geo-index-insert *people-geo-index* result)

         (unless (< (result-time result) (- (get-universal-time) 15552000))
           (geo-index-insert *activity-geo-index* result))))))

(defun reindex-person-location (id)
  ;when people change locations
  (let* ((result (gethash id *db-results*))
         (data (db id))
         (lat (getf data :lat))
         (long (getf data :long))
         (account-offers (gethash id *offer-index*))
         (account-requests (gethash id *request-index*))
         (matchmakers (loop for id in account-requests
                            when (gethash id *matchmaker-requests*)
                            collect id)))

    (unless result
      (notice :error :on "no db result on ungeoindex-person"))

    (geo-index-remove *people-geo-index* result)
    (geo-index-remove *activity-geo-index* result)

    (setf (result-latitude result) lat)
    (setf (result-longitude result) long)

    (when (and (getf data :created)
               (getf data :active))

      (metaphone-index-insert (cons (getf data :name) (getf data :aliases))
                              result)

      (when (and lat long)

        (geo-index-insert *people-geo-index* result)

        (unless (< (result-time result) (- (get-universal-time) 15552000))
          (geo-index-insert *activity-geo-index* result))

        (dolist (request-id account-requests)
          (let ((result (gethash request-id *db-results*)))
            (geo-index-remove *request-geo-index* result)
            (geo-index-remove *activity-geo-index* result)
            (setf (result-latitude result) lat)
            (setf (result-longitude result) long)
            (unless (< (result-time result) (- (get-universal-time) 15552000))
              (geo-index-insert *activity-geo-index* result))
            (geo-index-insert *request-geo-index* result)))

        (dolist (request-id matchmakers)
          ;; rematch all outstanding matchmaker requests
          (modify-matchmaker request-id)
          (update-matchmaker-request-data request-id))

        (dolist (offer-id account-offers)
          (let ((result (gethash offer-id *db-results*)))
            (geo-index-remove *offer-geo-index* result)
            (geo-index-remove *activity-geo-index* result)
            (setf (result-latitude result) lat)
            (setf (result-longitude result) long)
            (geo-index-insert *offer-geo-index* result)
            (update-matchmaker-offer-data offer-id)
            (unless (< (result-time result) (- (get-universal-time) 15552000))
              (geo-index-insert *activity-geo-index* result))))

        (dolist (offer-id account-offers)
          (update-matchmaker-offer-data offer-id))

        (dolist (gratitude-id (gethash id *gratitude-index*))
          (let ((result (gethash gratitude-id *db-results*)))
            (geo-index-remove *activity-geo-index* result)
            (setf (result-latitude result) lat)
            (setf (result-longitude result) long)
            (unless (< (result-time result) (- (get-universal-time) 15552000))
              (geo-index-insert *activity-geo-index* result))))))))

(defun reindex-person-names (id)
  (let* ((result (gethash id *db-results*))
         (data (db id))
         (names (cons (getf data :name)
                      (getf data :aliases))))
    (metaphone-index-insert names result)))

(defun merge-duplicate-accounts
  (most-active-id
   duplicate-account-id-list
   &aux (data-to-keep (copy-list (db most-active-id)))
        (id-to-keep (apply #'min duplicate-account-id-list))
        (ids-to-deactivate (remove id-to-keep duplicate-account-id-list))
        (oldest-data (db id-to-keep)))

  "ALWAYS TEST THE MERGE ON A DEVELOPMENT SYSTEM WITH CURRENT DATA"
  ;; keep earliest id
  ;; mark pending as appropriate
  ;; determine host
  ;; merge offers/requests
  ;; merge invitations
  ;; merge events
  ;; merge contact lists (following and followers)
  ;; merge groups (admin and members)
  ;;** merge group-membership-requests
  ;; merge group-membership-invitations
  ;; merge transactions
  ;; merge conversations (individual and as group admin)
  ;; merge names/aliases
  ;; merge loves ?
  ;; merge feedback
  ;; merge comments
  ;; merge gifts
  ;; merge statements of gratitude (given and recieved)
  ;; avatar
  ;; add emails
  ;; modify-db for duplicates :type :deleted-group-account, w/ reason and redirect id
  ;; create redirects from old ids to new profile
  ;; index person
  ;; yikes!
  ;; test searching for deleted accounts
  ;; test requesting an invitation from account to be deleted

  (declare (optimize (speed 0) (debug 3) (safety 2)))

  (dolist (id ids-to-deactivate)
    (dolist (token (gethash id *user-tokens-index*))
      (delete-token-cookie :userid id
                           :cookie (car token)))
    (remhash id *user-tokens-index*))

  (setf (getf data-to-keep :created)
        (getf oldest-data :created))

  (setf data-to-keep
        (merge-user-data id-to-keep data-to-keep duplicate-account-id-list ids-to-deactivate))

  (merge-user-primary-indexes id-to-keep ids-to-deactivate)

  (merge-user-secondary-indexes id-to-keep ids-to-deactivate)

  (dolist (result *feedback-index*)
    (when (find (car (result-people result)) ids-to-deactivate)
      (setf (result-people result) (list id-to-keep))
      (modify-db (result-id result) :by id-to-keep)))

  (index-person id-to-keep (apply #'modify-db (cons id-to-keep data-to-keep))))

(defun merge-user-data (id-to-keep data-to-keep duplicate-account-id-list ids-to-deactivate)
  ;; merge data/indexes from user's data
  (dolist (duplicate-id duplicate-account-id-list)
    (let ((data (db duplicate-id)))

      (when (and (and (getf data :host)
                      (not (find (getf data :host) ids-to-deactivate)))
                 (or (not (getf data-to-keep :host))
                     (find (getf data-to-keep :host)
                           (cons +kindista-id+ ids-to-deactivate))))
        (setf (getf data-to-keep :host) (getf data :host)))

      (dolist (prop '(:bio :bio-summary :bio-doing :bio-contact :bio-skills :bio-into))
        (when (not (getf data-to-keep prop))
          (setf (getf data-to-keep prop)
                (getf data prop))))

      (asetf (getf data-to-keep :aliases)
             (remove-duplicates
               (remove
                 (getf data-to-keep :name)
                 (append (cons (getf data :name)
                               (getf data :aliases))
                         it)
                 :test #'equalp)
               :test #'equalp))

      (asetf (getf data-to-keep :emails)
             (remove-duplicates
               (append it (getf data :emails))
               :test #'equalp))

      (awhen (getf data :following)
        (with-locked-hash-table (*followers-index*)
          (dolist (person it)
            (remove duplicate-id (gethash person *followers-index*))
            (pushnew id-to-keep (gethash person *followers-index*))))
        (setf (getf data-to-keep :following)
              (set-difference (union (getf data-to-keep :following) it)
                              duplicate-account-id-list)))

      (when (getf data-to-keep :pending)
        (setf (getf data-to-keep :pending)
              (getf data :pending)))

      (aif (getf data-to-keep :username)
        (with-locked-hash-table (*username-index*)
          (remhash (getf data :username) *username-index*))
        (setf it (getf data :username)))

      (unless (getf data-to-keep :avatar)
        (setf (getf data-to-keep :avatar)
              (getf data :avatar)))))

  data-to-keep)

(defun merge-user-primary-indexes (id-to-keep ids-to-deactivate)
  ;; update indexes based on other data
  (dolist (duplicate-id ids-to-deactivate)

    (dolist (offer-id (copy-list (gethash duplicate-id *offer-index*)))
      (index-inventory-item offer-id (modify-db offer-id :by id-to-keep)))

    (dolist (request-id (copy-list (gethash duplicate-id *request-index*)))
      (index-inventory-item request-id
                            (modify-db request-id :by id-to-keep)))

    (dolist (invite-id (copy-list
                         (gethash duplicate-id *person-invitation-index*)))
      (index-invitation invite-id (modify-db invite-id :host id-to-keep)))

    (with-locked-hash-table (*person-invitation-index*)
      (remhash duplicate-id *person-invitation-index*))

    (with-locked-hash-table (*invited-index*)
      (dolist (guest-id (copy-list
                          (gethash duplicate-id *invited-index*)))
        (modify-db guest-id :host id-to-keep)
        (pushnew guest-id (gethash id-to-keep *invited-index*))))

    (with-locked-hash-table (*invited-index*)
      (remhash duplicate-id *invited-index*))

    (merge-user-group-membership id-to-keep duplicate-id)))

(defun merge-user-group-membership (id-to-keep duplicate-id)
    (dolist (group (copy-list (getf (gethash duplicate-id
                                             *group-privileges-index*)
                                    :admin)))
      (with-locked-hash-table (*group-members-index*)
        (asetf (gethash group *group-members-index*)
          (remove duplicate-id it))
        (pushnew id-to-keep (gethash group *group-members-index*)))
      (with-locked-hash-table (*group-privileges-index*)
        (pushnew group (getf (gethash id-to-keep *group-privileges-index*)
                             :admin))
        (asetf (getf (gethash id-to-keep *group-privileges-index*) :member)
               (remove group it)))
      (amodify-db group :admins (remove-duplicates
                                  (substitute id-to-keep duplicate-id it))
                        :members (remove id-to-keep it)
                        :creator (subst id-to-keep duplicate-id it)
                        :notify-message (subst id-to-keep duplicate-id it)
                        :notify-gratitude (subst id-to-keep duplicate-id it)
                        :notify-reminders (subst id-to-keep duplicate-id it)
                        :notify-membership-request (subst id-to-keep
                                                          duplicate-id
                                                          it)))

    (dolist (group (getf (copy-list (gethash duplicate-id
                                             *group-privileges-index*))
                         :member))
      (let ((already-group-admin-p (find id-to-keep (db group :admins))))
        (with-locked-hash-table (*group-members-index*)
          (asetf (gethash group *group-members-index*)
                 (remove duplicate-id it))
          (pushnew id-to-keep (gethash group *group-members-index*)))
        (unless already-group-admin-p
         (with-locked-hash-table (*group-privileges-index*)
            (pushnew group (getf (gethash id-to-keep *group-privileges-index*)
                                 :member))))
        (if already-group-admin-p
          (amodify-db group :members (remove duplicate-id it))
          (amodify-db group :members (remove-duplicates
                                       (substitute id-to-keep
                                                   duplicate-id
                                                   it))))))

    (with-locked-hash-table (*group-privileges-index*)
      (remhash duplicate-id *group-privileges-index*)))

(defun merge-user-secondary-indexes (id-to-keep ids-to-deactivate)
  ;; must be run after group re-assignment
  (dolist (duplicate-id ids-to-deactivate)
    (let ((users-groups (get-users-groups id-to-keep)))
      (dolist (message (apply #'append
                              (remove nil
                                      (remove-if-not
                                        #'listp
                                        (gethash duplicate-id
                                                 *person-mailbox-index*)))))
        (case (message-type message)

          ((or :transaction :conversation)
           (dolist (comment-id (gethash (message-id message)
                                        *comment-index*))
             (when (eql duplicate-id (car (db comment-id :by)))
               (amodify-db comment-id
                           :by (subst id-to-keep duplicate-id it))))

           (index-message (message-id message)
                          (amodify-db (message-id message)
                                      :participants (remove-duplicates
                                                      (substitute id-to-keep
                                                                  duplicate-id
                                                                  it))
                                      :message-folders (subst id-to-keep
                                                              duplicate-id
                                                              it)
                                      :people (subst id-to-keep
                                                     duplicate-id
                                                     it))))

          (:group-membership-request ; when user is an admin
            (index-message (message-id message)
                           (amodify-db (message-id message)
                                       :people (remove-duplicates
                                                 (substitute id-to-keep
                                                             duplicate-id
                                                             it))
                                       :message-folders (subst id-to-keep
                                                               duplicate-id
                                                               it))))

          (:group-membership-invitation ;when user is not yet in group
            (let ((group-id (db (message-id message) :group-id)))
              (if (or (find group-id users-groups)
                      (assoc id-to-keep
                             (gethash group-id
                                      *group-membership-invitations-index*)))
                (delete-group-membership-invitation (message-id message))
                (progn
                  (with-locked-hash-table
                    (*group-membership-invitations-index*)
                    (asetf (gethash group-id
                                    *group-membership-invitations-index*)
                           (subst id-to-keep duplicate-id it)))
                  (index-message (message-id message)
                                 (amodify-db (message-id message)
                                             :people (remove-duplicates
                                                       (substitute id-to-keep
                                                                   duplicate-id
                                                                   it))
                                             :message-folders (subst id-to-keep
                                                                     duplicate-id
                                                                     it)))))))))

      ;; for group invitations the user has SENT
      (dolist (group-id users-groups)
        (dolist (invite-id (mapcar #'cdr
                                   (gethash group-id
                                            *group-membership-invitations-index*)))
          (when (eql (db invite-id :invited-by) duplicate-id)
            (index-message invite-id
                           (modify-db invite-id :invited-by id-to-keep))))))

    ;; transactions might need to be done before gratitudes
    (dolist (result (copy-list (gethash duplicate-id *profile-activity-index*)))
      (case (result-type result)
        (:event
          (let ((event-id (result-id result)))
            (index-event event-id
                         (amodify-db event-id
                                     :people (remove-duplicates
                                               (substitute id-to-keep
                                                           duplicate-id
                                                           it))))))

        (:gift
          (let ((gift-id (result-id result)))
            (amodify-db gift-id :recipients (remove-duplicates
                                              (substitute id-to-keep
                                                          duplicate-id
                                                          it))
                        :giver (if (= it duplicate-id)
                                 id-to-keep
                                 it))
            (index-gift gift-id (db gift-id))))

        (:gratitude
          (let* ((gratitude-id (result-id result))
                 (gratitude (db gratitude-id))
                 (author-id (getf gratitude :author)))
            (amodify-db gratitude-id :author (if (= author-id duplicate-id)
                                               id-to-keep
                                               author-id)
                        :subjects (substitute id-to-keep
                                              duplicate-id
                                              it)
                        :people (subst id-to-keep
                                       duplicate-id
                                       it)
                        :message-folders (subst id-to-keep
                                                duplicate-id
                                                it))
            (index-gratitude gratitude-id (db gratitude-id))))))

    (delete-active-account duplicate-id
                           :reason (strcat "Duplicate account. "
                                           "Merged into account #"
                                           id-to-keep
                                           ".")
                           :merged-into id-to-keep))
  )

(defun deactivate-person (id)
  (let ((result (gethash id *db-results*)))
    (metaphone-index-insert (list nil) result)
    (geo-index-remove *people-geo-index* result)
    (geo-index-remove *activity-geo-index* result)
    (with-mutex (*active-people-mutex*)
      (asetf *active-people-index* (remove id it)))
    (dolist (request-id (gethash id *request-index*))
      (deactivate-inventory-item request-id))
    (dolist (offer-id (gethash id *offer-index*))
      (deactivate-inventory-item offer-id))
    (modify-db id :active nil
                  :notify-message nil
                  :notify-kindista nil
                  :notify-reminders nil
                  :notify-expired-invites nil
                  :notify-gratitude nil)))

(defun reactivate-person (id)
  (let* ((result (gethash id *db-results*))
         (data (db id))
         (names (cons (getf data :name)
                      (getf data :aliases))))
    (metaphone-index-insert names result)
    (geo-index-insert *people-geo-index* result) 
    (geo-index-insert *activity-geo-index* result) 
    (with-mutex (*active-people-mutex*)
      (push id *active-people-index*))
    (dolist (result (gethash id *profile-activity-index*))
      (when (and (eq (result-type result) :gratitude)
                 (find id (cdr (result-people result))))
        (let* ((gratitude-id (result-id result))
               (gratitude (db gratitude-id)))
          (when (getf gratitude :pending)
            (modify-db gratitude-id :pending nil)))))
    (modify-db id :active t
                  :notify-message t
                  :notify-kindista t
                  :notify-reminders t
                  :notify-expired-invites t
                  :notify-gratitude t)))

(defun delete-pending-account (id)
  (let ((data (db id)))

    (deactivate-person id)

    (with-locked-hash-table (*pending-person-items-index*)
      (dolist (item-id (gethash id *pending-person-items-index*))
        (remove-from-db item-id))
      (remhash id *pending-person-items-index*))

    (awhen (getf data :emails)
      (with-locked-hash-table (*email-index*)
        (dolist (email it)
          (remhash email *email-index*)))
      (with-locked-hash-table (*banned-emails-index*)
        (dolist (email it)
          (setf (gethash email *banned-emails-index*) id))))

    (awhen (getf data :username)
      (with-locked-hash-table (*username-index*)
        (remhash it *username-index*)))

    (modify-db id :pending nil
                  :banned t
                  :username nil
                  :notify-kindista nil)))

(defun delete-active-account
  (id
   &key reason
        merged-into
   &aux (data (db id)))

  (deactivate-person id)

  (awhen (getf data :emails)
    (with-locked-hash-table (*email-index*)
      (dolist (email it)
        (remhash email *email-index*))))

  (modify-db id :emails (list nil)
                :type :deleted-person-account
                :merged-into merged-into
                :merge-date (when merged-into (get-universal-time))
                :deleted t
                :reason-for-account-deletion reason))

(defun find-people-with-incorrect-communication-settings ()
  (sort (iter (for id in *active-people-index*)
          (let ((data (db id)))
            (when (or
                      ;(not (getf data :notify-kindista))
                      ;(not (getf data :notify-reminders))
                       (not (getf data :notify-message)))
              (collect id)))) #'<))

(defun find-people-with-incorrect-address-settings ()
  (sort (iter (for id in *active-people-index*)
          (let ((data (db id)))
            (when (and (string= (getf data :street) "NIL NIL")
                       (or (not (getf data :lat))
                           (not (getf data :long))))
              (collect id)))) #'<))

(defun mutual-connections-sidebar (link-list)
  (html
    (:div :class "people item right only"
     (:h3 "Mutual Connections")
     (str link-list))))

(defun person-activity-html (userid &key type)
  (let* ((mutuals (mutual-connections userid))
         (mutual-links (html (:ul (dolist (link (alpha-people-links mutuals))
                                      (htm (:li (str link))))))))
    (profile-activity-html userid :type type
                                  :right (when mutuals
                                           (mutual-connections-sidebar mutual-links)))))

(defun profile-mutual-connections-html (userid)
  (let* ((user (db userid))
         (strid (username-or-id userid))
         (mutuals (mutual-connections userid))
         (*base-url* (strcat "/people/" strid)))
    (standard-page
      (getf user :name)
      (html
        (when *user* (str (profile-tabs-html userid :tab :connections)))
        (:div :class "activity"
          (:ul :class "mutuals-list"
           (dolist (link (alpha-people-links mutuals))
             (htm (:li (str link)))))))

      :top (profile-top-html userid)

      :selected "people")))

(defun get-person (id)
  (ensuring-userid (id "/people/~a")
    (cond
     ;((or (not editing)
     ;     (not (eql id *userid*)))
     ; (if (getf (db id) :bio)
     ;   (profile-bio-html id)
     ;   (person-activity-html id)))

      ((get-parameter "edit")
       (profile-bio-html id))

      ((or (not (eql id *userid*))
           (getf *user* :bio-summary)
           (getf *user* :bio-into)
           (getf *user* :bio-contact)
           (getf *user* :bio-skils)
           (getf *user* :bio-doing))
       (person-activity-html id))

      ((eql id *userid*)
       (profile-bio-html id))

    (t (not-found)))))

(defun get-person-about (id)
  (ensuring-userid (id "/people/~a/about")
    (profile-bio-html id)))

(defun get-person-activity (id)
  (ensuring-userid (id "/people/~a/activity")
    (person-activity-html id)))

(defun get-person-reputation (id)
  (ensuring-userid (id "/people/~a/reputation")
    (person-activity-html id :type :gratitude)))

(defun get-person-offers (id)
  (require-user
    (ensuring-userid (id "/people/~a/offers")
      (person-activity-html id :type :offer))))

(defun get-person-requests (id)
  (require-user
    (ensuring-userid (id "/people/~a/requests")
      (person-activity-html id :type :request))))

(defun get-person-mutual-connections (id)
  (require-user
    (ensuring-userid (id "/people/~a/connections")
      (profile-mutual-connections-html id))))

(defun contactp (id)
  (member id (getf *user* :following)))

(defun go-people ()
  (moved-permanently "/people"))

(defun people-tabs-html (&key (tab :contacts))
  (html
    (:menu :type "toolbar" :class "bar" :id "people-tabs"
      (:h3 :class "label" "People Menu")

      (if (eql tab :contacts)
        (htm (:li :class "selected" "Contacts"))
        (htm (:li (:a :href "/people/contacts" "Contacts"))))

      (if (eql tab :nearby)
        (htm (:li :class "selected" "Nearby"))
        (htm (:li (:a :href "/people/nearby" "Nearby"))))

      (if (eql tab :suggested)
        (htm (:li :class "selected" "Suggested"))
        (htm (:li (:a :href "/people/suggested" "Suggested"))))

      (if (eql tab :invited)
        (htm (:li :class "selected" "Invited"))
        (htm (:li (:a :href "/people/invited" "Invited")))))))

(defun get-people ()
  (if *user*
    (see-other "/people/contacts")
    (see-other "/people/nearby")))

(defun get-people-contacts ()
  (if *user*
    (my-contacts :person (people-tabs-html) "people")
    (see-other "/people/nearby")))

(defun get-people-nearby ()
  (nearby-profiles-html "people" (when *user* (people-tabs-html :tab :nearby))))

(defun post-people-suggested ()
  (when *user*
    (cond
      ((scan +number-scanner+ (post-parameter "remove"))
       (let ((other-person-id (parse-integer (post-parameter "remove")))
             (hidden-suggestions (getf *user* :hidden-suggested-contacts))
             (contacts (getf *user* :following)))
         (unless (or (member other-person-id contacts)
                     (assoc other-person-id hidden-suggestions))
           (hide-from-suggestions other-person-id *userid*)))
       (see-other (referer))))))

(defun get-people-suggested ()  ;(&optional userid *userid*)
  (if *user*
    (standard-page "Suggested people"
      (html
        (str (people-contacts-action-menu))
        (str (people-tabs-html :tab :suggested))
        (aif (return-suggested-people)
          (dolist (suggestion it)
            (let* ((id (car suggestion))
                   (group (eq (db id :type) :group)))
              (unless group
                (str (suggested-contact-card id suggestion)))))

          (htm (:h4 "You have no suggested contacts at this time."))))

      :selected "people"
      :right (html
               (str (donate-sidebar))
               (str (invite-sidebar))))
    (see-other "/people/nearby")))

(defun get-people-invited ()
  (require-user
    (let ((confirmed (gethash *userid* *invited-index*))
          (unconfirmed (unconfirmed-invites)))
      (standard-page
        "Invited"
        (html
          (str (people-contacts-action-menu))
          (str (people-tabs-html :tab :invited))
          (:div :id "my-invites"
            (when unconfirmed
              (htm
                (:h3 :class "my-invites" "Awaiting RSVP ")
                (:ul
                  (dolist (invite unconfirmed)
                    (let* ((id (getf invite :id))
                           (email (getf invite :email))
                           (times-sent (getf invite :times-sent))
                           (last-sent (getf invite :last-sent))
                           (expired (getf invite :expired)))
                      (htm
                        (:li
                          (:form :method "post" :action "/people/invited"
                            (:input :type "hidden" :name "invite-id" :value id)
                            (:button :class "yes" :type "submit" :name "resend"
                              (if expired
                                (htm "Renew invitation")
                                (htm "Resend invite")))
                            (:button :class "cancel" :type "submit" :name "delete" "Delete"))
                          (str email)
                          (:small :class "gray-text"
                            (if expired
                              (str (s+ " (expired "
                                       (humanize-universal-time expired) ")"))
                              (str (s+ (if (< 1 (length times-sent))
                                         " (reminder sent "
                                         " (invited ")
                                       (humanize-universal-time last-sent) ")")))))))))))

            (unless (or unconfirmed confirmed)
              (htm
                (:h2 "no invitations yet.")))

            (:p
              "would you like to "
              (:a :href "/invite" (str (s+ "invite someone"
                                           (when (or unconfirmed confirmed)
                                             " else"))))
              "?")))

        :selected "people"
        :right (html
                 (str (donate-sidebar))
                 (str (invite-sidebar)))))))

(defun post-people-invited ()
  (require-active-user
    (let* ((id (parse-integer (or (post-parameter "invite-id")
                                  (post-parameter "item-id"))))
           (invitation (db id))
           (email (getf invitation :recipient-email)))
      (when (eql (getf invitation :host) *userid*)
        (cond
          ((post-parameter "delete")
           (confirm-delete :url "/people/invited"
                           :item-id id
                           :next-url "/people/invited"
                           :type (s+ "invitation to " email)))
          ((post-parameter "really-delete")
           (delete-invitation id)
           (flash (s+ "Your invitation to " email " has been deleted."))
           (see-other "/people/invited"))
          ((post-parameter "resend")
           (resend-invitation-html id))
          ((post-parameter "cancel")
           (see-other "/people/invited"))
          ((post-parameter "confirm-resend")
           (resend-invitation id :text (awhen (post-parameter "text") it))
           (flash (strcat "Your invitation to " email " has been resent."))
           (see-other "/people/invited")))))))
