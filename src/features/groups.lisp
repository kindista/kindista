;;; Copyright 2012-2017 CommonGoods Network, Inc.
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

(defun new-group-membership-request-notice-handler ()
  (send-group-membership-request-notification-email (getf (cddddr *notice*) :id)))

(defun new-group-membership-invitation-notice-handler ()
  (send-group-membership-invitation-notification-email (getf (cddddr *notice*) :id)))

(defun create-group (&key name creator lat long address street city state country zip location-privacy category membership-method)
  (insert-db (list :type :group
                   :name name
                   :creator creator
                   :admins (list creator)
                   :active t
                   :location t
                   :lat lat
                   :long long
                   :address address
                   :street street
                   :city city
                   :state state
                   :country country
                   :zip zip
                   :category category
                   :membership-method membership-method
                   :location-privacy location-privacy
                   :notify-message (list creator)
                   :notify-gratitude (list creator)
                   :notify-reminders (list creator)
                   :notify-membership-request (list creator)
                   :notify-inventory-expiration (list creator)
                   :created (get-universal-time))))

(defun index-group (id data)
  (let ((result (make-result :id id
                             :latitude (getf data :lat)
                             :longitude (getf data :long)
                             :type :group
                             :people (append (getf data :admins)
                                             (getf data :members))
                             :time (getf data :created)))
        (name (getf data :name)))

    (with-locked-hash-table (*db-results*)
      (setf (gethash id *db-results*) result))

    (when (getf data :username)
      (with-locked-hash-table (*username-index*)
        (setf (gethash (getf data :username) *username-index*) id)))

    (with-locked-hash-table (*group-privileges-index*)
      (dolist (person (getf data :admins))
        (push id (getf (gethash person *group-privileges-index*) :admin)))
      (dolist (person (getf data :members))
        (push id (getf (gethash person *group-privileges-index*) :member))))

    (with-locked-hash-table (*group-members-index*)
      (dolist (person (getf data :admins))
        (push person (gethash id *group-members-index*)))
      (dolist (person (getf data :members))
        (push person (gethash id *group-members-index*))))

    (with-locked-hash-table (*profile-activity-index*)
      (asetf (gethash id *profile-activity-index*)
             (safe-sort (push result it) #'> :key #'result-time)))

    (unless (< (result-time result) (- (get-universal-time) 2592000))
      (with-mutex (*recent-activity-mutex*)
        (push result *recent-activity-index*)))

    (when (and (getf data :lat)
               (getf data :long)
               (getf data :created)
               (getf data :active))

      (metaphone-index-insert (list name) result)
      (geo-index-insert *groups-geo-index* result) 

      (unless (< (result-time result) (- (get-universal-time) 15552000))
        (geo-index-insert *activity-geo-index* result)))))

(defun reindex-group-location (id)
  ;when groups change locations
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
      (notice :error :on "no db result on reindex-group-location"))

    (geo-index-remove *groups-geo-index* result)
    (geo-index-remove *activity-geo-index* result)

    (setf (result-latitude result) lat)
    (setf (result-longitude result) long)

    (when (and lat long
               (getf data :created)
               (getf data :active))

      (metaphone-index-insert (list (getf data :name)) result)
      (geo-index-insert *groups-geo-index* result)

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
            (geo-index-insert *activity-geo-index* result)))))))

(defun reindex-group-name (id)
  (metaphone-index-insert (list (db id :name))
                          (gethash id *db-results*)))

(defun merge-new-duplicate-group-account (new-id pre-existing-id group-creator &key keep-new-name keep-new-location)
"WARNING: This needs further testing before use on the live server."
  (delete-group new-id :pre-existing-duplicate-id pre-existing-id
                       :group-creator group-creator
                       :keep-new-name keep-new-name
                       :keep-new-location keep-new-location))

(defun deactivate-group
  (id
   &aux (result (gethash id *db-results*)))
  (metaphone-index-insert (list nil) result)
  (geo-index-remove *groups-geo-index* result)
  (geo-index-remove *activity-geo-index* result)
  (dolist (request-id (gethash id *request-index*))
    (deactivate-inventory-item request-id))
  (dolist (offer-id (gethash id *offer-index*))
    (deactivate-inventory-item offer-id))
  (modify-db id :active nil))

(defun reactivate-group
  (id
   &aux (result (gethash id *db-results*))
        (data (db id))
        (name (getf data :name)))
  (metaphone-index-insert (list name) result)
  (geo-index-insert *groups-geo-index* result)
  (geo-index-insert *activity-geo-index* result)
  (modify-db id :active t))

(defun delete-group (id &key pre-existing-duplicate-id group-creator keep-new-name keep-new-location)
"WARNING: This needs further testing before use on the live server."
  (let* ((data (db id))
         (pre-existing-data (db pre-existing-duplicate-id))
         (result (gethash id *db-results*)))

    (with-locked-hash-table (*username-index*)
      (remhash (getf data :username) *username-index*))

    (with-locked-hash-table (*group-privileges-index*)
      (dolist (person (getf data :admins))
        (asetf (getf (gethash person *group-privileges-index*) :admin)
               (remove id it)))
      (dolist (person (getf data :members))
        (asetf (getf (gethash person *group-privileges-index*) :member)
               (remove id it))))

    (with-locked-hash-table (*profile-activity-index*)
      (dolist (result (gethash id *profile-activity-index*))
        (let* ((item-id (result-id result))
               (item-data (db item-id))
               (item-data-type (getf item-data :type)))
          (flet ((switch-ids (list)
                   (cons pre-existing-duplicate-id
                         (remove nil
                                 (remove id list)))))
            (case item-data-type
             ((or :offer :request)
              (aif pre-existing-duplicate-id
                (progn
                  (modify-db item-id :by it)
                  (setf (result-people result) it)
                  (push result (gethash it *profile-activity-index*)))
                (deactivate-inventory-item item-id)))
             (:gratitude
               (aif pre-existing-duplicate-id
                 (progn
                   (pushnew result (gethash it *profile-activity-index*))
                   (cond
                     ((= (getf item-data :author) id)
                      (setf (car (result-people result)) pre-existing-duplicate-id)
                      (modify-db item-id :author pre-existing-duplicate-id))
                     ((find id (getf item-data :subjects))
                      (asetf (cdr (result-people result)) (switch-ids it))
                      (amodify-db item-id :subjects (switch-ids it)))))
                 (delete-gratitude item-id)))
              (:event
                (aif pre-existing-duplicate-id
                  (let ((new-hosts (switch-ids (getf item-data :hosts))))
                    (modify-db item-id :hosts new-hosts)
                    (setf (result-people result) new-hosts))
                  (deactivate-inventory-item item-id))))))))

    (when (and (getf data :lat)
               (getf data :long)
               (getf data :created)
               (getf data :active))

      (metaphone-index-insert (list nil) result)
      (geo-index-remove *groups-geo-index* result)

      (unless (< (result-time result) (- (get-universal-time) 15552000))
        (geo-index-remove *activity-geo-index* result)))

    (when pre-existing-duplicate-id
      (let ((modify-pre-existing-data
              (list :avatar (or (getf pre-existing-data :avatar)
                                (getf data :avatar))
                    :category (getf data :category)
                    :membership-method (getf data :membership-method)
                    :location-privacy (getf data :location-privacy)
                    :creator (or group-creator
                                 (getf pre-existing-data :creator)
                                 (getf data :creator)))))
        (when keep-new-name (asetf modify-pre-existing-data
                                   (append (list :name (getf data :name)) it)))
        (when keep-new-location (asetf modify-pre-existing-data
                                  (append (list :location (getf data :location)
                                                :lat (getf data :lat)
                                                :long (getf data :long)
                                                :address (getf data :address)
                                                :street (getf data :street)
                                                :city (getf data :city)
                                                :state (getf data :state)
                                                :country (getf data :country)
                                                :zip (getf data :zip))
                                          it)))
        (apply #'modify-db pre-existing-duplicate-id modify-pre-existing-data))

      (dolist (message (gethash id *group-messages-index*))
        (let ((message-id (message-id message))
              (people (message-people message)))
          (case (message-type message )
            ((or :transaction :conversation)
             (setf (message-people message)
                   (subst pre-existing-duplicate-id id people))
             (modify-db message-id :people (message-people message))))))

      (dolist (follower (gethash id *followers-index*))
        (amodify-db follower :following (subst pre-existing-duplicate-id id it))))

    (when (and group-creator
               (not (eql (getf pre-existing-data :type) :group)))
      (change-person-to-group pre-existing-duplicate-id group-creator)))

  (with-locked-hash-table (*db-results*)
    (remhash id *db-results*))

  (modify-db id :type :deleted-group-account
                :reason-for-account-deletion (strcat "Merged with pre-existing-group-account: "
                                                     pre-existing-duplicate-id)))

(defun change-person-to-group (groupid admin-id)

  (awhen (db groupid :emails)
    (with-locked-hash-table (*email-index*)
      (dolist (email it)
        (remhash email *email-index*))))

  (with-mutex (*active-people-mutex*)
    (asetf *active-people-index* (remove groupid it)))

  (modify-db groupid :type :group
                     :creator admin-id
                     :admins (list admin-id)
                     :membership-method :invite-only
                     :notify-gratitude (list admin-id)
                     :notify-reminders (list admin-id)
                     :notify-membership-request (list admin-id)
                     :notify-message (list admin-id)
                     :notify-kindista nil
                     :notify-expired-invites nil
                     :emails nil
                     :pass nil)
  ;move all the group's messages into the admin's mailbox
  (flet ((remove-groupid (item)
           (if (listp item)
             (remove groupid item)
             item)))
    (let (completed)
      (doplist (folder messages (gethash groupid *person-mailbox-index*))
        (dolist (message messages)
          (unless (member message completed)
            (let ((id (message-id message))
                  (new-mailbox (cons admin-id groupid))
                  (people (mapcar #'caar (message-people message)))
                  (mailboxes (copy-list (message-people message)))
                  (folders (copy-list (message-folders message))))

              (asetf (car (assoc (list groupid) mailboxes :test #'equal))
                     new-mailbox)
             ;(asetf mailboxes
             ;       (remove (assoc (list admin-id) it :test #'equal)
             ;                it :test #'equal))

              (if (and (member groupid people)
                       (member admin-id people))
                (asetf folders (mapcar #'remove-groupid it))
                (asetf folders (subst admin-id groupid it)))

              (index-message id (modify-db id :message-folders folders
                                              :people mailboxes))
              (dolist (comment (gethash id *comment-index*))
                (when (eq (car (db comment :by)) groupid)
                  (modify-db comment :by new-mailbox)))
             (push message completed)))))))

  (index-group groupid (db groupid))
  (reindex-group-location groupid)
  (reindex-group-name groupid))

(defun change-group-category (groupid &key new-type)
  (let* ((standard-category (post-parameter-string "group-category"))
         (custom-category (post-parameter-string "custom-group-category"))
         ;;new-type is a way to change group-type from the back end
         (next (or (post-parameter "next")
                   (referer)))
         (newtype (or new-type custom-category standard-category)))
    (if (and (string= standard-category "other")
             (not custom-category))
      (see-other (url-compose next "group-category" "other"))
      (progn (modify-db groupid :category newtype)
             (see-other next)))))

(defun all-groups ()
  (loop for id being the hash-keys in *group-members-index*
        collect (gethash id *db-results*)))

(defun group-members (groupid)
  (gethash groupid *group-members-index*))

(defun add-group-member (personid groupid)
  (amodify-db groupid :members (pushnew personid it))
  (with-locked-hash-table (*group-members-index*)
     (pushnew personid (gethash groupid *group-members-index*)))
  (with-locked-hash-table (*group-privileges-index*)
     (pushnew groupid (getf (gethash personid *group-privileges-index*) :member))))

(defun remove-group-member (personid groupid)
  (amodify-db groupid :members (remove personid it))
  (with-locked-hash-table (*group-members-index*)
     (asetf (gethash groupid *group-members-index*)
            (remove personid it)))
  (with-locked-hash-table (*group-privileges-index*)
     (asetf (getf (gethash personid *group-privileges-index*) :member)
            (remove groupid it))))

(defun add-group-admin (personid groupid)
  (assert (eql (db personid :type) :person))
  (with-locked-hash-table (*group-privileges-index*)
     (asetf (getf (gethash personid *group-privileges-index*) :admin)
            (pushnew groupid it))
     (asetf (getf (gethash personid *group-privileges-index*) :member)
            (remove groupid it)))
  (if (member personid (db groupid :members))
    (amodify-db groupid :members (remove personid it)
                        :admins (pushnew personid it)
                        :notify-message (pushnew personid it)
                        :notify-gratitude (pushnew personid it)
                        :notify-reminders (pushnew personid it)
                        :notify-inventory-expiration (pushnew personid it)
                        :notify-membership-request (pushnew personid it))
    (amodify-db groupid :admins (pushnew personid it)
                        :notify-message (pushnew personid it)
                        :notify-gratitude (pushnew personid it)
                        :notify-reminders (pushnew personid it)
                        :notify-inventory-expiration (pushnew personid it)
                        :notify-membership-request (pushnew personid it))))

(defun remove-group-admin (personid groupid)
  (assert (eql (db personid :type) :person))
  (with-locked-hash-table (*group-privileges-index*)
     (asetf (getf (gethash personid *group-privileges-index*) :admin)
            (remove groupid it))
     (asetf (getf (gethash personid *group-privileges-index*) :member)
            (pushnew groupid it)))
  (amodify-db groupid :admins (remove personid it)
                      :members (pushnew personid it)
                      :notify-message (remove personid it)
                      :notify-gratitude (remove personid it)
                      :notify-reminders (remove personid it)
                      :notify-inventory-expiration (remove personid it)
                      :notify-membership-request (remove personid it)))

(defun group-activity-html (groupid &key type)
  (profile-activity-html groupid :type type
                                 :right (group-sidebar groupid)))

(defun groups-with-user-as-admin (&optional (userid *userid*)
                                            omit-deactivated-groups
                                  &aux group)
"Returns an a-list of (groupid . group-name)"
  (loop for id in (getf (if (eql userid *userid*)
                          *user-group-privileges*
                          (gethash userid *group-privileges-index*))
                        :admin)
        do (setf group (db id))
        when (or (not omit-deactivated-groups)
                 (getf group :active))
        collect (cons id (db id :name))))


(defun groups-with-user-as-member (&optional (userid *userid*)
                                             omit-deactivated-groups
                                   &aux group)
"Returns an a-list of (groupid . group-name)"
  (loop for id in (getf (if (eql userid *userid*)
                          *user-group-privileges*
                          (gethash userid *group-privileges-index*))
                        :member)
        do (setf group (db id))
        when (or (not omit-deactivated-groups)
                 (getf group :active))
        collect (cons id (db id :name))))

(defun group-admin-p (groupid &optional (userid *userid*))
"Returns the groupid if userid is an admin for that group."
  (find groupid
        (getf (if (eql userid *userid*)
                *user-group-privileges*
                (gethash userid *group-privileges-index*))
              :admin)))

(defmacro require-group-admin (groupid &body body)
  `(if (group-admin-p ,groupid)
     (progn ,@body)
     (permission-denied)))

(defun group-member-links (groupid &key ul-class)
  (let* ((group (db groupid))
         (admins (getf group :admins))
         (members (append admins (getf group :members))))
    (labels ((admin-p (id)
               (not (null (member id admins))))
             (member-less-p (m1 m2)
               (cond
                 ((eql (admin-p (first m1))
                       (admin-p (first m2)))
                  (string-lessp (second m1) (second m2)))
                 ((admin-p (first m1))
                  t)
                 (t
                  nil)))
             (triple (id)
               (let ((name (db id :name)))
                 (list id name (person-link id)))))
      (html
        (:ul :class (or ul-class "")
          (dolist (list (sort (mapcar #'triple members) #'member-less-p))
            (htm
              (:li (str (third list))
               (when (admin-p (first list))
                 (htm " (admin)"))))))))))

(defun group-sidebar (groupid)
  (html
    (when (group-admin-p groupid)
      (htm (:div :class "item right only"
             (:a :href (strcat "/groups/" groupid "/invite-members")
              "+add group members")))
      (str (membership-requests groupid :class "people item right only")))
    (str (members-sidebar groupid))))

(defun invite-group-members (groupid &key emails)
  (standard-page
    "Invite group members"
    (html
      (:div :class "invite-members"
        (:h2 "Invite "
             (when (get-parameter "add-another")
               (htm "more "))
             "people to join "
         (str
           (if (eq groupid +kindista-id+)
             "Kindista's group account"
             (db groupid :name))))
        (:form :method "get"
               :action (strcat "/groups/" groupid "/invite-members")
         (:label "Search by name:"
           (:input :type "text"
                   :name "search-name"
                   :placeholder (escape-for-html "Search current Kindista's by name")))
         (:button :class "submit yes" :name "add-member" :type "submit" "Search"))
        (aif (get-parameter "search-name")
          (let ((results (search-people it)))
            (if results
              (dolist (result results)
                (htm
                  (:form :method "post"
                       :class "invite-member-result"
                       :action (strcat "/groups/" groupid "/invite-members")
                     (str
                       (person-card
                         (car result)
                         :alias (cdr result)
                         :button (html (:button :class "submit yes"
                                                :name "invite-member"
                                                :value (car result)
                                                :type "submit"
                                                "Invite")))))))
              (htm
                (:p
                  "There are no Kindista members with the name "
                  (str it)
                  " .  Please try again."))))
          (htm
            (:form :method "post"
                   :action (strcat "/groups/" groupid "/invite-members")
              (:label  "...or invite people by email address:"
                (:textarea :rows "8"
                           :name "bulk-emails"
                           :placeholder "Separate multiple email addresses with comas..."
                           (when emails (str (separate-with-commas emails)))))
              (:button :class "submit yes" :name "add-member-by-email" :type "submit" "Invite")  )))))
    :top (simple-profile-top groupid)
    :selected "groups"))

(defun membership-requests (groupid &key (class ""))
  (let ((requests (gethash groupid *group-membership-requests-index*)))
    (when requests
      (html
        (:div :class (s+ "membership-requests " class)
          (:h3 "Membership requests")
            (dolist (request requests)
              (let* ((personid (car request))
                     (link (s+ "/people/" (username-or-id personid))))
                (htm
                  (:div :class "membership-request"
                    (:div :class "avatar"
                      (str
                        (v-align-middle
                          (:a :href link
                            (:img :src (get-avatar-thumbnail personid 70 70))))))
                    (:div :class "member-approval"
                      (str
                        (v-align-middle
                          (:form :class "member-approval-ui"
                                 :method "post"
                                 :action (strcat "/groups/" groupid "/members")
                             (:input :type "hidden"
                                     :name "membership-request-id"
                                     :value (cdr request))
                             (:button :class "yes small"
                                      :type "submit"
                                      :name "approve-group-membership-request"
                                      "approve")
                             (:button :class "cancel small"
                                      :type "submit"
                                      :name "deny-group-membership-request"
                                      "deny")))))
                    (:div :class "request-name"
                      (str (v-align-middle
                             (str (person-link personid))))))))))))))

(defun members-sidebar (groupid)
  (html
    (:div :class "people item right only"
      (:h3 "Group members")
      (str (group-member-links groupid)))))

(defun profile-group-members-html (groupid)
  (let* ((group (db groupid))
         (strid (username-or-id groupid))
         (membership-requests (membership-requests groupid :class "item members-tab"))
         (*base-url* (strcat "/groups/" strid)))
    (standard-page
      (getf group :name)
      (html
        (when *user* (str (profile-tabs-html groupid :tab :members)))
        (:div :class "activity"
          (when (group-admin-p groupid)
            (htm
              (:div :class "members-tab invite-members"
               (:a :href (strcat "/groups/" groupid "/invite-members")
                   :class "float-right"
                  "+add group members")))
            (when membership-requests
              (str membership-requests))
            (htm (:h3 "Current Members")))
          (str (group-member-links groupid :ul-class "members-tab current-members"))))
      :top (profile-top-html groupid)
      :selected "groups")))

(defun group-activity-selection-html (groupid group-name selected tab)
  (html
    (:form :method "get"
           :class "group-activity-selection small"
           :action (strcat "/groups/" groupid "/" tab)
      (:strong "Display "
               (str (cond
                     ((or (string= tab "activity")
                          (string= tab "offers")
                          (string= tab "requests"))
                      (s+ tab " from "))
                     ((string= tab "reputation")
               "gratitude about "))))
      (:div
        (:div
           (:label (:input :type "checkbox"
                           :name "group"
                           :id "group"
                           :onchange "this.form.submit()"
                           :checked (when (or (string= selected "all")
                                              (string= selected "group"))
                                          ""))
                   (str group-name)))
         (:div
           (:label (:input :type "checkbox"
                            :id "members"
                            :name "members"
                            :onchange "this.form.submit()"
                            :checked (when (or (string= selected "all")
                                               (string= selected "members"))
                                        ""))
                   "group members" ))))))

(defun group-members-activity (group-members &key type count)
  (let ((count (or count (+ 20 (floor (/ 30 (length group-members))))))
        ;; unless count is specified, limit the number of activity items calculated
        ;; for a given member to be from 20 (in a big group) to 50.
        (i 0)
        (activity nil))
    (dolist (person group-members)
      (setf i 0)
      (loop for result in (remove-private-items
                            (gethash person *profile-activity-index*))
            while (< i count)
            when (and (not (member result activity)) ; prevent duplicates
                      ;; activity rank needs dates in the past
                      ;; so it doesn't work for upcoming events
                      (not (eql (result-type result) :event))
                      (or (not type)
                          (if (eql type :gratitude)
                            (or (and (eql :gratitude (result-type result))
                                     (not (eql person (first (result-people result)))))
                                (and (eql :gift (result-type result))
                                     (not (eql person (car (last (result-people result)))))))
                            (eql type (result-type result)))))
            do (progn
                 (if activity
                   (asetf activity (push result it))
                   (asetf activity (list result)))
                 (incf i))))
    (sort activity #'> :key #'activity-rank)))

(defun get-groups ()
  (if (and *user*
           (> (+ (length (getf *user-group-privileges* :admin))
                 (length (getf *user-group-privileges* :member)))
              0))
    (see-other "/groups/my-groups")
    (see-other "/groups/nearby")))

(defun get-my-groups ()
  (if *user*
    (let ((admin-groups (groups-with-user-as-admin))
          (member-groups (groups-with-user-as-member)))
      (standard-page
        "My Groups"
        (html
          (str (group-contacts-action-menu))
          (str (groups-tabs-html :tab :my-groups))
          (unless (or admin-groups member-groups)
            (htm (:h3 "You have not joined any groups yet.")))
          (dolist (group (sort admin-groups #'string-lessp :key #'cdr))
            (str (group-card (car group))))
          (dolist (group (sort member-groups #'string-lessp :key #'cdr))
            (str (group-card (car group)))))
        :selected "groups"
        :right (html
                 (str (donate-sidebar))
                 (str (invite-sidebar)))))

      (see-other "groups/nearby")))

(defun get-groups-contacts ()
  (if *user*
    (my-contacts :group (groups-tabs-html :tab :contacts) "groups")
    (see-other "/groups/nearby")))

(defun get-groups-nearby ()
  (nearby-profiles-html "groups" (groups-tabs-html :tab :nearby)))

(defun get-groups-new ()
  (require-user (:require-active-user t :require-email t)
    (if (getf *user* :pending)
      (progn
        (pending-flash "create group profiles on Kindista")
        (see-other (or (referer) "/home")))
      (enter-new-group-details))))

(defun enter-new-group-details (&key error location name public-location membership-method)
  (standard-page
    "Create a group profile"
    (html
      (when error
        (flash error :error t))
      (:div :class "item"
        (:h2 "Create a Kindista profile for your group")
        (:div :class "item create-group"
          (:form :method "post" :action "/groups/new"
            (:div
              (:label :for "new-group-name" "Group name")
              (:input :type "text"
                      :id "new-group-name"
                      :name "name"
                      :placeholder (escape-for-html "Enter your group's name")
                      :value (awhen name (escape-for-html it))))
            (:div
              (:label :for "new-group-location" "Group location")
              (:input :type "text"
                      :id "new-group-location"
                      :name "location"
                      :placeholder (escape-for-html "Enter your group's address")
                      :value (awhen location (escape-for-html it)))
              (:br)
              (:input :type "checkbox"
                      :name "public-location"
                      :value (str (when public-location "checked")))
              "Display this address publicly to anyone looking at this group's profile page.")
            (:p (:em "Please note: you will be able to change the visibiity of your group's address on its settings page."))

            (:h3 "What kind of a group is this?")
            (:div :class "group-category-selection"
              (str
                (group-category-selection :class "new-group identity"
                                          :selected (or (post-parameter "group-category")
                                                        (post-parameter "custom-group-category")))))
            (:div :class "long"
              (str (group-membership-method-selection membership-method)))

           (:button :class "cancel"
                    :type "submit"
                    :name "cancel"

                    "Cancel")
           (:button :class "yes"
                    :type "submit"
                    :value "1"
                    :name "create-group"
                    "Create")))))))

(defun post-groups-new ()
  (require-user (:require-active-user t :require-email t)
    (let ((location (post-parameter-string "location"))
          (name (when (scan +text-scanner+ (post-parameter "name"))
                  (post-parameter "name")))
          (lat (post-parameter-float "lat"))
          (long (post-parameter-float "long"))
          (city (post-parameter-string "city"))
          (state (post-parameter-string  "state"))
          (country (post-parameter-string "country"))
          (street (awhen (post-parameter "street")
                    (unless (or (string= it "")
                                (equalp it "nil nil"))
                   it)))
          (zip (post-parameter-string "zip"))
          (group-category (or (post-parameter-string "custom-group-category")
                              (post-parameter-string "group-category")))
          (membership-method (post-parameter-string "membership-method"))
          (public (when (post-parameter "public-location") t)))

      (labels ((try-again (&optional e)
                 (enter-new-group-details :name (post-parameter "name")
                                          :location location
                                          :membership-method membership-method
                                          :error e))
               (new-group ()
                 (see-other
                   (strcat "/groups/"
                           (create-group :name name
                                         :creator *userid*
                                         :lat lat
                                         :long long
                                         :address location
                                         :street street
                                         :city city
                                         :state state
                                         :country country
                                         :zip zip
                                         :category (awhen group-category
                                                     (unless (string= "other"
                                                                      it)
                                                       it))
                                         :membership-method (if (string= membership-method
                                                                         "invite-only")
                                                              :invite-only
                                                              :group-admin-approval)
                                         :location-privacy (if public
                                                             :public
                                                             :private)))))

               (confirm-location ()
                 (multiple-value-bind (clat clong caddress ccity cstate ccountry cstreet czip)
                   (geocode-address location)
                   (verify-location "/groups/new"
                                    "Please verify your group's location."
                                    clat
                                    clong
                                    "name" name
                                    "location" caddress
                                    "city" ccity
                                    "state" cstate
                                    "country" ccountry
                                    "street" cstreet
                                    "zip" czip
                                    "group-category" group-category
                                    "membership-method" membership-method
                                    (when public "public-location")
                                    (when public "t")))))

        (cond
          ((getf *user* :pending)
           (pending-flash "create group profiles on Kindista")
           (see-other (or (referer) "/home")))

          ((post-parameter "cancel")
           (see-other "/groups"))

          ((> (length group-category) 48)
           (try-again "Please use a shorter custom group category name"))

          ((and group-category
                (nor (post-parameter "create-group")
                     (post-parameter "confirm-location")))
           (try-again))

          ((< (length name) 4)
           (try-again "Please use a longer name for your group"))

          ((or (not location)
               (string= location ""))
           (try-again "Please add a location for your group"))

         ((post-parameter "reset-location")
          (enter-new-group-details :name name
                                   :membership-method membership-method))

         ((not (and lat long city state location country))
          ;geocode the address
          (confirm-location))

         ((post-parameter "confirm-location")
          (aif (search-groups name :lat lat :long long)
            (confirm-group-uniqueness it name lat long location city state country street zip membership-method :public-location public)
            (new-group)))

         ((and lat long city state location country (post-parameter "confirm-uniqueness"))
          (new-group)))))))

(defun resend-group-membership-request (request-id)
  (index-message request-id
                 (modify-db request-id :resent (get-universal-time))))

(defun create-group-membership-request (groupid &key (person *userid*))
  (let* ((time (get-universal-time))
         (mailboxes (mailbox-ids (list groupid)))
         (people (mapcar #'(lambda (mailbox) (cons mailbox :unread))
                         mailboxes))
         (admin-ids (mapcar #'car mailboxes))
         (id (insert-db (list :type :group-membership-request
                              :group-id groupid
                              :people people ;group admins at time of request
                              :message-folders (list :inbox admin-ids
                                                     :unread admin-ids)
                              :requested-by person
                              :created time))))

    (notice :new-group-membership-request :time time :id id)

    id))

(defun index-group-membership-request (id data)
  (let ((groupid (getf data :group-id)))
    (with-locked-hash-table (*group-membership-requests-index*)
      (asetf (gethash groupid *group-membership-requests-index*)
             (acons (getf data :requested-by) id it))))
  (index-message id data))

(defun delete-group-membership-request (id)
  (let* ((request (db id))
         (group (getf request :group-id)))
    (with-locked-hash-table (*group-membership-requests-index*)
      (asetf (gethash group *group-membership-requests-index*)
             (remove (cons (getf request :requested-by) id)
                     it :test #'equal)))
    (remove-message-from-indexes id)
    (remove-from-db id)))

(defun approve-group-membership-request (id)
  (let* ((request (db id))
         (requested-by (getf request :requested-by))
         (group (getf request :group-id)))
    (unless (eql (db requested-by :type) :deleted-person-account)
      (add-group-member (getf request :requested-by) group))
    (delete-group-membership-request id)))

(defun create-group-membership-invitation (groupid invitee &key (host *userid*))
  (let* ((time (get-universal-time))
         (id (insert-db (list :type :group-membership-invitation
                              :group-id groupid
                              :people (list (cons (list invitee)  :unread))
                              :message-folders (list :inbox (list invitee)
                                                     :unread (list invitee))
                              :invited-by host
                              :created time))))

    (notice :new-group-membership-invitation :time time :id id)

    id))

(defun resend-group-membership-invitation (invitation-id)
  (index-message invitation-id
                 (modify-db invitation-id :resent (get-universal-time))))

(defun index-group-membership-invitation (id data)
  (let ((groupid (getf data :group-id)))
    (with-locked-hash-table (*group-membership-invitations-index*)
      (asetf (gethash groupid *group-membership-invitations-index*)
             (acons (caaar (getf data :people)) id it))))
  (index-message id data))

(defun accept-group-membership-invitation (id)
  (let* ((invitation (db id))
         (group (getf invitation :group-id))
         (invitee (caaar (getf invitation :people))))
    (add-group-member invitee group)
    (with-locked-hash-table (*group-membership-invitations-index*)
      (asetf (gethash group *group-membership-invitations-index*)
             (remove (cons invitee id) it :test #'equal)))
    (remove-message-from-indexes id)
    (remove-from-db id)))

(defun delete-group-membership-invitation (id)
  (let* ((invitation (db id))
         (group (getf invitation :group-id)))
    (with-locked-hash-table (*group-membership-invitations-index*)
      (asetf (gethash group *group-membership-invitations-index*)
             (remove (cons (caaar (getf invitation :people)) id)
                     it :test #'equal)))
    (remove-message-from-indexes id)
    (remove-from-db id)))

(defun confirm-group-uniqueness (results name lat long address city state country street zip membership-method &key public-location)
  (standard-page
    "Is your group already on Kindista?"
    (html
      (:div :class "item confirm-unique-group"
        (:h2 "Is your group already on Kindista?")
        (:p "It looks like your group might already on Kindista. "
         "Please do not create another profile for a group that is "
         "already on Kindista. ")
        (:p "If your group is listed below, please click the link to \"join\""
         "it on the group's profile page. "
         "If you believe you should included in the group's admins, "
         "please contact the current admin(s) and ask them to add you as an admin. "
         "Please "
         (:a :href "/contact-us" "let us know")
         " if you have any difficulty with this." )
         (:br)
        (str (groups-results-html results))
        (:form :method "post" :action "/groups/new"
            (:input :type "hidden" :name "name" :value (escape-for-html name))
            (:input :type "hidden" :name "lat" :value lat)
            (:input :type "hidden" :name "long" :value long)
            (:input :type "hidden" :name "location" :value (escape-for-html address))
            (:input :type "hidden" :name "city" :value (escape-for-html city))
            (:input :type "hidden" :name "state" :value state)
            (:input :type "hidden" :name "country" :value (escape-for-html country))
            (:input :type "hidden" :name "street" :value
             (awhen street (escape-for-html it)))
            (:input :type "hidden" :name "zip" :value zip)
            (:input :type "hidden" :name "membership-method" :value membership-method)
            (awhen public-location
              (htm (:input :type "hidden" :name "public-location" :value it)))
            (:button :class "cancel"
                     :type "submit"
                     :name "cancel"
                     "Nevermind, my group is alredy on Kindista!")
            (:button :class "yes"
                     :type "submit" :name "confirm-uniqueness" "My group is not already on Kindista, create a profile"))))))

(defun get-group (id)
  (ensuring-userid (id "/groups/~a")
    (let* ((group (db id))
           (group-adminp (find *userid* (getf group :admins))))

      (cond
        ((or (not group-adminp)
             (getf group :bio-summary)
             (getf group :bio-into)
             (getf group :bio-contact))
         (group-activity-html id))

        (group-adminp
         (see-other (strcat "/groups/" (username-or-id id) "/about")))

      (t (not-found))))))

(defun get-group-about (id)
  (ensuring-userid (id "/groups/~a/about")
    (profile-bio-html id)))

(defun get-group-activity (id)
  (ensuring-userid (id "/groups/~a/activity")
    (group-activity-html id)))

(defun get-group-reputation (id)
  (ensuring-userid (id "/groups/~a/reputation")
    (group-activity-html id :type :gratitude)))

(defun get-group-offers (id)
  (require-user (:allow-test-user t)
    (ensuring-userid (id "/groups/~a/offers")
      (group-activity-html id :type :offer))))

(defun get-group-requests (id)
  (require-user (:allow-test-user t)
    (ensuring-userid (id "/groups/~a/requests")
      (group-activity-html id :type :request))))

(defun get-group-members (id)
  (require-user (:allow-test-user t)
    (ensuring-userid (id "/groups/~a/members")
      (profile-group-members-html id))))

(defun get-invite-group-members (id)
  (ensuring-userid (id "/groups/~a/invite-members")
    (require-group-admin id
      (if (db id :active)
        (invite-group-members id)
        (progn
          (flash "This group has been deactivated. Please reactivate it from the group's settings page before inviting more members to join it." :error t)
          (see-other (url-compose "/settings/public"
                                  "groupid" id)))))))

(defun post-invite-group-members (groupid)
  (let* ((groupid (parse-integer groupid))
         (emails (remove-if
                   #'(lambda (email)
                       (gethash email *banned-emails-index*))
                   (remove-duplicates
                     (emails-from-string
                       (post-parameter "bulk-emails"))
                     :test #'string=)))
         (group (db groupid))
         (new-invitation-emails)
         (invitations-to-current-kindistas)
         (already-group-members) ; '((id . email) ...)
         (approved-member-ids)
         (new-invitation-member-ids)
         (resent-member-ids)
         (url (strcat "/groups/" (username-or-id groupid)))
         (next-url (or (post-parameter "next")
                       (url-compose (s+ url "/invite-members")
                                    "add-another" ""))))

    (awhen (post-parameter "invite-member")
       (if (find it (group-members groupid))
         (push (cons it nil) already-group-members)
         (push (parse-integer it)
               invitations-to-current-kindistas)))

    ;; figure out what kind of invitation to send
    (dolist (email emails)
      (aif (gethash email *email-index*)
        (if (find it (group-members groupid))
          (pushnew (cons it email) already-group-members
                   :test #'equal)
          (pushnew it invitations-to-current-kindistas))
        (push email new-invitation-emails)))

    (dolist (id invitations-to-current-kindistas)
      (acond
       ((assoc id
               (gethash groupid
                        *group-membership-invitations-index*))
        (resend-group-membership-request (cdr it))
        (push id resent-member-ids))

       ((assoc id
               (gethash groupid *group-membership-requests-index*))
        (approve-group-membership-request (cdr it))
        (push id approved-member-ids))

       (t
        (create-group-membership-invitation groupid id)
        (push id new-invitation-member-ids))))

    (awhen approved-member-ids
      (flash (strcat "You have approved " (name-list-all it) "'s "
                     (pluralize it "request" :hidenum t)
                     " to join " (getf group :name) ".")))

    (awhen resent-member-ids
      (flash (strcat (name-list-all it) "'s "
                     (if (> (length it) 1)
                       "invitation has" "invitations have")
                     " been resent." )))

    (awhen new-invitation-member-ids
      (flash (strcat (if (> (length it) 1)
                       "Invitations have" "An Invitation has")
                     " been sent to "
                     (name-list-all it)
                     ".")))

    (awhen already-group-members
      (flet ((id-details (data)
              (strcat* (person-link (car data))
                       (when (cdr data) ; when an email was included
                         (s+ " (" (cdr data) ") ")))))
        (flash (strcat (name-list it :func #'id-details
                                     :maximum-links 1000)
                       (if (> (length it) 1)
                         " are already members of "
                         " is already a member of ")
                       (getf group :name) ".")
               :error t)))

    (cond
     ((not new-invitation-emails)
      (see-other next-url))

     ((post-parameter "confirm")
      (let* ((duplicate-invites
               (remove-if-not #'(lambda (plist)
                                  (find (getf plist :email)
                                        new-invitation-emails
                                       :test #'equalp))
                              (unconfirmed-invites)))
             (new-invites (set-difference new-invitation-emails
                                          (mapcar #'(lambda (plist)
                                                      (getf plist :email))
                                                  duplicate-invites)
                                          :test #'equalp))
             (text (awhen (post-parameter-string "text")
                     (escape-for-html it))))

        (dolist (invite duplicate-invites)
          (resend-invitation (getf invite :id)
                             :groupid groupid
                             :text text))
        (dolist (email new-invites)
          (create-invitation email
                             :text text
                             :groups (list groupid))))

      (if (> (length new-invitation-emails) 1)
        (flash "Your invitations have been sent.")
        (flash "Your invitation has been sent."))
      (see-other next-url))

     ((post-parameter "try-again")
      (invite-group-members groupid :emails new-invitation-emails))

     (t
      (confirm-invitations :url (strcat "/groups/" groupid "/invite-members")
                           :emails new-invitation-emails
                           :bulk-emails (separate-with-commas
                                          new-invitation-emails)
                           :groupid groupid
                           :next-url next-url)))))

(defun post-group-members (groupid)
  (let* ((groupid (parse-integer groupid))
         (group (db groupid))
         (url (strcat "/groups/" (username-or-id groupid)))
         (next (or (post-parameter "next") url)))

    (cond
      ((post-parameter "request-membership")
       (if (eql (getf group :membership-method) :invite-only)
         (permission-denied)
         (unless (or (member *userid* (getf group :members))
                     (member *userid* (getf group :admins)))
           (let ((current-membership-request
                   (assoc *userid*
                          (gethash groupid
                                   *group-membership-requests-index*))))
             (aif current-membership-request
               (resend-group-membership-request (cdr it))
               (create-group-membership-request groupid)))))
       (flash (s+ "Your membership request has been forwarded to the "
                  (getf group :name)
                  " admins."))
       (see-other next))

      ((post-parameter "accept-group-membership-invitation")
       (aif (assoc *userid*
                   (gethash groupid *group-membership-invitations-index*))
         (progn
           (accept-group-membership-invitation (cdr it))
           (flash (s+ "You have joined " (getf group :name)))
           (see-other next))
         (permission-denied)))

      ((post-parameter "leave-group")
       (when (member *userid* (getf group :members))
         (confirm-action "Leave group"
                         (s+ "Are you sure you want to leave the group " (getf group :name) "?")
                         :url (strcat "/groups/" groupid "/members")
                         :next-url (referer)
                         :post-parameter "really-leave-group"
                         :details "If you leave this group, you will not be able to rejoin again without being re-approved by the group's admin.")))

      ((post-parameter "really-leave-group")
       (when (member *userid* (getf group :members))
         (remove-group-member *userid* groupid)
         (see-other url)))

      (t
       (require-group-admin groupid
         (cond
          ((post-parameter "approve-group-membership-request")
           (approve-group-membership-request
             (post-parameter-integer "membership-request-id"))
           (see-other (referer)))

          ((post-parameter "deny-group-membership-request")
           (delete-group-membership-request
             (post-parameter-integer "membership-request-id"))
           (see-other (referer)))))))))

(defun groups-tabs-html (&key (tab :my-groups))
  (html
    (:menu :type "toolbar"
           :class "bar"
           :id "groups-tabs"
      (:h3 :class "label" "Groups Menu")

      (if (eql tab :my-groups)
        (htm (:li :class "selected" "My Groups"))
        (htm (:li (:a :href "/groups/my-groups" "My Groups"))))

      (if (eql tab :contacts)
              (htm (:li :class "selected" "Contacts"))
              (htm (:li (:a :href "/groups/contacts" "Contacts"))))

      (if (eql tab :nearby)
        (htm (:li :class "selected" "Nearby"))
        (htm (:li (:a :href "/groups/nearby" "Nearby"))))

     ;(if (eql tab :suggested)
     ;  (htm (:li :class "selected" "Suggested"))
     ;  (htm (:li (:a :href "/people/suggested" "Suggested"))))

     ;(if (eql tab :invited)
     ;  (htm (:li :class "selected" "Invited"))
     ;  (htm (:li (:a :href "/people/invited" "Invited"))))
      )))

(defun post-existing-group (id)
  (require-active-user
    (let* ((id (parse-integer id))
           (group (db id))
           (url (strcat "/groups/" (username-or-id id))))

      (if (group-admin-p id)
        (cond

          ((post-parameter-string "group-category")
           (change-group-category id))

          ((post-parameter "membership-method")
           (modify-db id :membership-method (if (string= (post-parameter "membership-method")
                                                         "invite-only")
                                              :invite-only
                                              :group-admin-approval))
           (see-other (referer)))

          ((post-parameter "confirm-new-admin")
           (if (< (length (getf group :admins)) 4)
             (add-group-admin (parse-integer (post-parameter "item-id")) id)
             (flash (s+ "This account already has the maximum of four administrators. "
                        "If you would like to add an additional admin for this account, one "
                        "of the current admins must first revoke their admin privileges.")
                    :error t))
           (see-other (url-compose "/settings/admin-roles"
                                   "groupid" id)))

          ((post-parameter "confirm-revoke-admin-role")
           (if (member *userid* (getf group :admins))
             (progn
               (remove-group-admin *userid* id)
               (flash (s+ "You are no longer an admin for " (getf group :name) "'s account")))
             (permission-denied))
           (see-other url)))

        (permission-denied)))))


(defun get-users-groups (id)
  (let ((admin-and-member (gethash id *group-privileges-index*)))
    (union (getf admin-and-member :member)
           (getf admin-and-member :admin))))
