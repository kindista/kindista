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

(defun inactive-people ()
  "diagnostic tool for admins only. inefficient; should not be used in code."
  (set-difference
    (loop for id in (hash-table-keys *db-results*)
          when (eq (result-type (gethash id *db-results*)) :person)
          collect id)
    *active-people-index*
    :test #'equal))

(defun create-person (&key name email password host (pending nil))
  (insert-db `(:type :person
               :name ,name
               :emails ,(list email)
               :host ,host
               :pending ,pending
               :active t
               :help t
               :pass ,(new-password password)
               :created ,(get-universal-time)
               :notify-gratitude t
               :notify-message t
               :notify-reminders t
               :notify-expired-invites t
               :notify-kindista t)))

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
          (setf (gethash email *email-index*) id))))

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
             (sort (push result it) #'> :key #'result-time)))

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
         (long (getf data :long)))

    (unless result
      (notice :error :note "no db result on ungeoindex-person"))

    (geo-index-remove *people-geo-index* result)
    (geo-index-remove *activity-geo-index* result)

    (setf (result-latitude result) lat)
    (setf (result-longitude result) long)

    (when (and (getf data :created)
               (getf data :active))

      (metaphone-index-insert (cons (getf data :name) (getf data :aliases)) result)

      (when (and lat long)

        (geo-index-insert *people-geo-index* result)

        (unless (< (result-time result) (- (get-universal-time) 15552000))
          (geo-index-insert *activity-geo-index* result))

        (dolist (id (gethash id *request-index*))
          (let ((result (gethash id *db-results*)))
            (geo-index-remove *request-geo-index* result)
            (geo-index-remove *activity-geo-index* result)
            (setf (result-latitude result) lat)
            (setf (result-longitude result) long)
            (unless (< (result-time result) (- (get-universal-time) 15552000))
              (geo-index-insert *activity-geo-index* result))
            (geo-index-insert *request-geo-index* result)))

        (dolist (id (gethash id *offer-index*))
          (let ((result (gethash id *db-results*)))
            (geo-index-remove *offer-geo-index* result)
            (geo-index-remove *activity-geo-index* result)
            (setf (result-latitude result) lat)
            (setf (result-longitude result) long)
            (geo-index-insert *offer-geo-index* result) 
            (unless (< (result-time result) (- (get-universal-time) 15552000))
              (geo-index-insert *activity-geo-index* result))))

        (dolist (id (gethash id *gratitude-index*))
          (let ((result (gethash id *db-results*)))
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

(defun deactivate-person (id)
  (let ((result (gethash id *db-results*)))
    (metaphone-index-insert (list nil) result)
    (geo-index-remove *people-geo-index* result)
    (geo-index-remove *activity-geo-index* result)
    (with-mutex (*active-people-mutex*)
      (asetf *active-people-index* (remove id it)))
    (dolist (request-id (gethash id *request-index*))
      (delete-inventory-item request-id))
    (dolist (offer-id (gethash id *offer-index*))
      (delete-inventory-item offer-id))
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

(defun delete-active-account (id &optional reason)
  (let ((data (db id)))
    (deactivate-person id)

    (awhen (getf data :emails)
      (with-locked-hash-table (*email-index*)
        (dolist (email it)
          (remhash email *email-index*))))

    (modify-db id :emails (list nil)
                  :type :deleted-person-account
                  :deleted t
                  :reason-for-account-deletion reason)))

(defun find-people-with-incorrect-communication-settings ()
  (sort (iter (for id in *active-people-index*)
          (let ((data (db id)))
            (when (or (not (getf data :notify-kindista))
                      (not (getf data :notify-reminders)))
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
           (flash (s+ "Your invitation to " email "  has been deleted."))
           (see-other "/people/invited"))
          ((post-parameter "resend")
           (resend-invitation-html id))
          ((post-parameter "cancel")
           (see-other "/people/invited"))
          ((post-parameter "confirm-resend")
           (resend-invitation id :text (awhen (post-parameter "text") it))
           (flash (strcat "Your invitation to " email "has been resent."))
           (see-other "/people/invited")))))))
