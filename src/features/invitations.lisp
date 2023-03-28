;;; Copyright 2012-2015 CommonGoods Network, Inc.
;;;
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

(defun find-first-malicious-signup ()
  "This funcion is no longer needed and can be deleted. Kindista suffered a malicious attack on 3/26/2023. Someone spammed the signup page. This function was used to find the malicious invites."
  (let* ((kindista-invite-ids (gethash +kindista-id+ *person-invitation-index*))
         (now (get-universal-time))
         (attack-start (- now (* 4 +day-in-seconds+)))
         (attack-invites)
         (first-attack-invite (cons 1000000 (+ now +year-in-seconds+))))
    (dolist (id kindista-invite-ids)
      (let* ((invite (db id))
             (invite-date (getf invite :valid-until)))
        (when (> invite-date attack-start)
          (push id attack-invites)
          (when (and (< id (car first-attack-invite))
                     (not (> invite-date (cdr first-attack-invite))))
            (setf first-attack-invite (cons id  invite-date))
            ))))
    (values first-attack-invite
            (humanize-exact-time (cdr first-attack-invite) :detailed t))))

(defun send-invitation-notice-handler ()
  (let* ((invitation-id (getf (cddddr *notice*) :id))
         (new-group (getf (cddddr *notice*) :new-group))
         (new-gratitude (getf (cddddr *notice*) :new-gratitude))
         (invitation (db invitation-id))
         (host (getf invitation :host))
         (self (getf invitation :self)))
   (cond
     (self
       (send-email-verification invitation-id))
     ((eq host +kindista-id+)
      (send-requested-invite-email invitation-id))
     (t
      (send-invitation-email invitation-id
                             :new-group new-group
                             :new-gratitude new-gratitude)))))

(defun create-invitation
  (email
   &key text
        invite-request-id
        groups
        (expires (* 90 +day-in-seconds+))
        (host *userid*)
        self ;self invitations are verifications for alternate email addresses
        name
        fb-id
        fb-token
        fb-expires
        gratitude-id
   &aux (time (get-universal-time))
        (invitation (insert-db (list :type :invitation
                                     :host host
                                     :invite-request-id invite-request-id
                                     :token (random-password 9)
                                     :self self
                                     :groups groups
                                     :gratitudes (when gratitude-id
                                                   (list gratitude-id))
                                     :recipient-email email
                                     :text text
                                     :name name
                                     :times-sent (list time)
                                     :fb-id fb-id
                                     :fb-token fb-token
                                     :fb-expires fb-expires
                                     :valid-until (+ time expires)))))
    (notice :send-invitation :time time
                             :id invitation
                             :new-group (car groups)
                             :new-gratitude gratitude-id)
    invitation)

(defun index-invitation (id data)
  (declare (ignore data))
  (let* ((invitation (db id))
         (host (getf invitation :host))
         (email (getf invitation :recipient-email))
         (sent (car (getf invitation :times-sent)))
         (token (getf invitation :token)))
    (with-locked-hash-table (*invitation-index*)
      (push (cons id token) (gethash email *invitation-index*)))

    (unless (getf invitation :auto-reminder-sent)
      (with-mutex (*invitation-reminder-timer-mutex*)
        (setf *invitation-reminder-timer-index*
              (safe-sort (push (cons sent id) *invitation-reminder-timer-index*)
                         #'<
                         :key #'car))))

    (with-locked-hash-table (*person-invitation-index*)
       (push id (gethash host *person-invitation-index*)))))

(defun delete-invitation (id)
  (let* ((invitation (db id))
         (type (getf invitation :type))
         (host (getf invitation :host))
         (email (getf invitation :recipient-email)))
    (when (eql type :invitation)
      (with-locked-hash-table (*invitation-index*)
        (if (> (length (gethash email *invitation-index*)) 1)
          (asetf (gethash email *invitation-index*)
                 (remove (assoc id it) it))
          (remhash email *invitation-index*)))
      (with-mutex (*invitation-reminder-timer-mutex*)
        (asetf *invitation-reminder-timer-index*
               (remove (rassoc id it)
                       it
                       :test #'equal)))
      (with-locked-hash-table (*person-invitation-index*)
        (asetf (gethash host *person-invitation-index*)
               (remove id it)))
      (remove-from-db id))))

(defun resend-invitation (id &key text groupid invitee-name new-gratitude-id)
  (let* ((now (get-universal-time))
         (invite-data (db id))
         (invite-name (if (> (length invitee-name)
                             (length (getf invite-data :name)))
                        invitee-name
                        (getf invite-data :name)))
         (invite-gratitudes (remove nil
                                    (aif (getf invite-data :gratitudes)
                                      (cons new-gratitude-id it)
                                      (list new-gratitude-id))))
         (invite-groups (copy-list (getf invite-data :groups)))
         (invite-times-sent (getf invite-data :times-sent))
         (invite-parameters (list :valid-until (+ now (* 90 +day-in-seconds+))
                                  :expired-notice-sent nil
                                  :gratitudes invite-gratitudes
                                  :name invite-name
                                  :times-sent (cons now invite-times-sent))))
    (awhen text (appendf invite-parameters invite-parameters (list :text it)))
    (when groupid (appendf invite-parameters
                           invite-parameters
                           (list :groups (remove nil
                                                 (if invite-groups
                                                     (pushnew groupid
                                                              invite-groups)
                                                     (list groupid))))))

    (apply #'modify-db id invite-parameters)

    (with-mutex (*invitation-reminder-timer-mutex*)
      (asetf *invitation-reminder-timer-index*
             (remove (rassoc id it)
                     it
                     :test #'equal))
      (safe-sort (push (cons now id) *invitation-reminder-timer-index*) #'<
                  :key #'car))
  (notice :send-invitation :time now
                           :id id
                           :new-group groupid
                           :new-gratitude new-gratitude-id))
  id)

(defun get-automatic-invitation-reminders ()
  (when (or *productionp*
            ; only use on the live server or with test data
            (< (length *invitation-reminder-timer-index*) 10))
    (loop for (time . id) in *invitation-reminder-timer-index*
        with now = (get-universal-time)
        while (< (+ (car (db id :times-sent))
                    (* 5 +week-in-seconds+))
                 now)
        do (progn
             (send-automatic-invitation-reminder id)
               (with-mutex (*invitation-reminder-timer-mutex*)
                 (asetf *invitation-reminder-timer-index*
                        (remove (rassoc id it)
                                it
                                :test #'equal)))))))

(defun send-automatic-invitation-reminder (id)
  (let ((invitation (db id))
        (now (get-universal-time)))
    (with-mutex (*invitation-reminder-timer-mutex*)
      (remove (rassoc id *invitation-reminder-timer-index*)
              *invitation-reminder-timer-index*
              :test #'equal))
    (if (eql (getf invitation :host) +kindista-id+)
      (send-requested-invite-email id :auto-reminder t)
      (send-invitation-email id :auto-reminder t))
    (amodify-db id :times-sent (push now it)
                   :auto-reminder-sent (push now it))))

(defun pending-email-actions
  (email
   &key (userid *userid*)
   &aux (user (db userid)))
  (let ((group-invitations-sent nil))
    (dolist (invitation-id (mapcar #'car (gethash email *invitation-index*)))
      (let* ((invitation (db invitation-id))
             (host-id (getf invitation :host))
             (invitee-name (getf invitation :name))
             (groups (getf invitation :groups)))
        ;; send group invites for invitations from groups
        ;; that the new user never replied to
        (dolist (groupid groups)
          (unless (or (find groupid group-invitations-sent)
                      (find userid (gethash groupid *group-members-index*)))
            (aif (assoc userid
                        (gethash groupid
                                 *group-membership-invitations-index*))
                 (resend-group-membership-invitation (cdr it))
                 (create-group-membership-invitation groupid
                                                     userid
                                                     :host host-id))
            (asetf group-invitations-sent (push groupid it))))
        ;; see if the name on the invitation is one of the user's
        ;; aliases.  if not, add it.
        (unless
          (loop for name in (cons (getf user :name) (getf user :aliases))
                when (search (getf invitation :name) name :test #'equalp)
                collect name)
          (amodify-db userid :aliases (cons invitee-name it))
          (reindex-person-names userid))
        ;; see if anyone posted gratitude before the person joined
        (dolist (gratitude-id (getf invitation :gratitudes))
          (index-gratitude gratitude-id
                           (modify-db gratitude-id
                                      :subjects (list userid)
                                      :people `(((,userid) . :unread))
                                      :message-folders `(:inbox (,userid))
                                      :pending nil)))
        ;; add new user to contacts of others who had invited them
        (unless (or (find host-id (list *userid* +kindista-id+))
                    (find userid (db host-id :following)))
          (add-contact userid host-id)))
      (delete-invitation invitation-id))))

(defun add-alt-email (invitation-id)
  (let* ((invitation (db invitation-id))
         (userid (getf invitation :host))
         (email (getf invitation :recipient-email)))
    (amodify-db userid :emails (append it (list email))
                       :pending-alt-emails (remove invitation-id it))
    (with-locked-hash-table (*email-index*)
      (setf (gethash email *email-index*) *userid*)))
    (delete-invitation invitation-id))

(defun unconfirmed-invites (&optional (host *userid*))
"Returns a property list of (:id :email :times-sent :last-sent :expired)."
  (let ((all-invites (gethash host *person-invitation-index*))
        (unconfirmed (list)))
    (dolist (id all-invites)
      (let ((invitation (db id)))
        (awhen (getf invitation :recipient-email)
          (push (list :id id
                      :email (getf invitation :recipient-email)
                      :times-sent (getf invitation :times-sent)
                      :last-sent (car (getf invitation :times-sent))
                      :expired (awhen (getf invitation :valid-until)
                                 (when (< it (get-universal-time)) it)))
                unconfirmed))))
    (sort unconfirmed
          #'<
          :key #'(lambda (item) (getf item :last-sent)))))

(defun find-invitation-id-by-host (host-id invitation-email)
  "For finding an invitation-id for a given host-id/invitation-email pair. More efficient than calling unconfirmed-invites."
  (car (remove nil
               (mapcar #'(lambda (invite)
                           (find (car invite)
                                 (gethash host-id *person-invitation-index*)))
                       (gethash invitation-email
                                *invitation-index*)))))


(defun find-duplicate-invitations (&optional (host *userid*))
 "Finds duplicate invites from this host. Deletes invitations to any current Kindista members."
  (let* ((all-invites (gethash host *person-invitation-index*))
         (duplicates (make-hash-table :test #'equalp)))
    (dolist (id all-invites)
      (let* ((invitation (db id))
             (email (getf invitation :recipient-email)))
        (if (member email (hash-table-keys *email-index*) :test #'string=)
          (delete-invitation id)
          (asetf (gethash email duplicates)
                 (push (list :id id
                             :sent (car (last (getf invitation :times-sent))))
                       it)))))
    (maphash #'(lambda (key value)
                 (when (< (length value) 2)
                   (remhash key duplicates)))
             duplicates)
    duplicates))

(defun delete-duplicate-invitations (&optional (host *userid*))
"Deletes older duplicate invites from this host."
  (let* ((duplicates (copy-hash-table (find-duplicate-invitations host))))
    (labels ((sent-date (invite) (getf invite :sent))
             (ids (plist) (mapcar #'(lambda (invite)
                                      (getf invite :id))
                                  plist))
             (sort-invites (invites)
              (sort invites #'> :key #'sent-date))
             (delete-extra-invites (key value)
               (declare (ignore key))
               (dolist (id (cdr (ids (sort-invites value))))
                 (delete-invitation id))))
      (maphash #'delete-extra-invites duplicates))))

(defun delete-all-duplicate-invitations ()
  (dolist (id *active-people-index*)
    (delete-duplicate-invitations id)))

(defun invitee-count (userid)
  (+ (or (length (gethash userid *person-invitation-index*)) 0)
     (or (length (gethash userid *invited-index*)) 0)))

(defun kindista-invite-expirations ()
  (let ((invite-dates (list))
        (kindista-members (hash-table-keys *email-index*)))
    (dolist (invite-id (gethash +kindista-id+ *person-invitation-index*))
      (let* ((invite (db invite-id))
             (email (getf invite :recipient-email))
             (expires (getf invite :valid-until)))
        (unless (member email kindista-members :test #'string=)
          (if (assoc expires invite-dates)
            (push invite-id (cdr (assoc expires invite-dates)))
            (setf invite-dates (push (cons expires (list invite-id)) invite-dates))))))
    invite-dates))

(defun kindista-invite-humanized-expirations ()
  (mapcar #'(lambda (item)
              (when item
                (cons (humanize-exact-time (car item) :year-first t) (cdr item))))
          (kindista-invite-expirations)))

(defun expired-invitations (userid)
  (let ((expired-invites (list)))
    (dolist (invite-id (gethash userid *person-invitation-index*))
      (let* ((invite (db invite-id))
             (email (getf invite :recipient-email))
             (expired (< (getf invite :valid-until) (get-universal-time))))
        (when expired (push (cons email invite-id) expired-invites))))
    expired-invites))

(defun recently-expired-invitations (userid)
"Returns an association list of (email . invite-id) of each invitation for which the user
 has not yet been notified of its expiration."
  (let ((recently-expired-invites (list)))
    (dolist (invite-id (gethash userid *person-invitation-index*))
      (let* ((invite (db invite-id))
             (email (getf invite :recipient-email))
             (expired (< (getf invite :valid-until) (get-universal-time))))
        (when expired 
          (unless (getf invite :expired-notice-sent)
            (push (cons email invite-id) recently-expired-invites)))))
    recently-expired-invites))

(defun quick-invite-page (&key text)
  "for use at special events. normally not enabled."
  (standard-page "Send invitations"
    (html
      (:div :class "item create-event"
        (:form :method "post" :action "/invite"
          (:input :type "hidden" :name "text" :value text)
          (:h2 "Sign up for Kindista!")
          (:p "Please enter your email address below. "
           "We will send you an invitation email to complete the signup process. "
           "If you do not see the invitation in your inbox, please check your spam folder.")
          (:div
            (:label :for "new-account-email" "Email address")
            (:input :id "new-account-email" :type "text" :name "email" :placeholder "Please enter your email address"))
          (:div 
            (:label :for "new-account-email-confirmation"
             "Confirm your email address")
            (:input :id "new-account-email-confirmation"
                    :type "text"
                    :name "confirm-email"
                    :placeholder "Please confirm your email address"))
          (:p
            (:button :class "cancel" :type "submit" :class "cancel" :name "cancel" "Cancel")
            (:button :class "yes" :type "submit" :class "submit" :name "quick-invite" "Sign me up!")))))))

(defun invite-page (&key emails text next-url)
  (standard-page "Invite friends"
    (html
      (:div :class "item"
        (:form :method "post" :action "/invite"
          (:input :type "hidden" :name "next-url" :value (or next-url (referer)))
          (:h2 "Invite your friends to join Kindista!")
          (:p "You may enter multiple email addresses, separated by commas:")
          (:textarea :rows "3" :name "bulk-emails" :placeholder "Enter email addresses here..." (str (when emails emails)))
          (:p "Include a message for your recipient(s): (optional)")
          (:textarea :rows "6" :name "text" :placeholder "Enter your message here..." (str (when text text)))
          (:p
            (:button :type "submit" :class "cancel" :name "cancel" "Cancel")
            (:button :class "yes" :type "submit" :class "submit" :name "review" "Next")))))))

(defun confirm-invitations (&key url text emails bulk-emails next-url groupid)
  (standard-page "Confirm invitation"
    (let* ((count (length emails))
           (extra-s (when (> count 1) "s")))
      (html
        (:div :class "item confirm-invite"
          (:form :method "post" :action (or url "/invite")
            (:input :type "hidden" :name "next-url" :value next-url)
            (when text
              (htm (:input :type "hidden" :name "text" :value (escape-for-html text))))
            (:input :type "hidden" :name "groupid" :value groupid)
            (:input :type "hidden" :name "bulk-emails" :value bulk-emails)
            (:h2 "Review your invitation" (str extra-s))
            (:h3 (str (strcat count " ")) "Recipient" (str extra-s) ":")
            (dolist (email emails)
              (htm (str email)
                   (:br)))
            (if groupid
              (htm
                (:p "Include a message for your recipient(s): (optional)")
                (:textarea :rows "6"
                           :name "text"
                           :placeholder "Enter your message here..."
                           (when text (str text))))
              (if (equal text "")
                (htm
                  (:p "You did not include a personalized message to your invitation recipient" (str extra-s) "."))
                (htm
                  (:h3 "Your personalized invitation message is:")
                  (:blockquote :class "review-text" (str (html-text text))))))
            (:p
              (if groupid
                (htm (:button :class "cancel submit" :type "submit" :name "try-again" "Go Back"))
                (htm (:button :class "cancel submit" :type "submit" :name "edit" "Edit Invitation")))
              (:button :class "yes" :type "submit" :class "submit" :name "confirm" "Send Invitation" (str extra-s)))))))))

(defun get-invite ()
  (require-active-user
    (cond
      ((getf *user* :pending)
       (pending-flash "invite people to join Kindista")
       (see-other (or (referer) "/home"))) 
     ;((eq *userid* 7222)
     ; (quick-invite-page :text "Thanks for signing up for Kindista at Beloved."))
      (t (invite-page :emails (get-parameter "email"))))))

(defun post-invite ()
  (require-active-user
    (let* ((next-url (post-parameter "next-url"))
           (emails (remove-duplicates
                     (emails-from-string (post-parameter "bulk-emails"))
                     :test #'string=))
           (already-invited (unconfirmed-invites))
           (duplicate-invites (intersection emails
                                            (mapcar #'(lambda (invite)
                                                        (getf invite :email))
                                                    already-invited)
                                            :test #'string=))
           (member-emails (loop for email in emails
                                when (gethash email *email-index*)
                                collect email))
           (new-emails (set-difference emails
                                       (union duplicate-invites member-emails :test #'string=)
                                       :test #'string=))
           (valid-emails (union new-emails duplicate-invites :test #'string=))
           (text (post-parameter-string "text")))
    (cond
      ((getf *user* :pending)
       (pending-flash "invite people to join Kindista")
       (see-other (or next-url "/home")))

      ((post-parameter "cancel")
       (see-other next-url))

      ((and (post-parameter "review")
            (not (or already-invited new-emails)))
       (dolist (email member-emails)
         (flash (strcat email " is already a Kindista member!") :error t))
       (flash "You must enter at least 1 valid email address." :error t)
       (invite-page :text text
                    :emails (post-parameter "bulk-emails")
                    :next-url next-url))

      ((post-parameter "review")
       (dolist (email member-emails)
         (flash (strcat email " is already a Kindista member!") :error t))
       (confirm-invitations :text text
                            :emails valid-emails
                            :bulk-emails (post-parameter "bulk-emails")
                            :next-url next-url))

      ((post-parameter "edit")
       (invite-page :text text
                    :emails (post-parameter "bulk-emails")
                    :next-url next-url))

      ((post-parameter "confirm")
       (dolist (email new-emails)
         (create-invitation email :text text))
       (dolist (email duplicate-invites)
         (let* ((invite (find email already-invited
                              :key #'(lambda (item) (getf item :email))
                              :test #'string=))
                (id (getf invite :id)))
           (resend-invitation id :text text)))
       (if (> (+ (length duplicate-invites) (length new-emails)) 1)
         (flash "Your invitations have been sent.")
         (flash "Your invitation has been sent."))
       (see-other (post-parameter "next-url")))

      ((not (scan +email-scanner+ (post-parameter "email")))
       (flash "Please enter a valid email address." :error t)
       (see-other "/invite"))

      ((gethash (post-parameter "email") *email-index*)
       (flash (strcat (post-parameter "email") "is already a Kindista member!") :error t)
       (see-other "/invite"))

      ((not (string= (post-parameter "email") (post-parameter "confirm-email")))
       (flash "Your confirmation email does not match the email address you have entered." :error t)
       (see-other "/invite"))

      ((post-parameter "quick-invite")
       (create-invitation (post-parameter "email")
                          :text text)
       (flash (strcat "An invitation has been sent to "

                      (post-parameter "email")))
       (see-other "invite"))))))

(defun resend-invitation-html (id)
  (let* ((invitation (db id))
         (email (getf invitation :recipient-email))
         (message (getf invitation :text)))
    (standard-page
      "Resend an invitation"
      (html
        (:div :class "item"
          (:form :method "post" :action "/people/invited"
            (:input :type "hidden" :name "invite-id" :value id)
            (:h2 (str (s+ "Resend your invitation to " email)))
            (if message
              (htm (:p "Edit your message for this invitation (optional)."))
              (htm (:p "Include a personal message with this invitation (optional).")))
            (:textarea :rows "6" :name "text" :placeholder "Enter your message here..."
             (str (awhen message it)))

            (:p
              (:button :type "submit" :class "cancel" :name "cancel" "Cancel") 
              (:button :class "yes" :type "submit" :class "submit" :name "confirm-resend" "Resend"))))))))
