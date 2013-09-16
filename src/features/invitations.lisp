;;; Copyright 2012-2013 CommonGoods Network, Inc.
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

(defun send-invitation-notice-handler ()
  (let* ((invitation-id (getf (cddddr *notice*) :id))
         (invitation (db invitation-id))
         (host (getf invitation :host))
         (self (getf invitation :self)))
   (cond
     (self
       (send-email-verification invitation-id))
     ((eq host +kindista-id+)
      (send-requested-invite-email invitation-id))
     (t
      (send-invitation-email invitation-id)))))

(defun create-invitation (email &key text invite-request-id (expires (* 90 +day-in-seconds+)) (host *userid*) (self nil) name)
; self invitations are verifications for alternate email addresses
  (let* ((time (get-universal-time))
         (invitation (insert-db `(:type :invitation
                                  :host ,host
                                  :invite-request-id ,invite-request-id
                                  :token ,(random-password 9)
                                  :self ,self
                                  :recipient-email ,email
                                  :text ,text
                                  :name ,name
                                  :times-sent ,(list time)
                                  :valid-until ,(+ time expires)))))

    (notice :send-invitation :time time :id invitation)
    invitation))

(defun index-invitation (id data)
  (declare (ignore data))
  (let* ((invitation (db id))
         (host (getf invitation :host))
         (email (getf invitation :email))
         (sent (car (getf invitation :times-sent)))
         (token (getf invitation :token)))
    (with-locked-hash-table (*invitation-index*)
      (push (cons id token) (gethash email *invitation-index*)))
    (unless (getf invitation :auto-reminder-sent)
      (with-mutex (*invitation-reminder-timer-mutex*)
        (sort (push (cons sent id) *invitation-reminder-timer-index*) #'< :key #'car)))
    (with-locked-hash-table (*person-invitation-index*)
       (push id (gethash host *person-invitation-index*)))))

(defun delete-invitation (id)
  (let* ((invitation (db id))
         (type (getf invitation :type))
         (host (getf invitation :host))
         (email (getf invitation :email)))
    (when (eql type :invitation)
      (with-locked-hash-table (*invitation-index*)
        (let ((invitations (gethash email *invitation-index*)))
          (if (> (length invitations) 1)
            (asetf invitations (remove (assoc id invitations) it))
            (remhash email *invitation-index*))))
      (with-mutex (*invitation-reminder-timer-mutex*)
        (asetf *invitation-reminder-timer-index*
               (remove (rassoc id it)
                       it
                       :test #'equal)))
      (with-locked-hash-table (*person-invitation-index*)
        (asetf (gethash host *person-invitation-index*)
               (remove id it))) 
      (remove-from-db id))))

(defun resend-invitation (id &key text)
  (let ((now (get-universal-time))
        (sent (db id :times-sent)))
    (if text
      (amodify-db id :text text
                     :times-sent (push now it)
                     :expired-notice-sent nil
                     :valid-until (+ now (* 90 +day-in-seconds+)))
      (amodify-db id :times-sent (push now it)
                     :expired-notice-sent nil
                     :valid-until (+ now (* 90 +day-in-seconds+))))
    (with-mutex (*invitation-reminder-timer-mutex*)
      (asetf *invitation-reminder-timer-index*
             (remove (rassoc id it)
                     it
                     :test #'equal))
      (sort (push (cons (car sent) id) *invitation-reminder-timer-index*) #'< :key #'car) )
  (notice :send-invitation :time now :id id)))

(defvar *auto-invite-reminder-timer* nil)

(defun automatic-invitation-reminders ()
  (when *productionp*
    (setf *auto-invite-reminder-timer* (make-timer #'automatic-invitation-reminders))
    (loop for (time . id) in *invitation-reminder-timer-index*
        with now = (get-universal-time)
        while (< (+ (car (db id :times-time))
                    (* 5 +week-in-seconds+))
                 now)
        do (progn
             (send-automatic-invitation-reminder id)
               (with-mutex (*invitation-reminder-timer-mutex*)
                 (asetf *invitation-reminder-timer-index*
                        (remove (rassoc id it)
                                it
                                :test #'equal))))
        ; schedule timer for the first invitation that is less than 5 weeks old
        finally (schedule-timer *auto-invite-reminder-timer*
                                (+ time (* 5 +week-in-seconds+))
                                :absolute-p t))))

(defun send-automatic-invitation-reminder (id)
  (let ((invitation (db id))
        (now (get-universal-time)))
    (with-mutex (*invitation-reminder-timer-mutex*)
      (remove (rassoc id *invitation-reminder-timer-index*)
              *invitation-reminder-timer-index*
              :test #'equal))
    (case (getf invitation :host)
      (+kindista-id+
         (if (and (> id 4067)
                  (< id 4779))
           (send-prelaunch-invite-reminder id)
           (send-requested-invite-email id :auto-reminder t)))
      (t
        (send-invitation-email id :auto-reminder t)))
    (amodify-db id :times-sent (push now it)
                   :auto-reminder-sent now)))

(defun migrate-to-new-invitation-reminder-system ()
"helper function for migrating to new invitation-reminder-system"
  (add-notify-expired-invite-parameter-to-active-people)
  (modify-all-invitations-for-migration))

(defun add-notify-expired-invite-parameter-to-active-people ()
"helper function for migrating to new invitation-reminder-system"
  (dolist (id *active-people-index*)
    (modify-db id :notify-expired-invites t)))

(defun inefficient-invitations-list ()
"helper function for migrating to new invitation-reminder-system"
  (save-db)
  (let ((invitation-ids (list)))
    (maphash #'(lambda (key value)
                 (when (eql :invitation (getf value :type))
                   (push key invitation-ids)))
             *db*)
    invitation-ids))

(defun modify-all-invitations-for-migration ()
"helper function for migrating to new invitation-reminder-system"
  (dolist (id (inefficient-invitations-list))
    (let* ((invite (db id))
           (host (getf invite :host))
           (expires (getf invite :valid-until))
           (new-default-expiration (+ (get-universal-time) (* 6 +week-in-seconds+)))
           (sent (- expires
                    (* +day-in-seconds+
                       (if (and (eql host +kindista-id+) (> id 4776))
                          60
                          30)))))
      (if (< expires new-default-expiration)
        (modify-db id :times-sent (list sent)
                      :valid-until new-default-expiration)
        (modify-db id :times-sent (list sent))))))

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
"Returns an association list of (invite-id . recipient-email)."
  (let ((all-invites (gethash host *person-invitation-index*))
        (unconfirmed nil))
    (dolist (id all-invites)
      (let ((invitation (db id)))
        (awhen (getf invitation :recipient-email)
          (push (cons id it) unconfirmed ))))
    unconfirmed))

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
          (unless (getf invite-id :expired-notice-sent)
            (push (cons email invite-id) recently-expired-invites)))))
    recently-expired-invites))

(defun quick-invite-page (&key text)
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
            (:label "Email address")
            (:input :type "text" :name "email" :placeholder "Please enter your email address"))
          (:div 
            (:label "Confirm your email address")
            (:input :type "text" :name "confirm-email" :placeholder "Please confirm your email address"))
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

(defun confirm-invitations (&key text emails bulk-emails next-url)
  (standard-page "Confirm invitation"
    (let* ((count (length emails))
           (pluralize (when (> count 1) "s")))
      (html
        (:div :class "item confirm-invite"
          (:form :method "post" :action "/invite"
            (:input :type "hidden" :name "next-url" :value next-url)
            (:input :type "hidden" :name "text" :value (escape-for-html text))
            (:input :type "hidden" :name "bulk-emails" :value bulk-emails)
            (:h2 "Review your invitation" (str pluralize))
            (:h3 (str (strcat count " ")) "Recipient" (str pluralize) ":")
            (dolist (email emails)
              (htm (str email)
                   (:br)))
            (if (equal text "")
              (htm
                (:p "You did not include a personalized message to your invitation recipient" (str pluralize) "."))
              (htm
                (:h3 "Your personalized invitation message is:")
                (:blockquote :class "review-text" (str (html-text text)))))
            (:p
              (:button :class "cancel" :type "submit" :class "submit" :name "edit" "Edit Invitation")
              (:button :class "yes" :type "submit" :class "submit" :name "confirm" "Send Invitation" (str pluralize)))))))))

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
                                            (mapcar #'cdr already-invited) :test #'string=))
           (member-emails (loop for email in emails
                                when (gethash email *email-index*)
                                collect email))
           (new-emails (set-difference emails
                                       (union duplicate-invites member-emails :test #'string=)
                                       :test #'string=))
           (valid-emails (union new-emails duplicate-invites :test #'string=))
           (text (unless (string= (post-parameter "text") "")
                   (post-parameter "text"))))
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
       (invite-page :text (post-parameter "text")
                    :emails (post-parameter "bulk-emails")
                    :next-url next-url))

      ((post-parameter "review")
       (dolist (email member-emails)
         (flash (strcat email " is already a Kindista member!") :error t))
       (confirm-invitations :text (post-parameter "text")
                            :emails valid-emails
                            :bulk-emails (post-parameter "bulk-emails")
                            :next-url next-url))

      ((post-parameter "edit")
       (invite-page :text (post-parameter "text")
                    :emails (post-parameter "bulk-emails")
                    :next-url next-url))

      ((post-parameter "confirm")
       (dolist (email (union new-emails duplicate-invites :test #'string=))
         (create-invitation email :text text))
      ;(dolist (email duplicate-invites)
      ;  (let ((id (car (rassoc email already-invited))))
      ;    (modify-db id :text text)
      ;    (notice :send-invitation :id id)))
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
                          :text (unless (string= (post-parameter "text") "")
                                  (post-parameter "text")))
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
