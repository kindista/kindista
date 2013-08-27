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

(defun new-invitation-notice-handler ()
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

(defun create-invitation (email &key text invite-request-id (expires 2592000) (host *userid*) (self nil) name)
; self invitations are verifications for alternate email addresses
  (let* ((time (get-universal-time))
         (invitation (insert-db `(:type :invitation
                                  :host ,host
                                  :invite-request-id ,invite-request-id
                                  :token ,(random-invitation-token 9)
                                  :self ,self
                                  :recipient-email ,email
                                  :text ,text
                                  :name ,name
                                  :valid-until ,(+ time expires)))))

    (notice :new-invitation :time time :id invitation)
    invitation))

(defun index-invitation (id data)
  (declare (ignore data))
  (let* ((invitation (db id))
         (host (getf invitation :host))
         (token (getf invitation :token)))
   (with-locked-hash-table (*invitation-index*)
     (push id (gethash token *invitation-index*)))
   (with-locked-hash-table (*person-invitation-index*)
      (push id (gethash host *person-invitation-index*)))))

(defun delete-invitation (invitation-id)
  (let* ((invitation (db invitation-id))
         (host (getf invitation :host))
         (token (getf invitation :token)))
    (with-locked-hash-table (*invitation-index*)
      (remhash token *invitation-index*))
    (with-locked-hash-table (*person-invitation-index*)
      (asetf (gethash host *person-invitation-index*)
             (remove invitation-id it)))
    (remove-from-db invitation-id)))

(defun unique-invite-token-p (token)
  (not (member token (hash-table-keys *invitation-index*) :test #'string=)))

(defun random-invitation-token (length)
  (do ((new-token (random-password length) (random-password length)))
      ((unique-invite-token-p new-token) new-token)))

(defun add-alt-email (invitation-id)
  (let* ((invitation (db invitation-id))
         (userid (getf invitation :host))
         (email (getf invitation :recipient-email)))
    (amodify-db userid :emails (append it (list email))
                       :pending-alt-emails (remove invitation-id it))
    (with-locked-hash-table (*email-index*)
      (setf (gethash email *email-index*) *userid*)))
    (delete-invitation invitation-id))

(defun unconfirmed-invitations (&optional (host *userid*))
  (let ((all-invites (gethash host *person-invitation-index*))
        (now (get-universal-time))
        (valid-invitations nil)
        (expired-invitations nil))
    (dolist (id all-invites)
      (let ((invitation (db id)))
        (awhen (getf invitation :recipient-email)
          (cond
            ((member it (hash-table-keys *email-index*) :test #'string=)
             (delete-invitation id))
            ((> (getf invitation :valid-until) now)
             (pushnew (cons id it) valid-invitations :test #'equal))
            (t (pushnew (cons id it) expired-invitations :test #'equal))))))
    (values expired-invitations valid-invitations)))

(defun emails-awaiting-rsvp ()
  (let ((kindista-invites nil)
        (user-invites nil))
    (dolist (invite-id (hash-table-values *invitation-index*))
      (let* ((invite (db (car invite-id)))
              (email (getf invite :recipient-email)))
        (unless (member email (hash-table-keys *email-index*) :test #'string=)
          (case (getf invite :host)
            (2 (pushnew email kindista-invites :test #'string=))
            (t (pushnew email user-invites :test #'string=))))))
    (values kindista-invites
            user-invites
            (length kindista-invites)
            (length user-invites))))

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
            (:button :class "cancel" :type "submit" :class "cancel" :name "cancel" "Cancel") 
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
           (member-emails (iter (for email in emails) 
                                (awhen (gethash email *email-index*)
                                  (collect email))))
           (new-emails (set-difference emails
                                       member-emails
                                       :test #'string=)))
    (cond
      ((getf *user* :pending)
       (pending-flash "invite people to join Kindista")
       (see-other (or next-url "/home")))

      ((post-parameter "cancel")
       (see-other next-url))

      ((and (post-parameter "review")
            (not new-emails))
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
                            :emails new-emails 
                            :bulk-emails (post-parameter "bulk-emails")
                            :next-url next-url))

      ((post-parameter "edit")
       (invite-page :text (post-parameter "text")
                    :emails (post-parameter "bulk-emails")
                    :next-url next-url))

      ((post-parameter "confirm")
       (dolist (email new-emails)
         (create-invitation email :text (unless
                                          (string= (post-parameter "text") "")
                                          (post-parameter "text"))))
       (if (> (length new-emails) 1)
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

