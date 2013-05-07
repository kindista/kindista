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
         (self (db invitation-id :self))
         (invite-request (db invitation-id :invite-request-id)))
   (cond
     (self
       ((send-email-verification invitation-id)))
     (invite-request
       (send-invite-request-response invitation-id))
     (t
      (send-invitation-email invitation-id)))))

(defun create-invitation (email &key text invitation-request-id (host *userid*) (self nil))
; self invitations are verifications for alternate email addresses
  (let* ((time (get-universal-time))
         (invitation (insert-db (list :type :invitation
                                      :host host
                                      :invite-request-id invitation-request-id
                                      :token (random-password 9)
                                      :self self
                                      :recipient-email email
                                      :text text
                                      :valid-until (+ time 2592000)))))

    (notice :new-invitation :time time :id invitation)
    invitation))

(defun index-invitation (id data)
  (let* ((host (getf (db id) :host)))
   (with-locked-hash-table (*person-invitation-index*) 
      (push id (gethash host *person-invitation-index*)))))

(defun delete-invitation (invitation-id)
  (let ((host (getf (db invitation-id) :host)))
    (with-locked-hash-table (*person-invitation-index*) 
      (asetf (gethash host *person-invitation-index*)
             (remove invitation-id it))) 
    (remove-from-db invitation-id)))

(defun add-alt-email (invitation-id)
  (let* ((invitation (db invitation-id))
         (userid (getf invitation :host))
         (email (getf invitation :recipient-email)))
    (amodify-db userid :emails (append it (list email))
                       :pending-alt-emails (remove invitation-id it))) 
    (delete-invitation invitation-id))

(defun unconfirmed-invitations (&optional (host *userid*))
  (let ((all-invites (gethash host *person-invitation-index*))
        (now (get-universal-time)))
    (iter (for id in all-invites)
          (unless (< (getf (db id) :valid-until) now)
            (awhen (getf (db id) :recipient-email)
              (collect it))))))

(defun invite-page (&key emails text next-url)
  (standard-page "Invite friends"
    (html
      (:div :class "item"
        (:form :method "post" :action "/invite"
          (:input :type "hidden" :name "next-url" :value (or next-url (referer)))
          (:h2 "Invite your friends to join Kindista!")
          (:p "You may enter multiple email addresses, separated by comas:")
          (:textarea :rows "3" :name "bulk-emails" :placeholder "Enter email addresses here..." (str (when emails emails)))
          (:p "Include a message for your recipient(s): (optional)")
          (:textarea :rows "6" :name "text" :placeholder "Enter your message here..." (str (when text text)))
          (:p 
            (:button :class "no" :type "submit" :class "cancel" :name "cancel" "Cancel") 
            (:button :class "yes" :type "submit" :class "submit" :name "review" "Next")))))))

(defun confirm-invitations (&key text emails bulk-emails next-url)
  (standard-page "Confirm invitation"
    (let* ((count (length emails))
           (pluralize (when (> count 1) "s")))
      (html
        (:div :class "item confirm-invite"
          (:form :method "post" :action "/invite"
            (:input :type "hidden" :name "next-url" :value next-url)
            (:input :type "hidden" :name "text" :value text)
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
                (str text)))
            (:p
              (:button :class "no" :type "submit" :class "submit" :name "edit" "Edit Invitation")
              (:button :class "yes" :type "submit" :class "submit" :name "confirm" "Send Invitation" (str pluralize)))))))))

(defun get-invite ()
  (require-user
    (invite-page)))

(defun post-invite ()
  (require-user
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
       (see-other (post-parameter "next-url")))))))

