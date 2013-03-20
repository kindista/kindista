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

(defun create-invitations (&key (count 1) (host *userid*))
  (iter (for i from 1 to count) 
        (insert-db (list :type :invitation
                         :host host
                         :recipient-email nil
                         :text nil
                         :valid-until (get-universal-time)))))

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

(defun address-invitation (id &key recipient-email text)
  (modify-db id :recipient-email recipient-email 
                :text text
                :valid-until (+ (get-universal-time) 2592000)))

(defun available-invitations (host)
  (let ((all-invites (gethash host *person-invitation-index*))
        (now (get-universal-time)))
    (iter (for id in all-invites) 
          (unless (> (getf (db id) :valid-until) now)
            (collect id)))))

(defun unconfirmed-invitations (host)
  (let ((all-invites (gethash host *person-invitation-index*))
        (now (get-universal-time)))
    (iter (for id in all-invites) 
          (unless (< (getf (db id) :valid-until) now)
            (awhen (getf (db id) :recipient-email) 
              (collect it))))))

(defun send-invitation (recipient-email &key (host *userid*) text)
  (let* ((available-invites (available-invitations host))
         (invitation (car available-invites)))
    (if available-invites
      (progn 
        (address-invitation invitation :recipient-email recipient-email
                                       :text text)
        (send-invitation-email invitation)
        invitation)
      (flash "You do not have any invitations at this time." :error t))))


(defun available-invitation-count (host)
 (length (available-invitations host)))

(defun invite-page (&key emails available-count text next-url pluralize)
  (standard-page "Invite friends"
    (html
      (:div :class "item"
        (:form :method "post" :action "/invite"
          (:input :type "hidden" :name "next-url" :value (or next-url (referer)))
          (:h2 "Invite your friends to join Kindista!")
          (:p "You have " 
              (:strong (str available-count)) 
              " invitation" (str pluralize)
              " available at this time."
           (:br)
           "You may enter multiple email addresses, separated by comas:")
          (:textarea :rows "3" :name "bulk-emails" :placeholder "Enter email addresses here..." (str (when emails emails)))
          (:p "Include a message for your recipients: (optional)")
          (:textarea :rows "6" :name "text" :placeholder "Enter your message here..." (str (when text text)))
          (:p 
            (:button :class "no" :type "submit" :class "cancel" :name "cancel" "Cancel") 
            (:button :class "yes" :type "submit" :class "submit" :name "review" "Next")))))))

(defun confirm-invitations (&key text emails bulk-emails next-url)
  (standard-page "Confirm invitation"
    (let ((pluralize (when (> (length emails) 1) "s")))
      (html
        (:div :class "item"
          (:form :method "post" :action "/invite"
            (:input :type "hidden" :name "next-url" :value next-url)
            (:input :type "hidden" :name "text" :value text)
            (:input :type "hidden" :name "bulk-emails" :value bulk-emails)
            (:h2 "Review your invitation")
            (:h3 "Recipient" (str pluralize) ":")
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
              (:button :class "yes" :type "submit" :class "submit" :name "confirm" "Send Invitation"))))))))

(defun get-invite-page ()
  (with-user
    (let* ((available-count (available-invitation-count *userid*))
           (pluralize (when (> available-count 1) "s")))
    (cond 
      (available-count
        (invite-page :available-count available-count
                     :pluralize pluralize))
      (t
       (flash "You do not have any available invitations at this time." :error t)
       (see-other "/home"))))))

(defun post-invite-page ()
  (with-user
    (let* ((next-url (post-parameter "next-url"))
           (available-count (available-invitation-count *userid*))
           (emails (remove-duplicates 
                     (emails-from-string (post-parameter "bulk-emails"))
                     :test #'string=))
           (pluralize (when (> available-count 1) "s")))
    (cond
      ((not (> available-count 0))
       (flash "You do not have any available invitations at this time." :error t)
       (see-other next-url))

      ((post-parameter "cancel")
       (see-other next-url))

      ((and (post-parameter "review")
            (not emails))
       (flash "You must enter at least 1 valid email address." :error t)
       (invite-page :text (post-parameter "text")
                    :emails (post-parameter "bulk-emails")
                    :available-count available-count
                    :pluralize pluralize
                    :next-url next-url))

      ((and (post-parameter "review")
            (> (length emails) available-count))
       (flash (strcat "You have entered too many email addresses. You only have " 
                      available-count 
                      " invitation" pluralize
                      " available at this time.")
              :error t)
       (invite-page :text (post-parameter "text")
                    :emails (post-parameter "bulk-emails")
                    :available-count available-count
                    :pluralize pluralize
                    :next-url next-url))

      ((post-parameter "review")
       (confirm-invitations :text (post-parameter "text")
                            :emails emails 
                            :bulk-emails (post-parameter "bulk-emails")
                            :next-url next-url))

      ((post-parameter "edit")
       (invite-page :text (post-parameter "text")
                    :emails (post-parameter "bulk-emails")
                    :available-count available-count
                    :pluralize pluralize
                    :next-url next-url))

      ((post-parameter "confirm")
       (dolist (email emails)
         (send-invitation email :text (post-parameter "text")))
       (if (> (length emails) 1)
         (flash "Your invitations have been sent.")
         (flash "your invitaion has been sent."))
       (see-other (post-parameter "next-url")))))))

