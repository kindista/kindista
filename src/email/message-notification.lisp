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

(defun send-comment-notification-email (comment-id)
  (let* ((comment (db comment-id)) ; get the comment
         (on-id (getf comment :on))
         (on-item (db on-id))  ; get the conversation
         (on-type (getf on-item :type)) ; :converation or :transaction
         (participants (getf on-item :participants))
         (inventory-item (db (getf on-item :on))) ; when transaction, get inventory item
         (inventory-type (if (eq (getf inventory-item :type) :request)
                           "request" "offer"))
         (inventory-text (or (getf inventory-item :title)
                             (getf inventory-item :details)
                             (getf on-item :deleted-item-title)
                             (getf on-item :deleted-item-details)
                             (getf on-item :deleted-item-text)))
         (sender-id (car (getf comment :by)))
         (sender-group-id (cdr (getf comment :by)))
         (sender-group (db sender-group-id))
         (sender (db sender-id))
         (sender-name (getf sender :name))
         (inventory-poster (getf inventory-item :by))
         ;; get an a list of (person-id . group-id)
         (people-boxes (mapcar #'car (getf on-item :people)))
         (recipient-boxes (remove-if #'(lambda (box)
                                         (eql sender-id (car box)))
                                     people-boxes))
         ;;get a list of '(((person-id) . recipient-data) ...)
         ;;for boxes without group-ids as cdr
         (recipient-people (mapcar #'(lambda (box) (cons box (db (car box))))
                                   (remove-if #'cdr recipient-boxes)))
         ;;remove people who don't want notification emails
         (valid-recipient-people (remove-if-not #'(lambda (recipient)
                                                    (getf (cdr recipient)
                                                          :notify-message))
                                                recipient-people))
         (group-ids (remove-duplicates
                      (remove nil (mapcar #'cdr recipient-boxes))))
         (valid-recipient-group-admin-boxes
           (apply #'append
                  (mapcar #'(lambda (groupid)
                              (let ((group (db groupid))
                                    (admin-boxes))
                                ;; make sure it's actually a group
                                (when (eql (getf group :type) :group)
                                  (dolist (id (getf group :notify-message))
                                    (push (cons id groupid) admin-boxes))
                                  admin-boxes)))
                          group-ids)))
         (valid-recipient-group-admins
           (mapcar #'(lambda (box) (cons box (db (car box))))
                   ;; don't send mult. mess. to admins of more than 1 group
                   (remove-duplicates
                     ;; don't send 2 messages if person is a recipient
                     ;; and also an admin of a group that is a recipient
                     (remove-if #'(lambda (person-id)
                                    (or (eql person-id sender-id)
                                        (find person-id
                                              (mapcar #'caar
                                                  valid-recipient-people))))
                                valid-recipient-group-admin-boxes
                                :key #'car)
                     :key #'car)))
         (all-recipients (append valid-recipient-people
                                 valid-recipient-group-admins))
         (deleted-item-type (getf on-item :deleted-item-type))
         (text (if (and deleted-item-type
                        (eql comment-id (apply #'min (gethash on-id *comment-index*))))
                 (deleted-invalid-item-reply-text
                   (db (car (remove sender-id participants)) :name)
                   sender-name
                   deleted-item-type
                   (getf comment :text))
                 (getf comment :text))))

    (flet ((subject-text (groupid)
             (if (eq on-type :transaction)
               (if (eql sender-id inventory-poster)
                 (s+ sender-name " has replied to your question about their " inventory-type ":")
                 (s+ sender-name " has replied to "
                     (aif groupid
                       (s+ (db it :name) "'s ")
                       "your ")
                     inventory-type ":"))
               (or (getf on-item :subject)
                   (s+ "New message from " sender-name)))))

      (dolist (recipient all-recipients)
        (let* ((groupid (cdar recipient))
               (person (cdr recipient))
               (email (car (getf person :emails)))
               (unsub-key (getf person :unsubscribe-key))
               (people (name-list (remove (caar recipient)
                                          participants)
                                  :func #'person-name
                                  :maximum-links 5))
               (subject (subject-text groupid)))

          (cl-smtp:send-email
            +mail-server+
            "PleaseDoNotReply <noreply@kindista.org>"
            email
            subject
            (comment-notification-email-text on-id
                                             sender-name
                                             subject
                                             people
                                             text
                                             :email email
                                             :unsubscribe-key unsub-key
                                             :group-name (getf sender-group
                                                               :name)
                                             :groupid groupid
                                             :on-type on-type
                                             :inventory-text inventory-text)
            :html-message (comment-notification-email-html
                            on-id
                            sender-name
                            subject
                            people
                            text
                            :email email
                            :unsubscribe-key unsub-key
                            :group-name (getf sender-group :name)
                            :groupid groupid
                            :on-type on-type
                            :inventory-text inventory-text)))))))

(defun comment-notification-email-text
  (on-id
   from
   subject
   people
   text
   &key inventory-text
        email
        unsubscribe-key
        group-name
        groupid
        on-type)
  (strcat*
(no-reply-notice)
"You can also reply to this message by clicking on the link below."
#\linefeed #\linefeed
"A conversation with " people
#\linefeed #\linefeed
"Subject: " subject
(awhen inventory-text
  (strcat #\linefeed it #\linefeed))
#\linefeed #\linefeed
from (awhen group-name (s+ " from " it )) " says:"
#\linefeed #\linefeed
"\"" text "\""
#\linefeed #\linefeed
"You can see the conversation on Kindista here:"
#\linefeed
(strcat *email-url* (if (eq on-type :transaction) "transactions/" "conversations/") on-id)
#\linefeed #\linefeed
"Thank you for sharing your gifts with us!
-The Kindista Team"
#\linefeed #\linefeed #\linefeed
(unsubscribe-notice-ps-text
  unsubscribe-key
  email
  (s+ "notifications when people send "
      (or group-name "you")
      " messages through Kindista")
  :groupid groupid)))


(defun comment-notification-email-html
  (on-id
   from
   subject
   people
   text
   &key inventory-text
        email
        unsubscribe-key
        group-name
        groupid
        on-type
   &aux (url (strcat *email-url*
                     (if (eq on-type :transaction)
                       "transactions/"
                       "conversations/")
                     on-id)))

  (html-email-base
    (html
      (:p :style *style-p* (:strong (str (no-reply-notice))))
      (:p :style *style-p* "You can also reply to this message by clicking on the link below." )

      (:p :style *style-p*
        "A conversation with " (:strong (str people)))

      (:p :style *style-p*
        (:strong "Subject: " (str subject)))

      (awhen inventory-text
        (htm (:table :cellspacing 0
                     :cellpadding 0
                     :style *style-quote-box*
               (:tr (:td :style "padding: 4px 12px;"
                        "\"" (str (email-text it)) "\"")))
             (:br)))

      (:p :style *style-p*
        (:strong (str from))
        (when group-name
          (htm " from " (:strong (str group-name))))
        " says:")

      (:table :cellspacing 0
              :cellpadding 0
              :style *style-quote-box*
        (:tr (:td :style "padding: 4px 12px;"
                 "\"" (str (email-text text)) "\"")))

      (:p :style *style-p*
       "You can see the conversation on Kindista here: "
       (:a :href url (str url)))

      (:p :style *style-p* "Thank you for sharing your gifts with us!")

      (:p "-The Kindista Team")

      (str (unsubscribe-notice-ps-html
             unsubscribe-key
             email
             (s+ "notifications when people send "
                 (or group-name "you")
                 " messages through Kindista")
             :groupid groupid)))))

