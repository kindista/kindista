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
         (on-type (getf on-item :type)) ; :converation or :reply
         (participants (getf on-item :participants))
         (inventory-item (db (getf on-item :on))) ; when reply, get inventory item
         (inventory-type (if (eq (getf inventory-item :type) :request)
                           "request" "offer"))
         (inventory-text (or (getf inventory-item :text)
                             (getf on-item :deleted-item-text)))
         (sender-id (car (getf comment :by)))
         (sender-name (db sender-id :name))
         (inventory-poster (getf inventory-item :by))
         (inventory-poster-data (db inventory-poster))
         (notify-group-admins (when (eql (getf inventory-poster-data :type)
                                         :group)
                                (getf inventory-poster-data :notify-message)))
         (text (aif (getf on-item :deleted-item-type)
                 (deleted-invalid-item-reply-text (db (car (remove sender-id participants)) :name)
                                                  sender-name
                                                  it
                                                  (getf comment :text))
                 (getf comment :text)))

         ;; get an a list of (person-id . group-id)
         (people (mapcar #'car (getf on-item :people))))

    (flet ((subject (groupid)
             (if (eq on-type :reply)
               (if (eql sender-id inventory-poster)
                 (s+ sender-name " has replied to your question about their " inventory-type ":")
                 (s+ sender-name " has replied to "
                     (aif groupid
                       (s+ (db it :name) "'s ")
                       "your ")
                     inventory-type ":"))
               (getf on-item :subject))))

      (dolist (recipient (loop for person in (remove (assoc sender-id people)
                                                     people
                                                     :test #'equalp)
                               when (or (and (member (car person) participants)
                                             (db (car person) :notify-message))
                                        ; or when it's to a group
                                        (member (car person)
                                                notify-group-admins))
                               collect person))

        (cl-smtp:send-email
          +mail-server+
          "DoNotReply <noreply@kindista.org>"
          (car (db (car recipient) :emails))
          (or (subject (cdr recipient))
              (s+ "New message from " sender-name))
          (comment-notification-email-text on-id
                                           sender-name
                                           (or (subject (cdr recipient))
                                               (s+ "New message from "
                                                   sender-name))
                                           (name-list (remove (car recipient)
                                                              participants)
                                                      :func #'person-name
                                                      :maximum-links 5)
                                           text
                                           :inventory-text inventory-text)
          :html-message (comment-notification-email-html
                          on-id
                          (person-email-link sender-id)
                          (or (subject (cdr recipient))
                              (s+ "New message from " sender-name))
                          (name-list (remove (car recipient) participants)
                                     :func #'person-email-link
                                     :maximum-links 5)
                          text
                          :inventory-text inventory-text))))))

(defun comment-notification-email-text (on-id from subject people text &key inventory-text)
  (strcat 
(no-reply-notice)
#\linefeed #\linefeed
"You can also reply to this message by clicking on the link below."
#\linefeed #\linefeed
"A conversation with " people
#\linefeed
"Subject: " subject
(awhen inventory-text
  (strcat #\linefeed it #\linefeed))
#\linefeed
from " says:"
#\linefeed #\linefeed
"\"" text "\""
#\linefeed #\linefeed
"You can see the conversation on Kindista here:"
(strcat +base-url+ "conversations/" on-id)

#\linefeed #\linefeed
"You can see the conversation on Kindista here:"
"If you no longer wish to receive notifications when people send you messages, please edit your communication settings:
"
(strcat +base-url+ "settings/communication")
#\linefeed #\linefeed
"Thank you for sharing your gifts with us!
-The Kindista Team"))


(defun comment-notification-email-html (on-id from subject people text &key inventory-text)
  (html-email-base
    (html
      (:p :style *style-p* (:strong (str (no-reply-notice))))
      (:p :style *style-p* "You can also reply to this message by clicking on the link below." )

      (:p :style *style-p*
        "A conversation with " (str people))

      (:p :style *style-p*
        (:strong "Subject: " (str subject)))

      (awhen inventory-text
        (htm (:table :cellspacing 0
                     :cellpadding 0
                     :style *style-quote-box*
               (:tr (:td :style "padding: 4px 12px;"
                        "\"" (str (html-text it)) "\"")))
             (:br)))

      (:p :style *style-p*
        (str from) " says:")

      (:table :cellspacing 0
              :cellpadding 0
              :style *style-quote-box*
        (:tr (:td :style "padding: 4px 12px;"
                 "\"" (str (html-text text)) "\"")))

      (:p :style *style-p*
       "You can see the conversation on Kindista here: "
       (:a :href (strcat +base-url+ "conversations/" on-id)
                 (str (strcat +base-url+ "conversations/" on-id))))

      (:p :style *style-p*
          "If you no longer wish to receive notifications when people send you messages, please edit your communication settings:"
       (:br)
       (:a :href (strcat +base-url+ "settings/communication")
                 (str (strcat +base-url+ "settings/communication"))))

      (:p :style *style-p* "Thank you for sharing your gifts with us!")
      (:p "-The Kindista Team"))))

