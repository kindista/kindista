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
  (let* ((comment (db comment-id))
         (on-id (getf comment :on))
         (on-item (db on-id))
         (on-type (getf on-item :type))
         (inventory-item (db (getf on-item :on)))
         (inventory-type (if (eq (getf inventory-item :type) :request)
                           "request" "offer"))
         (inventory-text (getf inventory-item :text))
         (text (getf comment :text))
         (sender-id (getf comment :by))
         (sender-name (db sender-id :name))
         (inventory-poster (getf inventory-item :by))
         (subject (if (eq on-type :reply)
                    (if (eql sender-id inventory-poster)
                      (s+ sender-name " has replied to your question about their " inventory-type ":")
                      (s+ sender-name " has replied to your " inventory-type ":"))
                    (getf on-item :subject)))
         (people (mapcar #'car (getf on-item :people))))
    (dolist (to (iter (for person in (remove sender-id people))
                      (when (db person :notify-message)
                        (collect person))))
      (cl-smtp:send-email
        +mail-server+
        "Kindista <noreply@kindista.org>"
        (car (db to :emails))
        (s+ "New message from " sender-name)
        (comment-notification-email-text on-id
                                         sender-name
                                         subject
                                         (name-list (remove to people)
                                                    :func #'person-name
                                                    :minimum-links 5)
                                         text
                                         :inventory-text inventory-text)
        :html-message (comment-notification-email-html
                        on-id
                        (person-email-link sender-id)
                        subject
                        (name-list (remove to people)
                                   :func #'person-email-link
                                   :minimum-links 5)
                        text
                        :inventory-text inventory-text)))))

(defun comment-notification-email-text (on-id from subject people text &key inventory-text)
  (strcat "A conversation with " people
"

Subject: " subject
(awhen inventory-text (strcat 
"
"it))
"

"
from " says:

"
"\"" text "\" "
"

You can see the conversation on Kindista here:
"
(strcat +base-url+ "conversation/" on-id)

"

If you no longer wish to receive notifications when people send you messages, please edit your settings:
"
(strcat +base-url+ "settings/communication")

"

Thank you for sharing your gifts with us!
-The Kindista Team"))


(defun comment-notification-email-html (on-id from subject people text &key inventory-text)
  (html-email-base
    (html
      (:p :style *style-p*
        "A conversation with " (str people))

      (:p :style *style-p*
        (:strong "Subject: " (str subject)))

      (awhen inventory-text
        (htm (:table :cellspacing 0
                     :cellpadding 0
                     :style *style-quote-box*
               (:tr (:td :style "padding: 4px 12px;"
                        "\"" (str it) "\"")))))

      (:p :style *style-p*
        (str from) " says:")

      (:table :cellspacing 0
              :cellpadding 0
              :style *style-quote-box*
        (:tr (:td :style "padding: 4px 12px;"
                 "\"" (str text) "\"")))

      (:p :style *style-p*
       "You can see the conversation on Kindista here: "
       (:a :href (strcat +base-url+ "conversation/" on-id)
                 (str (strcat +base-url+ "conversation/" on-id))))

      (:p :style *style-p*
          "If you no longer wish to receive notifications when people send you messages, please edit your settings:"
       (:br)
       (:a :href (strcat +base-url+ "settings/communication")
                 (str (strcat +base-url+ "settings/communication"))))

      (:p :style *style-p* "Thank you for sharing your gifts with us!")
      (:p "-The Kindista Team"))))

