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

(defun send-transaction-action-notification-email
  (transaction-id
   log-event
   &aux (transaction (db transaction-id))
        (transaction-initiator (db (getf transaction :by)))
        (inventory-id (getf transaction :on))
        (inventory-item (db inventory-id))
        (inventory-by (db (getf inventory-item :by)))
        (on-type (getf inventory-item :type))
        (event-party (getf log-event :party))
        (event-actor-id (car event-party))
        (event-actor (db event-actor-id))
        (event-actor-name (getf event-actor :name))
        (group-actor (db (cdr event-party)))
        (other-party-id (transaction-other-party transaction-id
                                                 event-actor-id))
        (other-party (db other-party-id))
        (recipients (case (getf other-party :type)
                      (:group
                        (getf other-party :admins))
                      (:person
                        (list other-party-id))))
        (transaction-url (strcat +base-url+
                                 "transactions/"
                                 transaction-id))
        (inventory-url (strcat +base-url+
                               (resource-url inventory-id
                                             inventory-item
                                             t))))

  (labels ((gift-text (&key html-p)
             (gift-given-notification-text (aif group-actor
                                             (if html-p
                                               (group-link (cdr event-party))
                                               (getf it :name))
                                             (if html-p
                                               (person-link event-actor-id)
                                               event-actor-name))
                                           (case (getf other-party :type)
                                             (:group
                                               (if html-p
                                                 (group-link other-party-id)
                                                 (getf other-party :name)))
                                             (t "you"))
                                           transaction-url
                                           :html-p html-p))
           (notification-text (inventory-descriptor &key html-p title-p)
             (cond
               ((and (eq (getf log-event :action) :gave) title-p)
                "Please confirm the gift you received through Kindista")
               ((eq (getf log-event :action) :gave)
                (gift-text :html-p html-p))
               (t (transaction-action-text
                    log-event
                    on-type
                    inventory-descriptor
                    (getf inventory-by :name)
                    (getf transaction-initiator :name)
                    :html-p html-p)))))

    (dolist (recipient-id recipients)
      (let ((recipient (db recipient-id)))
        (cl-smtp:send-email
          +mail-server+
          "DoNotReply <noreply@kindista.org>"
           (car (getf recipient :emails))
           (notification-text (case on-type
                                (:offer "an offer")
                                (:request "a request"))
                              :html-p nil
                              :title-p t)
           (transaction-action-notification-email-text
             (getf other-party :name)
             event-actor-name
             (notification-text (case on-type
                                  (:offer "an offer")
                                  (:request "a request"))
                                :html-p nil)
             transaction-url
             (cdr event-party))
           :html-message (transaction-action-notification-email-html
                           (getf other-party :name)
                           event-actor-name
                           (notification-text
                             (html
                               (str (case on-type
                                      (:offer "an ")
                                      (:request "a ")))
                               (:a :href inventory-url
                                 (str (case on-type
                                        (:offer "offer")
                                        (:request "request")))))
                             :html-p t)
                            transaction-url
                            (cdr event-party)))))))

(defun gift-given-notification-text
  (giver-name
   recipient-name
   transaction-url
   &key (html-p nil)
   &aux (text (strcat*
                 giver-name
                 " has indicated that they have given "
                 recipient-name
                 " a gift in a transaction facilitated by Kindista."
                 #\linefeed #\linefeed
                 "Please "
                 (if html-p
                   (html (:a :href transaction-url "confirm"))
                   (s+ "go to " transaction-url " and confirm"))
                 " that you have received this gift and post a statement of gratitude to let other Kindistas know that "
                 giver-name
                 " is a contributing member of the Kindista community.")))
   (if html-p
     (regex-replace-all "\\n" text "<br>")
     text))

(defun transaction-action-notification-email-text (name other-party-name text url group-id)
  (strcat*
(no-reply-notice)
#\linefeed
"Hi " name ","
#\linefeed #\linefeed
"We're writing to let you know about recent activity in transaction you are part of on Kindista."
#\linefeed #\linefeed
text
#\linefeed #\linefeed
"For more information, or to reply to "
other-party-name
", please go to:"
#\linefeed
url
#\linefeed #\linefeed
"If you no longer wish to receive notifications when people send you messages, please edit your communication settings:
"
(s+ +base-url+
    "settings/communication"
    (awhen group-id (strcat "?groupid=" it)))
#\linefeed #\linefeed
"Thank you for sharing your gifts with us!"
#\linefeed
"-The Kindista Team"))

(defun transaction-action-notification-email-html
  (name other-party-name text url group-id)
  (html-email-base
    (html
      (:p :style *style-p* (:strong (str (no-reply-notice))))

      (:p :style *style-p* "Hi " (str name) ",")

      (:p :style *style-p*
        "We're writing to let you know about recent activity in a transaction you are part of on Kindista."
        )

      (:table :cellspacing 0 :cellpadding 0
              :style *style-quote-box*
        (:tr (:td :style "padding: 4px 12px;"
               (str text))))

      (:p :style *style-p*
        "For more information, or to reply to "
        (str other-party-name)
        ", please go to:"
        (:br)
        (:a :href url
         (str url)))

      (:p :style *style-p*
          "If you no longer wish to receive notifications when people send you messages, please edit your settings:"
       (:br)
       (:a :href (s+ +base-url+
                     "settings/communication"
                     (awhen group-id (strcat "?groupid=" it)))
           (str (s+ +base-url+
                    "settings/communication"
                    (awhen group-id (strcat "?groupid=" it))))))

      (:p :style *style-p* "Thank you for sharing your gifts with us!")
      (:p "-The Kindista Team"))))
