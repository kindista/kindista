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
   &optional message
   &aux (transaction (db transaction-id))
        (inventory-id (getf transaction :on))
        (on-item (db inventory-id))
        (on-type (getf on-item :type))
        (on-type-string (case on-type
                          (:offer "offer")
                          (:request "request")))
        (on-title (getf on-item :title))
        (on-details (getf on-item :details))
        (event-party (if (getf transaction :log)
                       (getf log-event :party)
                       (list (getf transaction :by))))
        (event-actor-id (car event-party))
        (event-actor (db event-actor-id))
        (event-actor-name (getf event-actor :name))
        (group-actor (db (cdr event-party)))
        (other-party-id (transaction-other-party transaction-id
                                                 event-actor-id))
        (other-party (db other-party-id))
        (other-party-is-group-p (eq (getf other-party :type) :group))
        (groupid (when other-party-is-group-p other-party-id))
        (group-name (when groupid (getf other-party :name)))
        (recipients (if other-party-is-group-p
                      (getf other-party :admins)
                      (list other-party-id)))
        (transaction-url (strcat +base-url+
                                 "transactions/"
                                 transaction-id)))

  (labels ((gift-text (&key html-p)
             (gift-given-notification-text (aif group-actor
                                               (getf it :name)
                                               event-actor-name)
                                           (if groupid
                                             group-name
                                             "you")
                                           transaction-url
                                           :html-p html-p))
           (notification-text (userid &key title-p (html-p nil))
             (cond
               ((and (eq (getf log-event :action) :gave) title-p)
                "Please confirm the gift you received through Kindista")
               ((eq (getf log-event :action) :gave)
                (gift-text :html-p html-p))
               ((not log-event)
                (s+ event-actor-name
                    " replied to "
                    (if groupid
                      (s+ group-name "'s ")
                      "your ")
                    on-type-string
                    (unless title-p ".")))
               (t (transaction-action-text
                    log-event
                    transaction
                    on-item
                    :userid userid
                    :inventory-descriptor (case on-type
                                            (:offer "an offer")
                                            (:request "a request")))))))

    (dolist (recipient-id recipients)
      (let* ((recipient (db recipient-id))
             (email (car (getf recipient :emails)))
             (unsub-key (getf recipient :unsubscribe-key)))
        (when (getf recipient :notify-message)
          (cl-smtp:send-email
            +mail-server+
            "PleaseDoNotReply <noreply@kindista.org>"
            email
            (notification-text recipient-id :title-p t)
            (transaction-action-notification-email-text
              (getf other-party :name)
              event-actor-name
              transaction-url
              :email email
              :unsubscribe-key unsub-key
              :group-name group-name
              :groupid groupid
              :on-title on-title
              :on-details on-details
              :on-type-string on-type-string
              :text (notification-text recipient-id)
              :message message)
            :html-message (transaction-action-notification-email-html
                            (getf other-party :name)
                            event-actor-name
                            transaction-url
                            :email email
                            :unsubscribe-key unsub-key
                            :group-name group-name
                            :groupid groupid
                            :on-title on-title
                            :on-details on-details
                            :on-type-string on-type-string
                            :text (notification-text recipient-id :html-p t)
                            :message message)))))))

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

(defun transaction-action-notification-email-text
  (name
   other-party-name
   url
   &key on-title
        on-details
        on-type-string
        text
        message
        unsubscribe-key
        email
        group-name
        groupid)
  (strcat*
(no-reply-notice " use the link below to reply to this transaction on Kindista.org")
#\linefeed #\linefeed
"Hi " name ","
#\linefeed #\linefeed
text
#\linefeed #\linefeed
(string-capitalize on-type-string) " details:"
#\linefeed
(ellipsis (or on-title on-details))
(when message
  (strcat* #\linefeed #\linefeed
           other-party-name
           " says:"
           #\linefeed #\linefeed
           message))
#\linefeed #\linefeed
"To reply to "
other-party-name
", please go to:"
#\linefeed
url
#\linefeed #\linefeed
(unsubscribe-notice-ps-text
  unsubscribe-key
  email
  (s+ "notifications when people send "
      (or group-name "you")
      " messages and reply to "
      (if groupid "its" "your")
      " offers and requests")
  :groupid groupid)
#\linefeed #\linefeed
"Thank you for sharing your gifts with us!"
#\linefeed
"-The Kindista Team"))

(defun transaction-action-notification-email-html
  (name
   other-party-name
   url
   &key on-title
        on-details
        on-type-string
        text
        message
        unsubscribe-key
        email
        group-name
        groupid)
  (html-email-base
    (html
      (:p :style *style-p* (:strong (str (no-reply-notice " use the link below to reply to this transaction on Kindista.org"))))

      (:p :style *style-p* "Hi " (str name) ",")

      (:p :style *style-p* (str text))

     (:p :style *style-p*
      (str (string-capitalize on-type-string)) " details:")
     (:table :cellspacing 0 :cellpadding 0
             :style *style-quote-box*
       (:tr (:td :style "padding: 4px 12px;"
              (str (ellipsis (or on-title on-details))))))

      (when message
        (htm
          (:p :style *style-p*  (str other-party-name) " says:")
          (:table :cellspacing 0 :cellpadding 0
                  :style *style-quote-box*
            (:tr (:td :style "padding: 4px 12px;"
                   (str message))))))

      (:p :style *style-p*
        "To reply to "
        (str other-party-name)
        ", please go to:"
        (:br)
        (:a :href url
         (str url)))

      (str
        (unsubscribe-notice-ps-html
          unsubscribe-key
          email
          (s+ "notifications when people send "
              (or group-name "you")
              " messages and reply to "
              (if groupid "its" "your")
              " offers and requests")
          :groupid groupid))

      (:p :style *style-p* "Thank you for sharing your gifts with us!")
      (:p "-The Kindista Team"))))
