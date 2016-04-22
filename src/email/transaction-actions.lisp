;;; Copyright 2012-2016 CommonGoods Network, Inc.
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
        (inventory-by (getf on-item :by))
        (on-type (getf on-item :type))
        (on-type-string (case on-type
                          (:offer "offer")
                          (:request "request")))
        (on-title (getf on-item :title))
        (on-details (getf on-item :details))
        (event-party (if (getf transaction :log)
                       (getf log-event :party)
                       (list (getf transaction :by))))
        (transaction-action (getf log-event :action))
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
        (transaction-url (strcat *email-url*
                                 "transactions/"
                                 transaction-id)) 
        (deactivation-text (s+ "No longer "
                               on-type-string
                               "ing this item? "))
        (deactivation-link (url-compose (strcat *email-url*
                                                on-type-string
                                                "s/"
                                                inventory-id)
                                        "deactivate" "t"))
        (recipients (remove nil
                            (if other-party-is-group-p
                              (getf other-party :notify-message)
                              (when (and (getf other-party :notify-message)
                                         (getf other-party :active))
                                (list other-party-id))))))
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
                  "please confirm the gift you received through kindista")
                 ((eq (getf log-event :action) :gave)
                  (gift-text :html-p html-p))
                 ((not log-event)
                  (s+ event-actor-name
                      " replied to "
                      (if groupid
                        (s+ group-name "'s ")
                        "your ")
                      on-type-string
                      (unless title-p ":")))
                 (t (transaction-action-text
                      log-event
                      transaction
                      on-item
                      :userid userid
                      :inventory-descriptor (case on-type
                                              (:offer "an offer")
                                              (:request "a request")))))))

    (when (and recipients (> (length recipients) 0))
      (dolist (recipient-id recipients)
        (let* ((recipient (db recipient-id))
               (push-message-author-and-type
                 (s+ (if (eql inventory-by recipient-id)
                           "your "
                           (s+ (string-capitalize (db inventory-by :name)) "'s "))
                     on-type-string))
               (inventory-author-and-type
                 (s+ (if (eql inventory-by recipient-id)
                        "Your "
                        (s+ (db inventory-by :name) "'s "))
                     on-type-string))
               (email (car (getf recipient :emails)))
               (unsub-key (getf recipient :unsubscribe-key)))

          (send-push-through-chrome-api (list (list :id recipient-id))
                                        :message-title (s+ "New message about " push-message-author-and-type)
                                        :message-body on-title
                                        :message-tag "transaction-tag"
                                        ;:message-type on-type
                                        :message-url (strcat +base-url+
                                                             "transactions/"
                                                             transaction-id))
          (when (or *productionp*
                    (getf recipient :test-user))
            (cl-smtp:send-email
              +mail-server+
              "Kindista <noreply@kindista.org>"
              email
              (notification-text recipient-id :title-p t)
              (transaction-action-notification-email-text
                event-actor-name
                transaction-url
                inventory-author-and-type
                :deactivation-link deactivation-link
                :deactivation-text deactivation-text
                :transaction-action transaction-action
                :email email
                :unsubscribe-key unsub-key
                :group-name group-name
                :groupid groupid
                :on-title on-title
                :on-details on-details
                :text (notification-text recipient-id))
              :html-message (transaction-action-notification-email-html
                              event-actor-name
                              transaction-url
                              inventory-author-and-type
                              :transaction-action transaction-action
                              :deactivation-link deactivation-link
                              :deactivation-text deactivation-text
                              :email email
                              :unsubscribe-key unsub-key
                              :group-name group-name
                              :groupid groupid
                              :on-title on-title
                              :on-details on-details
                              :text (notification-text recipient-id :html-p t)
                              :message message))))))))

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
  (other-party-name
   url
   inventory-author-and-type
   &key on-title
        on-details
        transaction-action
        deactivation-link
        deactivation-text
        text
        unsubscribe-key
        email
        group-name
        groupid)
  (strcat*
    text
    #\linefeed #\linefeed
    inventory-author-and-type
    ":"
    #\linefeed
    (awhen on-title (strcat it #\linefeed))
    (awhen on-details (strcat it #\linefeed))
    ;(when message
    ;  (strcat* #\linefeed #\linefeed
    ;           other-party-name
    ;           " says:"
    ;           #\linefeed #\linefeed
    ;           message))
    #\linefeed
    (if (eq transaction-action :gave)
      "To post gratitude for "
      "To reply to ")
    other-party-name
    ", please go to:"
    #\linefeed
    url
    #\linefeed #\linefeed
    deactivation-text
    #\linefeed
    deactivation-link
    #\linefeed #\linefeed
    (unsubscribe-notice-ps-text
      unsubscribe-key
      email
      (s+ "notifications when people send "
          (or group-name "you")
          " messages and reply to "
          (if groupid "its" "your")
          " offers and requests")
      :groupid groupid)))

(defun transaction-action-notification-email-html
  (other-party-name
   url
   inventory-author-and-type
   &key on-title
        on-details
        transaction-action
        deactivation-link
        deactivation-text
        text
        message
        unsubscribe-key
        email
        group-name
        groupid)
  (html-email-base
    (html
      (:p :style *style-p* (str (email-text text)))

      (:p :style *style-p* (str inventory-author-and-type) ":")

      (:table :cellspacing 0 :cellpadding 0
              :style *style-quote-box*
        (:tr (:td :style "padding: 4px 12px;"
               (awhen on-title
                 (htm (:p (:strong (str (ellipsis it :email t))))))
               (awhen on-details
                 (htm (:p (str (ellipsis it :email t))))))))

      (str (email-action-button
             url
             (if message
               "See Message"
               (s+ (if (eq transaction-action :gave)
                     "Post gratitude for "
                     "Respond to ")
                   other-party-name))))
     ;(when message
     ;  (htm
     ;    (:p :style *style-p*  (str other-party-name) " says:")
     ;    (:table :cellspacing 0 :cellpadding 0
     ;            :style *style-quote-box*
     ;      (:tr (:td :style "padding: 4px 12px;"
     ;             (str (email-text message)))))))

     ;(:p :style *style-p*
     ;  "To reply to "
     ;  (str other-party-name)
     ;  ", please go to:"
     ;  (:br)
     ;  (:a :href url
     ;   (str url)))

      (:p :style *style-p*
       (str deactivation-text)
       (:strong (:a :href deactivation-link "Deactivate it.")))

      (str
        (unsubscribe-notice-ps-html
          unsubscribe-key
          email
          (s+ "notifications when people send "
              (or group-name "you")
              " messages and reply to "
              (if groupid "its" "your")
              " offers and requests")
          :groupid groupid)))))
