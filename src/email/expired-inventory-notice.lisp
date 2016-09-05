;;; Copyright 2016 CommonGoods Network, Inc.
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

(defun send-inventory-expiration-notice
  (item-id
   &aux (item (db item-id))
        (type (getf item :type))
        (item-description (or (getf item :title)
                              (getf item :details)))
        (typestring (case type (:offer "offer") (:request "request")))
        (by-id (getf item :by))
        (by (db by-id))
        (group-p (eql (getf by :type) :group))
        (group-name (when group-p (getf by :name)))
        (recipients)
        (status (if (< (getf item :expires)
                    (get-universal-time))
                  " has expired"
                  " will expire soon"))
        (title (strcat* (case type (:offer "An ") (:request "A "))
                        typestring
                        " "
                        (or group-name "you")
                        " posted on Kindista "
                        status))
        (message (s+ (if group-name
                       (s+ group-name "'s ")
                       "Your ")
                     typestring
                     status
                     "."))
        (url (s+ (url-compose (strcat *email-url* typestring "s/" item-id)
                              "edit" "t"
                              "focus" "expiration")
                 "#expiration")))

  (if (eql (getf by :type) :person)
    (when (getf by :notify-inventory-expiration)
      (push (list :name (getf by :name)
                  :email (car (getf by :emails))
                  :unsubscribe-key (getf by :unsubscribe-key))
          recipients))
    (dolist (admin-id (getf by :admins))
      (let ((person (db admin-id)))
        (when (getf person :notify-inventory-expiration)
          (push (list :group-name group-name
                      :name (getf person :name)
                      :groupid by
                      :email (car (getf person :emails))
                      :unsubscribe-key (getf person :unsubscribe-key)
                      :id admin-id)
              recipients)))))

    (dolist (recipient recipients)
      (cl-smtp:send-email +mail-server+
                          "Kindista <noreply@kindista.org>"
                          (getf recipient :email)
                          title
                          (inventory-expiration-email-text
                            message
                            item-description
                            typestring
                            url
                            recipient)
                          :html-message (inventory-expiration-email-html
                                          message
                                          item-description
                                          typestring
                                          url
                                          recipient))))

(defun inventory-expiration-email-text
  (message item-description typestring url recipient)
  (strcat
    message
    #\linefeed #\linefeed
    item-description
    #\linefeed #\linefeed
    "You can renew this "
    typestring
    " here:"
    #\linefeed #\linefeed
    url
    #\linefeed #\linefeed
    "Thank you for sharing your gifts with us!"
    #\linefeed
    ";-The Kindista Team"
    #\linefeed #\linefeed
    (unsubscribe-notice-ps-text
      (getf recipient :unsubscribe-key)
      (getf recipient :email)
      "notifications when your offers and requests are about to expire"
      :groupid (getf recipient :groupid)
      :unsub-type "inventory-expiration")))

(defun inventory-expiration-email-html
  (message item-description typestring url recipient)
  (html-email-base
    (html
      (:p :style *style-p* (str message))

      (str (email-blockquote item-description))

      (str (email-action-button url (s+ "Renew this "
                                        (string-capitalize typestring))))

      (:p :style *style-p* "Thank you for sharing your gifts!")

      (:p "-The Kindista Team")

      (str (unsubscribe-notice-ps-html
             (getf recipient :unsubscribe-key)
             (getf recipient :email)
             "notifications when your offers and requests are about to expire"
             :groupid (getf recipient :groupid)
             :unsub-type "inventory-expiration"))
      )))

(defun send-inventory-expiration-by-account-notice
  (account-id
   typestring
   count
   &aux (by (db account-id))
        (group-p (eql (getf by :type) :group))
        (group-name (when group-p (getf by :name)))
        (recipients)
        (title (strcat* (aif group-name (s+ it " has ") "You have ")
                        (pluralize count typestring)
                        " on Kindista which recently expired."))
        (message (s+ title
                     " Due to a bug in our system, you were not notified when your "
                     typestring
                     "(s) expired. "))
        (url (url-compose (strcat *email-url*
                                  (if group-p "groups/" "people/")
                                  (username-or-id account-id)
                                  "/"
                                  typestring
                                  "s")
                          "filter" "inactive")))

  (if (eql (getf by :type) :person)
    (when (and (getf by :active)
               (getf by :notify-inventory-expiration))
      (push (list :name (getf by :name)
                  :email (car (getf by :emails))
                  :unsubscribe-key (getf by :unsubscribe-key))
          recipients))
    (dolist (admin-id (getf by :admins))
      (let ((person (db admin-id)))
        (when (and (getf person :active)
                   (getf person :notify-inventory-expiration))
          (push (list :group-name group-name
                      :name (getf person :name)
                      :groupid by
                      :email (car (getf person :emails))
                      :unsubscribe-key (getf person :unsubscribe-key)
                      :id admin-id)
              recipients)))))

    (dolist (recipient recipients)
      (cl-smtp:send-email +mail-server+
                          "Kindista <noreply@kindista.org>"
                          (getf recipient :email)
                          title
                          (inventory-expiration-by-account-email-text
                            message
                            typestring
                            count
                            url
                            recipient)
                          :html-message (inventory-expiration-by-account-email-html
                                          message
                                          typestring
                                          count
                                          url
                                          recipient))))

(defun inventory-expiration-by-account-email-text
  (message typestring count url recipient)
  (strcat
    message
    #\linefeed #\linefeed
    "You can review and renew your expired  "
    (pluralize count typestring)
    " here:"
    #\linefeed #\linefeed
    url
    #\linefeed #\linefeed
    "Thank you for sharing your gifts with us!"
    #\linefeed
    ";-The Kindista Team"
    #\linefeed #\linefeed
    (unsubscribe-notice-ps-text
      (getf recipient :unsubscribe-key)
      (getf recipient :email)
      "notifications when your offers and requests are about to expire"
      :groupid (getf recipient :groupid)
      :unsub-type "inventory-expiration")))

(defun inventory-expiration-by-account-email-html
  (message typestring count url recipient)
  (html-email-base
    (html
      (:p :style *style-p* (str message))


      (str (email-action-button url (s+ "Review your expired "
                                        (pluralize count
                                                   (string-capitalize
                                                     typestring)))))

      (:p :style *style-p* "Thank you for sharing your gifts!")

      (:p "-The Kindista Team")

      (str (unsubscribe-notice-ps-html
             (getf recipient :unsubscribe-key)
             (getf recipient :email)
             "notifications when your offers and requests are about to expire"
             :groupid (getf recipient :groupid)
             :unsub-type "inventory-expiration"))
      )))

