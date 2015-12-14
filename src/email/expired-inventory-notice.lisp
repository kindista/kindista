;;; Copyright 2012-2015 CommonGoods Network, Inc.
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
        (typestring (case type (:offer "offer") (:request "request")))
        (by-id (getf item :by))
        (by (db by-id))
        (group-p (eql (getf by :type) :group))
        (group-name (when group-p (getf by :name)))
        (recipients)
        (status (s+ typstring
                    (if (< (getf item :expires)
                        (get-universal-time))
                      " has expired"
                      " will expire soon")))
        (title (strcat* "Your "
                         (when group-p "group's ")
                         status))
        (message (s+ (when group-name
                       (s+ group-name "'s ")
                       "Your ")
                     status))
        (url (strcat *email-url* typestring "s/" item-id)))

  (if (eql by-type :person)
    (when (getf by :notify-inventory-expiration)
      (push (list :id subject
                  :name (getf by :name)
                  :email (car (getf by :emails))
                  :unsubscribe-key (getf by :unsubscribe-key))
          recipients))
    (dolist (admin-id (getf by :admins))
      (let ((person (db admin-id)))
        (when (getf person :notify-inventory-expiration)
          (push (list :group-name name
                      :name (getf person :name)
                      :groupid by
                      :email (car (getf person :emails))
                      :unsubscribe-key (getf person :unsubscribe-key)
                      :id admin-iy)
              recipients)))))

    (dolist (recipient recipients)
      (cl-smtp:send-email +mail-server+
                          "Kindista <noreply@kindista.org>"
                          (getf recipient :email)
                          title
                          (inventory-expiration-email-text
                            url
                            recipient)
                          :html-message (inventory-expiration-email-html
                                          url
                                          author-name
                                          recipient))))

(defun gratitude-notification-email-text
  (gratitude-url
   author-name
   recipient)
  (strcat
#\linefeed #\linefeed
author-name
" has shared a statement of gratitude about "
(or (getf recipient :group-name) "you")
" on Kindista."
#\linefeed #\linefeed
"You can see the statement on Kindista here:"
#\linefeed
gratitude-url
#\linefeed #\linefeed
"Thank you for sharing your gifts with us!"
#\linefeed
";-The Kindista Team"
#\linefeed #\linefeed
(unsubscribe-notice-ps-text
  (getf recipient :unsubscribe-key)
   (getf recipient :email)
   (s+ "notifications when people post statements of gratitude about "
       (or (getf recipient :group-name) "you"))
   :groupid (getf recipient :groupid))))

(defun gratitude-notification-email-html
  (gratitude-url author-name recipient)
  (html-email-base
    (html
      (:p :style *style-p*
          (str author-name)
            " has shared a statement of gratitude about "
                (str (or (getf recipient :group-name) "you"))
                " on Kindista.")

      (str (email-action-button gratitude-url "View on Kindista"))

      (:p :style *style-p* "Thank you for sharing your gifts!")

      (:p "-The Kindista Team")

      (str (unsubscribe-notice-ps-html
             (getf recipient :unsubscribe-key)
             (getf recipient :email)
             (s+ "notifications when people post statements of gratitude about "
                 (or (getf recipient :group-name) "you"))
             :groupid (getf recipient :groupid)))
      )))

