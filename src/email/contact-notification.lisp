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

(defun send-contact-notification (from-id to-id)
  (let* ((from (db from-id))
         (from-name (getf from :name))
         (recipient (db to-id))
         (email (car (getf recipient :emails)))
         (unsubscribe-key (getf recipient :unsubscribe-key)))
    (when (and (getf recipient :notify-new-contact)
               (getf recipient :active))
      (send-push-through-chrome-api (list to-id)
                                    :message-title (s+ from-name
                                                      " added you to their contacts")
                                    :message-body ""
                                    :message-tag "contact-tag"
                                    :message-url (strcat +base-url+
                                                         "people/"
                                                         from-id))
      
        (cl-smtp:send-email +mail-server+
                            "Kindista <noreply@kindista.org>"
                            email
                            (s+ from-name
                                " added you to their contacts on Kindista!")
                            (contact-notification-email-text from-name
                                                             email
                                                             unsubscribe-key)
                            :html-message (contact-notification-email-html
                                            from-id
                                            email
                                            unsubscribe-key)))))
(defun contact-notification-email-text
  (from-name email unsubscribe-key)
  (strcat from-name
" has added you to their contacts on Kindista."
#\linefeed #\linefeed
"Thank you for sharing your gifts with us!"
#\linefeed
"-The Kindista Team"
(unsubscribe-notice-ps-text
  unsubscribe-key
  email
  "notifications when people add you to their list of Kindista contacts "
  :detailed-notification-description "notifications when people add you"
  :unsub-type "new-contact")))


(defun contact-notification-email-html
  (from-id email unsubscribe-key)
  (html-email-base
    (html
      (:p :style *style-p* 
          (str (person-email-link from-id))
            " has added you to their contacts on Kindista.")

      (:p :style *style-p* "Thank you for sharing your gifts with us!")
      (:p "-The Kindista Team")

      (str (unsubscribe-notice-ps-html
             unsubscribe-key
             email
             "notifications when people add you to their list of Kindista contacts "
             :detailed-notification-description "notifications when people add you"
             :unsub-type "new-contact")))))

