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

(defun send-contact-notification-email (from-id to-id)
  (let* ((from (db from))
         (from-name (getf from :name))
         (to (db to))
         (to-email (car (getf to :emails)))
         (to-unsub-key (getf to :unsubscribe-key)))
    (cl-smtp:send-email +mail-server+
                        "Kindista <noreply@kindista.org>"
                        to-email
                        (s+ from-name
                            " added you to their contacts on Kindista!")
                        (contact-notification-email-text from-name
                                                         to-email
                                                         to-unsub-key)
                        :html-message (contact-notification-email-html
                                        from-id
                                        to-email
                                        to-unsub-key))))

(defun contact-notification-email-text (from-name email unsub-key)
  (strcat (getf (db from) :name)
" has added you to their contacts on Kindista."
#\linefeed #\linefeed
"Thank you for sharing your gifts with us!"
#\linefeed
"-The Kindista Team"
(unsubscribe-notice-ps-text
  unsub-key
  email
  "notifications when people add you to their list of Kindista contacts "
  :detailed-notification-description "notifications when people add you")))


(defun contact-notification-email-html (from-id email unsub-key)
  (html-email-base
    (html
      (:p :style *style-p* 
          (str (person-email-link from-id))
            " has added you tho their contacts on Kindista.")

      (:p :style *style-p* "Thank you for sharing your gifts with us!")
      (:p "-The Kindista Team")

      (str (unsubscribe-notice-ps-html
             unsub-key
             email
             "notifications when people add you to their list of Kindista contacts "
             :detailed-notification-description "notifications when people add you")))))

