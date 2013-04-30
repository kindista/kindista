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

(defun send-contact-notification-email (from to)
  (cl-smtp:send-email +mail-server+
                      "Kindista <noreply@kindista.org>"
                      to
                      (s+ from " added you to their contacts on Kindista!")
                      (contact-notification-email-text from)
                      :html-message (contact-notification-email-html from)))

(defun contact-notification-email-text (from)
  (strcat (getf (db from) :name)
" has added you to their contacts on Kindista.
"

"
If you no longer wish to receive notifications, please edit your settings:
"
+base-url+ "settings/email"
"

Thank you for sharing your gifts with us!
-The Kindista Team"))


(defun contact-notification-email-html (from)
  (html-email-base
    (html
      (:p :style *style-p* 
          (str (person-email-link from))
            " has added you tho their contacts on Kindista.")

      (:p :style *style-p* 
          "If you no long wish to receive notifications, please edit your settings:"
       (:br)
       (:a :href (strcat +base-url+ "settings/communication") (strcat +base-url+ "settings/communication")))

      (:p :style *style-p* "Thank you for sharing your gifts with us!")
      (:p "-The Kindista Team"))))

