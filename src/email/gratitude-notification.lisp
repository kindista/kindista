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

(defun send-gratitude-notification-email (gratitude-id)
  (let* ((gratitude (db gratitude-id))
         (from (getf gratitude :author))
         (to-list (iter (for subject in (getf gratitude :subjects))
                        (when (db subject :notify-gratitude)
                          (collect subject)))))
    (dolist (to to-list)
      (cl-smtp:send-email +mail-server+
                          "Kindista <noreply@kindista.org>"
                          (car (getf (db to) :emails))
                          (s+ (getf (db from) :name) " has posted a statement of gratitude about you")
                          (gratitude-notification-email-text gratitude-id
                                                             gratitude
                                                             from)
                          :html-message (gratitude-notification-email-html gratitude-id gratitude from)))))

(defun gratitude-notification-email-text (gratitude-id gratitude from)
  (strcat
(no-reply-notice)
(getf (db from) :name)
" has shared a statement of gratitude about you on Kindista.

"
(getf gratitude :text)
"

You can see the statement on Kindista here:
"
+base-url+ "gratitude/" gratitude-id

"
If you no longer wish to receive notifications when people post gratitude about you, please edit your settings:
"
+base-url+ "settings/communication"
"

Thank you for sharing your gifts with us!
-The Kindista Team"))


(defun gratitude-notification-email-html (gratitude-id gratitude from)
  (html-email-base
    (html
      (:p :style *style-p* (str (no-reply-notice)))
      (:p :style *style-p*
        "PLEASE DO NOT REPLY TO THIS EMAIL, IT WILL NOT BE DELIVERED TO THE SENDER."
        (:br)
        "If you want to reply to the message, please click on the link below.")

      (:p :style *style-p* 
          (str (person-email-link from))
            " has shared a "
            (:a :href (strcat +base-url+ "gratitude/" gratitude-id)
                          "statement of gratitude")
                " about you on Kindista.")

      (:table :cellspacing 0 :cellpadding 0
              :style *style-quote-box*
        (:tr (:td :style "padding: 4px 12px;"

               (str (getf gratitude :text)))))

      (:p :style *style-p* 
          "If you no longer wish to receive notifications when people post gratitude about you, please edit your settings:"
       (:br)
       (:a :href (strcat +base-url+ "settings/communication") (strcat +base-url+ "settings/communication")))

      (:p :style *style-p* "Thank you for sharing your gifts with us!")
      (:p "-The Kindista Team"))))

