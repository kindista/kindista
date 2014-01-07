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
         (author-name (db from :name))
         (recipients))

    (dolist (subject (getf gratitude :subjects))
      (let* ((data (db subject))
             (name (getf data :name)))
        (awhen (getf data :notify-gratitude)
          (if (eql (getf data :type) :person)
            (push (list :id subject) recipients)
            (dolist (member it)
              (push (list :group-name name
                          :group-id
                          :id member)
                    recipients))))))

    (dolist (recipient recipients)
      (let ((group-name (getf recipient :group-name))
            (group-id (getf recipient :group-id)))
        (cl-smtp:send-email +mail-server+
                            "Kindista <noreply@kindista.org>"
                            (car (db (getf recipient :id) :emails))
                            (s+ author-name
                                " has posted a statement of gratitude about "
                                (aif group-name it "you"))
                            (gratitude-notification-email-text
                              author-name
                              gratitude-id
                              gratitude
                              :group-name group-name
                              :group-id group-id)
                            :html-message (gratitude-notification-email-html
                                            gratitude-id
                                            gratitude
                                            from
                                            :group-name group-name
                                            :group-id group-id))))))

(defun gratitude-notification-email-text (author-name gratitude-id gratitude &key group-name group-id)
  (strcat
(no-reply-notice)
#\linefeed #\linefeed
author-name
" has shared a statement of gratitude about "
(or group-name "you")
" on Kindista."
#\linefeed #\linefeed
(getf gratitude :text)
#\linefeed #\linefeed
"You can see the statement on Kindista here:"
#\linefeed
+base-url+ "gratitude/" gratitude-id
#\linefeed #\linefeed
" If you no longer wish to receive notifications when people post gratitude about you, please edit your settings:"
#\linefeed
+base-url+ "settings/communication" (awhen group-id (strcat "?groupid=" it))
#\linefeed #\linefeed
"Thank you for sharing your gifts with us!
-The Kindista Team"))


(defun gratitude-notification-email-html (gratitude-id gratitude from &key group-name group-id)
  (html-email-base
    (html
      (:p :style *style-p* (:strong (str (no-reply-notice))))
      (:p :style *style-p*
        "If you want to reply to the message, please click on the link below.")

      (:p :style *style-p* 
          (str (person-email-link from))
            " has shared a "
            (:a :href (strcat +base-url+ "gratitude/" gratitude-id)
                          "statement of gratitude")
                " about "
                (or group-name "you")
                " on Kindista.")

      (:table :cellspacing 0 :cellpadding 0
              :style *style-quote-box*
        (:tr (:td :style "padding: 4px 12px;"

               (str (getf gratitude :text)))))

      (:p :style *style-p* 
          "If you no longer wish to receive notifications when people post gratitude about you, please edit your settings:"
       (:br)
       (:a :href (s+ +base-url+
                     "settings/communication"
                     (awhen group-id (strcat "?groupid=" it)))
           (s+ +base-url+ "settings/communication")))

      (:p :style *style-p* "Thank you for sharing your gifts with us!")
      (:p "-The Kindista Team"))))

