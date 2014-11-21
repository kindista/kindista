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
        (if (eql (getf data :type) :person)
          (when (getf data :notify-gratitude)
            (push (list :id subject
                        :email (car (getf data :emails))
                        :unsubscribe-key (getf data :unsubscribe-key))
                recipients))
          (dolist (admin (getf data :notify-gratitude))
            (let ((person (db admin)))
              (push (list :group-name name
                          :groupid subject
                          :email (car (getf person :emails))
                          :unsubscribe-key (getf person :unsubscribe-key)
                          :id admin)
                    recipients))))))

    (dolist (recipient recipients)
      (cl-smtp:send-email +mail-server+
                          "DoNotReply <noreply@kindista.org>"
                          (car (db (getf recipient :id) :emails))
                          (s+ author-name
                              " has posted a statement of gratitude about "
                              (aif (getf recipient :group-name)
                                it
                                "you"))
                          (gratitude-notification-email-text
                            author-name
                            gratitude-id
                            gratitude
                            recipient)
                          :html-message (gratitude-notification-email-html
                                          gratitude-id
                                          gratitude
                                          from
                                          recipient)))))

(defun gratitude-notification-email-text
  (author-name
   gratitude-id
   gratitude
   recipient)
  (strcat
(no-reply-notice)
#\linefeed #\linefeed
author-name
" has shared a statement of gratitude about "
(or (getf recipient :group-name) "you")
" on Kindista."
#\linefeed #\linefeed
(getf gratitude :text)
#\linefeed #\linefeed
"You can see the statement on Kindista here:"
#\linefeed
*email-url* "gratitude/" gratitude-id
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
  (gratitude-id gratitude from recipient)
  (html-email-base
    (html
      (:p :style *style-p* (:strong (str (no-reply-notice))))
      (:p :style *style-p*
        "If you want to reply to the message, please click on the link below.")

      (:p :style *style-p* 
          (str (person-email-link from))
            " has shared a "
            (:a :href (strcat *email-url* "gratitude/" gratitude-id)
                          "statement of gratitude")
                " about "
                (str (or (getf recipient :group-name) "you"))
                " on Kindista.")

      (:table :cellspacing 0 :cellpadding 0
              :style *style-quote-box*
        (:tr (:td :style "padding: 4px 12px;"
               (str (getf gratitude :text)))))

      (:p :style *style-p* "Thank you for sharing your gifts with us!")

      (:p "-The Kindista Team")

      (str (unsubscribe-notice-ps-html
             (getf recipient :unsubscribe-key)
             (getf recipient :email)
             (s+ "notifications when people post statements of gratitude about "
                 (or (getf recipient :group-name) "you"))
             :groupid (getf recipient :groupid)))
      )))

