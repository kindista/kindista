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

(defun send-gratitude-notification (gratitude-id)
  (let* ((gratitude (db gratitude-id))
         (from (getf gratitude :author))
         (author-name (db from :name))
         (url (strcat *email-url* "gratitude/" gratitude-id))
         (recipients)
         (push-recipients))

    (dolist (subject (getf gratitude :subjects))
      (let* ((data (db subject))
             (name (getf data :name)))
        (if (eql (getf data :type) :person)
          (when (and (getf data :notify-gratitude)
                     (getf data :active))
            (push (list :id subject
                        :email (car (getf data :emails))
                        :unsubscribe-key (getf data :unsubscribe-key))
                recipients)
            (push subject (getf push-recipients :individuals)))
          (dolist (admin (getf data :notify-gratitude))
            (let ((person (db admin)))
              (when (getf person :active)
                (push (list :group-name name
                            :groupid subject
                            :email (car (getf person :emails))
                            :unsubscribe-key (getf person :unsubscribe-key)
                            :id admin)
                    recipients)
                (push admin (getf push-recipients :groups))))))))
    (send-push-through-chrome-api (getf push-recipients :individuals)
                                  :message-title "Statement of Gratitude"
                                  :message-body (s+ author-name
                                                    " shared gratitude for you")
                                  :message-tag "gratitude_tag"
                                  ;:message-type :gratitude
                                  :message-url (strcat +base-url+
                                                       "gratitude/"
                                                       gratitude-id))
    (send-push-through-chrome-api (getf push-recipients :groups)
                                 :message-title "Statement of Gratitude"
                                 :message-body (s+ author-name
                                                   " shared gratitude for a group you manage")
                                 :message-tag "gratitude_tag"
                                 ;:message-type :gratitude
                                 :message-url (strcat +base-url+
                                                      "gratitude/"
                                                      gratitude-id))
    (dolist (recipient recipients)
      (cl-smtp:send-email +mail-server+
                          "Kindista <noreply@kindista.org>"
                          (car (db (getf recipient :id) :emails))
                          (s+ author-name
                              " has posted a statement of gratitude about "
                              (aif (getf recipient :group-name)
                                it
                                "you"))
                          (gratitude-notification-email-text
                            url
                            author-name
                            recipient)
                          :html-message (gratitude-notification-email-html
                                          url
                                          author-name
                                          recipient)))))

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
   :groupid (getf recipient :groupid)
   :unsub-type "gratitude")))

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
             :groupid (getf recipient :groupid)
             :unsub-type "gratitude")))))
