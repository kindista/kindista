;;; Copyright 2012-2017 CommonGoods Network, Inc.
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

(defun send-feedback-notification-email (id)
  (let* ((feedback (db id))
         (by (db (getf feedback :by) :name))
         (text (getf feedback :text))
         (feedback-url (strcat *email-url* "feedback#" id)))
    (cl-smtp:send-email +mail-server+
                        "Kindista <noreply@kindista.org>"
                        (if *productionp*
                          "feedback@kindista.org"
                          *error-message-email*)
                        (s+ "Kindista feedback from " by)
                        (feedback-notification-email-text id
                                                          by
                                                          text
                                                          feedback-url)
                        :html-message (feedback-notification-email-html
                                        id
                                        by
                                        text
                                        feedback-url))))

(defun feedback-notification-email-text (id by text feedback-url)
  (strcat
"New User Feedback on Kindista"
#\linefeed #\linefeed
"Feedback ID: " id
#\linefeed #\linefeed
"Posted by: " by
#\linefeed #\linefeed
"Text: " text
#\linefeed #\linefeed
"Link: " feedback-url))

(defun feedback-notification-email-html
  (id by text feedback-url)
  (html-email-base
    (html
      (:p :style *style-p*
       (:h2 "New User Feedback on Kindista"))

      (:p :style *style-p* "Feedback ID: " (str id))

      (:p :style *style-p* "Posted by: " (str by))

      (:p :style *style-p* "Text: " (str text))

      (str (email-action-button feedback-url "Review and Respond"))
)))


