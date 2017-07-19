;;; Copyright 2017 CommonGoods Network, Inc.
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

(defun send-feedback-reply-notification-email (id)
  (let* ((reply (db id))
         (on (getf reply :on))
         (feedback (db on))
         (by (db (car (getf reply :by)) :name))
         (text (getf reply :text))
         (feedback-url (strcat *email-url* "feedback#" on)))
    (cl-smtp:send-email +mail-server+
                        "Kindista <feedback@@kindista.org>"
                        (first (db (getf feedback :by) :emails))
                        "A reply has been posted to the feedback you left on Kindista"
                        (feedback-reply-notification-email-text
                          by
                          text
                          feedback-url)
                        :html-message (feedback-reply-notification-email-html
                                        by
                                        text
                                        feedback-url))))

(defun feedback-reply-notification-email-text (by text feedback-url)
  (strcat
"A reply has been posted to feedback you left on Kindista"
#\linefeed #\linefeed
"Posted by: " by
#\linefeed #\linefeed
"Text: " text
#\linefeed #\linefeed
"Link: " feedback-url))

(defun feedback-reply-notification-email-html
  (by text feedback-url)
  (html-email-base
    (html
      (:p :style *style-p*
       (:h2 "A reply has been posted to feedback you left on Kindista"))

      (:p :style *style-p* "Posted by: " (str by))

      (:p :style *style-p* "Text: " (str text))

      (str (email-action-button feedback-url "View Feedback and Comment"))
)))


