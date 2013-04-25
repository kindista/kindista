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

(defun send-feedback-notification-email (id)
  (let* ((feedback (db id))
         (by (db (getf feedback :by) :name))
         (text (getf feedback :text)))
    (cl-smtp:send-email +mail-server+
                        "Kindista <noreply@kindista.org>"
                        "feedback@kindista.org"
                        (s+ "Kindista feedback from " by)
                        (feedback-notification-email-text id
                                                          by
                                                          text))))

(defun feedback-notification-email-text (id by text)
  (strcat
"Feedback ID: " id
"

Posted by: " by
"

Text:
" text))


