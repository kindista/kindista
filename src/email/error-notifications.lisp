;;; Copyright 2012-2015 CommonGoods Network, Inc.
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

(defun send-error-notification-email (&key on userid url data)
  (cl-smtp:send-email +mail-server+
                      "Kindista <noreply@kindista.org>"
                      (if *productionp*
                        "errors@kindista.org"
                        *error-message-email*)
                      "Notifying humans, an error has occured!"
                      (error-notification-email-text on userid url data)))

(defun error-notification-email-text (on userid url data)
(strcat
"An error has occured..."
#\linefeed #\linefeed
"Url: " url
#\linefeed #\linefeed
"What was happening: " on
#\linefeed #\linefeed
"Affected user: " userid
#\linefeed #\linefeed
"Affected data: "
#\linefeed
data
))


