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

(defun send-invite-request-notification-email (id)
 (cl-smtp:send-email +mail-server+
                     "Kindista <noreply@kindista.org>"
                     "feedback@kindista.org"
                     "New Kindista Invitation Request" 
                     (invite-request-notification-email-text id)))

(defun invite-request-notification-email-text (id)
(let ((request (db id)))
(strcat
"Invitation Request ID: " id
"

Requested by: " (getf request :name)
"

Email: " (getf request :email)
"

Resources offered: 
"(getf request :offering)
"

Help offered: 
" 
(if (getf request :events) "help with events" "")
"
"
(if (getf request :invite) "invite friends" "")
"
"
(if (getf request :resources) "post resources" "")
"
"
(if (getf request :gratitude) "post gratitude" ""))))


