
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

(defun send-transaction-action-notification-email
  (transaction-id
   log-event
   &aux (transaction (db transaction-id))
        (transaction-initiator (getf transaction :by))
        (inventory-item (db (getf transaction :on)))
        (inventory-type (getf inventory-item :type))
        (inventory-by (getf inventory-item :by))
        (sender-party (getf log-event :party))
   )
  (cl-smtp:send-email +mail-server+
                         "DoNotReply <noreply@kindista.org>"
                         (car (db recipient :emails))
                         (s+ host-name
                             " has invited you to join their group, "
                             group-name
                             ", on Kindista")
                         (group-membership-invitation-notification-email-text
                           host-name
                           group-id
                           group-name)
                         :html-message (group-membership-invitation-notification-email-html
                                         from
                                         group-id
                                         group-name))
  )
