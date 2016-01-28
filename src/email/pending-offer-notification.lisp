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

(defun send-pending-offer-notification-email (id)
 (cl-smtp:send-email +mail-server+
                     "Kindista <noreply@kindista.org>"
                     "feedback@kindista.org"
                     "New Kindista Account to Review" 
                     (pending-offer-notification-email-text id)
                     :html-message (pending-offer-notification-email-html id)))

(defun pending-offer-notification-email-text (id)
(let* ((offer (db id))
       (userid (getf offer :by))
       (user (db userid)))
(strcat
"New Kindista user, " (getf user :name) " (ID:" userid "), posted a new offer."
#\linefeed #\linefeed
"Offer title: "
#\linefeed
(getf offer :title)
#\linefeed #\linefeed
"Offer details: "
#\linefeed
(getf offer :details)
#\linefeed #\linefeed
"Please review this user's activity as soon as possible and approve their Kindista membership if appropriate.")))

(defun pending-offer-notification-email-html
  (id
   &aux (offer (db id))
        (userid (getf offer :by))
        (user (db userid)))
  (html-email-base
    (html
      (:p :style *style-p*
       "New Kindista user, "
       (str (getf user :name))
       " (ID:"
       userid
       "), posted a new offer." )

      (:h2 (str (getf offer :title)))

      (:p :style *style-p*
       (str (getf offer :details)))

      (str (email-action-button (strcat *email-url* "admin/pending-accounts")
                                "Review Account for Approval"))

      (:p :style *style-p*
 "Please review this user's activity as soon as possible and approve their Kindista membership if appropriate."))))
