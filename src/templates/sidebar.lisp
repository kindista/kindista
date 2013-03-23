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

(defun donate-sidebar ()
  (html
    (:div :class "item"
      (:h3 (:a :href "/donate" "Help spread the sharing!"))
      (:p "Help Kindista grow big and strong by responding to our " (:a :href "/group/kindista/requests" "requests") " or " (:a :href "/donate" "making a donation") "?"))))

(defun invite-sidebar ()
  (with-user
    (when *user*
      (let ((invitations (available-invitation-count *userid*)))
        (html
          (:div :class "item right"
            (:h3 (:a :href "/invite" "Invite friends"))
            (:p "Kindista is invitation-only. As a Kindista member, you can invite people you know to join. ") 
            (:p "You currently have " (:strong (str invitations)) " available invitations.") 
            (when (> invitations 0) 
              (htm (:a :class "anchor-button" :href "/invite" "Invite Friends"))) 
            (:a :href "/help/faqs#how-do-invitations-work" "How does this work?")))))))

