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
    (:div :class "item right only"
      (:h3 "Support Kindista")
      (:p (:a :class "yes" :href "/donate" "Donate"))
      (:p "Your donations are tax-deductable to the extent permitted by law. Kindista is a project of CommonGoods Network, an Oregon 501(c)(3) non-profit."))))

(defun invite-sidebar ()
  (with-user
    (when *user*
      (html
        (:div :class "item right"
          (:h3 (:a :href "/invite" "Invite friends"))
          (:p "Kindista is invitation-only. As a Kindista member, you can invite people you know to join. ")
          (:br)
          (:a :class "yes" :href "/invite" "Invite Friends")
          (:a :href "/help/faqs#how-do-invitations-work" "How does this work?"))))))

