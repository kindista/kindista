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
      (:p (:a :class "blue" :href "/donate" "Donate to Kindista"))
      (:p :class "small" "Your tax-deductable donations support the operation and improvement of Kindista. Help make this website possible!"))))

(defun invite-sidebar ()
  (with-user
    (when *user*
      (html
        (:div :class "item right"
          (:a :class "blue" :href "/invite" "Invite People")
          (:p :class "small" "Want to express gratitude for your friends? It's easy to invite them to join Kindista. "
              (:a :href "/faq#how-do-invitations-work" "Learn how invitations work") "."))))))

(defun events-sidebar ()
  (let ((events (local-upcoming-events :count 6 :paginate nil :sidebar t)))
    (html
      (:div :class "item right only event"
       (:h2 "Upcoming Events")
       (if (string= events "")
         (htm
           (:p :class "small"
            "There are not any events posted in your area at this time. "
            (unless (= (user-distance) 0) "To see more events, increase the \"show activity within\" distance."))))
       (:a :class "add" :href "/events/new" "+add an event")
       (str events)
       ))))

(defparameter *sharing-guide-sidebar-html*
  (markdown-file (s+ +markdown-path+ "sharing-guide-sidebar.md")))

(defun sharing-guide-sidebar ()
  (html
    (:div :class "item right only sharing-guidelines"
     (:h2 "Sharing on Kindista")
     (:div :class "small"
       (str *sharing-guide-sidebar-html*)))))
