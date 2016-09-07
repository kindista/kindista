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

(defun donate-sidebar ()
  (html
    (:div :class "item right only"
      (:p (:a :class "blue" :href "/donate" "Donate to Kindista"))
      (:p :class "small" "Your tax-deductable donations support the operation and improvement of Kindista. Help make this website possible!"))))

(defun invite-sidebar (&key mobile)
  (with-user
    (when *user*
      (html
        (:div :class (s+ "invite item right" (unless mobile " only"))
          (:a :class "blue" :href "/invite" "Invite People")
          (:p :class "small" "Want to express gratitude for your friends? It's easy to invite them to join Kindista. "
              (:a :href "/faq#how-do-invitations-work" "Learn how invitations work") "."))))))

(defun facebook-signup-sidebar ()
  (when (and *user*
             (not *productionp*) ; hide until blog post
             (not (getf *user* :fb-token))
             (not (getf *user* :fb-link-active)))
    (html
      (:div :class "item right"
       (str (facebook-sign-in-button :redirect-uri "settings/social"
                                     :button-text "Facebook Activation"))
       (:p :class "small" "Activating Facebook on Kindista enables you to share your Offers, Requests, and Gratitude with your Facebook friends. Spread the love!")))))

(defun events-sidebar (&aux (count 6))
  (multiple-value-bind (events featured-events)
    (local-upcoming-events)
    (html
      (:div :class "item right only event"
       (:h2 "Upcoming Events")
       (unless events
         (htm
           (:p :class "small"
            "There are not any events posted in your area at this time. "
            (unless (= (user-distance) 0) "To see more events, increase the \"show activity within\" distance."))))
       (:a :class "add" :href "/events/new" "+add an event")
       (when featured-events
         (htm
           (:div :class "featured events item"
             (:div :class "featured header"
               (:h3 (str (pluralize featured-events
                                   "Featured Event"
                                   :hidenum t))))
            (dolist (fevent featured-events)
              (str (event-activity-item fevent
                                        :sidebar t
                                        :featuredp t
                                        :truncate t))))))
       (str (upcoming-events-html
              (remove-if (lambda (result) (find result featured-events))
                         events)
                         :count count
                         :paginate nil
                         :sidebar t))))))

(defparameter *sharing-guide-sidebar-html*
  (markdown-file (s+ +markdown-path+ "sharing-guide-sidebar.md")))

(defun sharing-guide-sidebar ()
  (html
    (:div :class "item right only sharing-guidelines"
     (:h2 "Sharing on Kindista")
     (:div :class "small"
       (str *sharing-guide-sidebar-html*)))))
