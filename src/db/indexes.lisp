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

(defvar *indexes* ())

(defmacro defindex (name &rest flags)
  `(progn
     (unless (member (quote ,name) *indexes*)
       (push (quote ,name) *indexes*))
     (defvar ,name (make-hash-table :synchronized t ,@flags))))

(defun clear-indexes ()
  (dolist (index *indexes*)
    (clrhash (symbol-value index)))
  (setf *feedback-index* ()
        *event-index* ()
        *invitation-reminder-timer-index* ()
        *invite-request-index* ()
        *recent-activity-index* ()
        *active-people-index* ()))

(defindex *activity-geo-index*)
(defindex *comment-index*)
(defindex *email-index* :test 'equalp)
(defindex *banned-emails-index* :test 'equalp)
(defindex *event-geo-index*)
(defindex *event-stem-index* :test 'equalp)
(defindex *followers-index*)
(defindex *geo-index-index* :test 'equal)
(defindex *gratitude-index*)
(defindex *gratitude-results-index*)
(defindex *group-membership-requests-index*) ;(personid . requestid)
(defindex *group-priviledges-index*)
(defindex *groups-geo-index*)
(defindex *invited-index*)
(defindex *invitation-index* :test 'equalp)
(defindex *love-index*)
(defindex *metaphone-index* :test 'equalp)
(defindex *pending-person-items-index*)
(defindex *people-geo-index*)
(defindex *person-alias-index*)
(defindex *person-conversation-index*)
(defindex *person-mailbox-index* :test 'equalp)
(defindex *person-invitation-index*)
(defindex *person-notification-index*)
(defindex *profile-activity-index*)
(defindex *request-geo-index*)
(defindex *request-index*) ;should be called "person-request-index"
(defindex *request-stem-index* :test 'equalp)
(defindex *offer-geo-index*)
(defindex *offer-index*) ;should be called "person-offer-index"
(defindex *offer-stem-index* :test 'equalp)
(defindex *username-index* :test 'equalp)

(defvar *recent-activity-mutex* (make-mutex))
(defvar *recent-activity-index* ())
(defvar *invite-request-mutex* (make-mutex))
(defvar *invite-request-index* ())
(defvar *invitation-reminder-timer-mutex* (make-mutex))
(defvar *invitation-reminder-timer-index* ())
(defvar *event-mutex* (make-mutex))
(defvar *event-index* ())
(defvar *feedback-mutex* (make-mutex))
(defvar *feedback-index* ())
(defvar *active-people-mutex* (make-mutex))
(defvar *active-people-index* ())
