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

(defvar *indexes* ())

(defmacro defindex (name &rest flags)
  `(progn
     (unless (member (quote ,name) *indexes*)
       (push (quote ,name) *indexes*))
     (defvar ,name (make-hash-table :synchronized t ,@flags))))

(defun clear-indexes ()
  (clrhash *db-results*)
  (clrhash *db-messages*)
  (dolist (index (remove (quote *user-tokens-index*)
                         *indexes*))
    (clrhash (symbol-value index)))
  (setf *active-people-index* ()
        *blog-index* ()
        *completed-transactions-index* ()
        *event-index* ()
        *feedback-index* ()
        *global-matchmaker-requests-index* ()
        *invitation-reminder-timer-index* ()
        *inventory-expiration-timer-index* ()
        *inventory-refresh-timer-index* ()
        *invite-request-index* ()
        *recent-activity-index* ()
        *requests-without-matchmakers-index* ()
        ))

(defindex *account-inactive-offer-index*)
(defindex *account-inactive-request-index*)
(defindex *activity-geo-index*)
(defindex *comment-index*)
(defindex *email-index* :test 'equalp)
(defindex *banned-emails-index* :test 'equalp)
(defindex *event-geo-index*)
(defindex *event-stem-index* :test 'equalp)
(defindex *followers-index*)
(defindex *geo-index-index* :test 'equal)
(defindex *gratitude-index*)
(defindex *gratitude-invitation-index*) ;key=gratitude-id, v=invitation-id
;; *gratitude-invitation-index* is not yet being used.
;; it is only useful if we need to find out what invitation a gratitude
;; is associated with (since the gratitude subject id isn't determined
;; until the subject joins kindista).
(defindex *gratitude-results-index*)
(defindex *group-membership-requests-index*) ;(personid . requestid)
(defindex *group-membership-invitations-index*) ;(personid . invitationid)
(defindex *group-privileges-index*)
(defindex *group-members-index*)
(defindex *group-messages-index*)
(defindex *groups-geo-index*)
(defindex *invited-index*)
(defindex *invitation-index* :test 'equalp)
(defindex *love-index*)
(defindex *metaphone-index* :test 'equalp)
(defindex *pending-person-items-index*)
(defindex *pending-gratitude-index*)
;; key=account-id, val=(:offers (list (result . transaction-id))
;;                      :requests (list (result . transaction-id)))

(defindex *people-geo-index*)
(defindex *person-alias-index*)
(defindex *person-mailbox-index*)
(defindex *person-invitation-index*)
(defindex *person-notification-index*) ;doesn't appear to be used yet
(defindex *person-suggested-connections-index*)
(defindex *profile-activity-index*)
(defindex *account-inventory-matches-index*) ;k=person/groupid v=(:requests :offers)
(defindex *request-geo-index*)
(defindex *request-index*) ;should be called "person-request-index"
(defindex *request-stem-index* :test 'equalp) ;k=stem v=(:title :details :tags)
(defindex *matchmaker-requests-geo-index*)
(defindex *offer-geo-index*)
(defindex *offer-index*) ;should be called "person-offer-index"
(defindex *offer-stem-index* :test 'equalp) ;k=stem v=(:title :details :tags)
(defindex *offers-with-matching-requests-index*)
(defindex *username-index* :test 'equalp)
(defindex *eventname-index* :test 'equalp)
(defindex *user-tokens-index*)

(defvar *active-people-mutex* (make-mutex))
(defvar *active-people-index* ())
(defvar *blog-mutex* (make-mutex))
(defvar *blog-index* ())
(defvar *completed-transactions-mutex* (make-mutex))
(defvar *completed-transactions-index* ())
(defvar *event-mutex* (make-mutex))
(defvar *event-index* ())
(defvar *feedback-mutex* (make-mutex))
(defvar *feedback-index* ())
(defvar *global-matchmaker-requests-mutex* (make-mutex))
(defvar *global-matchmaker-requests-index* ())
(defvar *invitation-reminder-timer-mutex* (make-mutex))
(defvar *invitation-reminder-timer-index* ())
(defvar *inventory-expiration-timer-mutex* (make-mutex))
(defvar *inventory-expiration-timer-index* ())
(defvar *inventory-refresh-timer-mutex* (make-mutex))
(defvar *inventory-refresh-timer-index* ())
(defvar *invite-request-mutex* (make-mutex))
(defvar *invite-request-index* ())
(defvar *recent-activity-mutex* (make-mutex))
(defvar *recent-activity-index* ())
(defvar *requests-without-matchmakers-mutex* (make-mutex))
(defvar *requests-without-matchmakers-index* ())
