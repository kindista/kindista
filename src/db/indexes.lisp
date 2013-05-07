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
  (setf *old-inventory-index* ())
  (setf *feedback-index* ())
  (setf *recent-activity-index* ()))

(defindex *activity-geo-index*)
(defindex *activity-person-index*)
(defindex *comment-index*)
(defindex *email-index* :test 'equalp)
(defindex *event-geo-index*)
(defindex *followers-index*)
(defindex *geo-index-index* :test 'equal)
(defindex *gratitude-index*)
(defindex *invited-index*)
(defindex *love-index*)
(defindex *metaphone-index* :test 'equalp)
(defindex *people-geo-index*)
(defindex *person-alias-index*)
(defindex *person-conversation-index*)
(defindex *person-invitation-index*)
(defindex *person-notification-index*)
(defindex *request-geo-index*)
(defindex *request-index*)
(defindex *request-stem-index* :test 'equalp)
(defindex *offer-geo-index*)
(defindex *offer-index*)
(defindex *offer-stem-index* :test 'equalp)
(defindex *username-index* :test 'equalp)

(defvar *recent-activity-mutex* (make-mutex))
(defvar *recent-activity-index* ())
(defvar *invite-request-mutex* (make-mutex))
(defvar *invite-request-index* ())
(defvar *old-inventory-mutex* (make-mutex))
(defvar *old-inventory-index* ())
(defvar *feedback-mutex* (make-mutex))
(defvar *feedback-index* ())
