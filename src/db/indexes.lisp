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
     (defvar ,name (make-hash-table :synchronized t ,@flags))))
;     (unless (member (quote ,name) *indexes*)
;       (push (quote ,name) *indexes*)))))

(defun clear-indexes ()
  (dolist (index *indexes*)
    (clrhash (symbol-value index))))

(defindex *activity-geo-index*)
(defindex *activity-person-index*)
(defindex *comment-index*)
(defindex *email-index* :test 'equalp)
(defindex *event-geo-index*)
(defindex *followers-index*)
(defindex *geo-index-index* :test 'equal)
(defindex *gratitude-index*)
(defindex *love-index*)
(defindex *metaphone-index* :test 'equalp)
(defindex *people-geo-index*)
(defindex *person-alias-index*)
(defindex *person-conversation-index*)
(defindex *person-invitation-index*)
(defindex *request-geo-index*)
(defindex *request-index*)
(defindex *request-stem-index* :test 'equalp)
(defindex *resource-geo-index*)
(defindex *resource-index*)
(defindex *resource-stem-index* :test 'equalp)
(defindex *username-index* :test 'equalp)

