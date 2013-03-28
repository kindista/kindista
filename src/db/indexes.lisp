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

(defvar *geo-index-index* (make-hash-table :test 'equal :synchronized t :size 500 :rehash-size 1.25))
(defvar *activity-geo-index* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))
(defvar *event-geo-index* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))
(defvar *activity-person-index* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))
(defvar *person-invitation-index* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))
(defvar *resource-index* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))
(defvar *resource-geo-index* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))
(defvar *resource-stem-index* (make-hash-table :test 'equalp :synchronized t :size 500 :rehash-size 1.25))
(defvar *request-index* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))
(defvar *request-geo-index* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))
(defvar *request-stem-index* (make-hash-table :test 'equalp :synchronized t :size 500 :rehash-size 1.25))
(defvar *people-geo-index* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))
(defvar *followers-index* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))
(defvar *love-index* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))
(defvar *comment-index* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))
(defvar *gratitude-index* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))
(defvar *person-alias-index* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))
(defvar *metaphone-index* (make-hash-table :test 'equalp :synchronized t :size 500 :rehash-size 1.25))
(defvar *email-index* (make-hash-table :test 'equalp :synchronized t :size 500 :rehash-size 1.25))
(defvar *username-index* (make-hash-table :test 'equalp :synchronized t :size 500 :rehash-size 1.25))
