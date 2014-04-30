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

(defvar *timeline-index* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))

(defun timeline-insert (userid result)
  "insert objectid at time into userid's timeline and sort"

  (with-locked-hash-table (*timeline-index*)
    (asetf (gethash userid *timeline-index*)
           (safe-sort (cons result it)
                 #'> :key #'result-time))))

(defun sort-timeline (userid)
  (with-locked-hash-table (*timeline-index*)
    (asetf (gethash userid *timeline-index*)
           (safe-sort it #'> :key #'result-time))))

(defun timeline-remove (userid result)
  "insert objectid at time into userid's timeline and sort"

  (with-locked-hash-table (*timeline-index*)
    (asetf (gethash userid *timeline-index*)
           (remove result it))))
