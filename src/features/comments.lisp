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

(defun create-comment (&key on (by *userid*) text)
  (let ((id (insert-db (list :type :comment
                             :on on
                             :by by
                             :text text
                             :created (get-universal-time)))))
    (modify-db on :latest-comment id)))

(defun latest-comment (id)
  (or (getf (db id) :latest-comment) 0))

(defun index-comment (id data)
  (push id (gethash (getf data :on) *comment-index*)))

(defun comments (id)
  (gethash id *comment-index*))
