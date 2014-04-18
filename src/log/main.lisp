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

(defvar *log-lock* (make-mutex :name "log stream"))
(defvar *log-stream*  (open (concatenate 'string +db-path+ "log")
                            :if-exists :append
                            :if-does-not-exist :create
                            :direction :output)) 

(defun log-notice ()
  (with-mutex (*log-lock*)
    (with-standard-io-syntax
      ; probably need an error handler in here in case the log gets moved
      (prin1 *notice* *log-stream*)
      (fresh-line *log-stream*))))

