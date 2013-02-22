; Copyright 2013 CommonGoods Network, Inc.
;
; This file is part of Kindista.
;
; Kindista is free software: you can redistribute it and/or modify
; it under the terms of the GNU Affero General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; Kindista is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU Affero General Public License for more details.
;
; You should have received a copy of the GNU Affero General Public License
; along with Kindista.  If not, see <http://www.gnu.org/licenses/>.

(in-package :kindista)

(defvar *metrics-lock* (make-mutex :name "metrics log"))
(defvar *metrics-log*  (open (s+ +db-path+ "metrics")
                             :if-exists :append
                             :if-does-not-exist :create
                             :direction :output)) 

(defstruct metric
           ;;;;;;
           type
           timestamp
           text
           user
           ip)


(defun log-metric (metric)
  (with-mutex (*metrics-lock*)
    (with-standard-io-syntax
      ; probably need an error handler in here in case the log gets moved
      (prin1 metric *metrics-log*)
      (fresh-line *metrics-log*))
    (fsync *metrics-log*)))
