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

(defvar *notice*) ; used as a dynamic variable for notice handlers
(defvar *notice-thread* nil)
(defvar *notice-mailbox* (make-mailbox :name "notice reader"))
(defvar *notice-handlers* (make-hash-table :synchronized t :size 100 :test 'eq))

(defun notice (type &rest rest 
                    &key (time (get-universal-time))
                         (userid *userid*)
                         (ip (when (boundp 'hunchentoot:*request*) (header-in* :x-real-ip)))
                    &allow-other-keys)
  (send-message *notice-mailbox* `(,type ,time ,userid ,ip ,@rest)))

(defun add-notice-handler (notice thunk)
  (with-locked-hash-table (*notice-handlers*)
    (push thunk (gethash notice *notice-handlers*))))

(defun test-handler ()
  (pprint *notice*)
  (terpri))

(defun notice-thread-loop ()
  (loop
    (let ((*notice* (receive-message *notice-mailbox*)))
      (if (eq *notice* 'exit)
        (return)
        (progn
          (dolist (handler (gethash (first *notice*) *notice-handlers*))
            (funcall handler))
          (dolist (handler (gethash :all *notice-handlers*))
            (funcall handler)))))))

(defun start-notice-thread ()
  (if (or (not *notice-thread*)
          (and *notice-thread* (not (thread-alive-p *notice-thread*))))
    (setf *notice-thread* (make-thread #'notice-thread-loop))))

(defun stop-notice-thread ()
  (when *notice-thread*
    (send-message *notice-mailbox* 'exit)
    (join-thread *notice-thread*) 
    (setf *notice-thread* nil)))
