;;; Copyright 2016 CommonGoods Network, Inc.
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

;; At some point we should move /send-all-reminders /send-inventory-digest
;; /schedule-metric-system-timer and /automatic-invitation-reminders to the
;; scheduler-loop and remove them from the crontab

(defvar *scheduler-thread* nil)

(defvar *scheduler-timer* nil)

(defun stop-scheduler-thread ()
  (when (and *scheduler-thread* (thread-alive-p *scheduler-thread*))
    (terminate-thread *scheduler-thread*))
  (setf *scheduler-thread* nil))

(defun scheduler-loop (&aux (now (get-universal-time)))
  (flet ((call (route)
          (http-request (s+ +base-url+ route))
          (sleep 180)))
    (loop
      (ignore-errors
        (when (< (or *scheduler-timer*
                     ;; schedule for 5 min after system start up
                     (setf *scheduler-timer* (+ now 300)))
                 now)
          (setf *scheduler-timer* (+ now 3600))
          ;; (get-inventory-expriation-reminders)
          (call "inventory-expiration-reminders")
          (call "inventory-refresh"))))))

