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

(defclass metric-system ()
  ((thread :initarg :thread)
   (mailbox :initform (make-mailbox)
            :reader metric-system-mailbox)
   (timer :reader metric-system-timer)
   (active-today :initform (make-hash-table))
   (checked-mailbox :initform (make-hash-table))
   (used-search :initform (make-hash-table))
   (got-offers :initform (make-hash-table))
   (got-requests :initform (make-hash-table))
   (messages-sent :initform (make-hash-table))))

(defmethod initialize-instance :after ((system metric-system) &key)
  (setf (slot-value system 'timer) (make-timer #'(lambda ()
                                                   (send-message (metric-system-mailbox system) '(:daily))))))

(defmethod start ((system metric-system))
  (unless (and (slot-boundp system 'thread)
               (thread-alive-p (slot-value system 'thread)))
    (setf (slot-value system 'thread) (make-thread (fdefinition 'metric-system-loop) :arguments (list system)))
    (send-message (metric-system-mailbox system) '(:start))))

(defmethod stop ((system metric-system) &key soft)
  (declare (ignore soft))
  (when (thread-alive-p (slot-value system 'thread))
    (send-message (metric-system-mailbox system) '(:stop))))

(defun get-schedule-metric-system-timer ()
  (when (or (string= (header-in* :x-real-ip) "127.0.0.1")
            (string= (header-in* :x-real-ip) *local-ip-address*))
    (send-message (metric-system-mailbox (accessor-metric-system *acceptor*)) '(:daily))))

(defgeneric send-metric (system &rest message))

(defmethod send-metric ((system metric-system) &rest message)
  (send-message (metric-system-mailbox system) message))

(defun send-metric* (&rest message)
  (apply #'send-metric (acceptor-metric-system *acceptor*) message))

(defgeneric save-metrics (system))

(defmethod save-metrics ((system metric-system))
  (labels ((data (slot)
             (hash-table-keys (slot-value system slot))))
    (let* ((now (local-time:now))
           (dirname (strcat *metrics-path*
                            (with-output-to-string (str)
                              (local-time:format-timestring str
                                                            now
                                                            :format '((:year 4) #\/ (:month 2) #\/)))))
           (filename (strcat *metrics-path*
                             (with-output-to-string (str)
                               (local-time:format-timestring str
                                                             now
                                                             :format '((:year 4) #\/ (:month 2) #\/ (:day 2))))))
          (active-today (data 'active-today))
          (checked-mailbox (data 'checked-mailbox))
          (used-search (data 'used-search))
          (got-offers (data 'got-offers))
          (got-requests (data 'got-requests))
          (messages-sent (data 'messages-sent)))

      (when (file-exists-p filename)
        (ignore-errors
          (with-open-file (file filename :direction :input)
            (let ((data (read file)))
              (setf active-today (union active-today (getf data :active-today)))
              (setf checked-mailbox (union checked-mailbox (getf data :checked-mailbox)))
              (setf used-search (union used-search (getf data :used-search)))
              (setf got-offers (union got-offers (getf data :got-offers)))
              (setf got-requests (union got-requests (getf data :got-requests)))    
              (setf messages-sent (union messages-sent (getf data :messages-sent)))))))

      (ensure-directories-exist dirname)

      (multiple-value-bind (offers requests gratitudes)
          (sharing-items-count)
        (with-open-file (file filename :direction :output
                                       :if-exists :supersede)
          (with-standard-io-syntax
            (let ((*print-pretty* t))
              (prin1 (list :active-today active-today
                           :checked-mailbox checked-mailbox
                           :used-search used-search
                           :got-offers got-offers
                           :got-requests got-requests
                           :messages-sent messages-sent
                           :total-active-users (active-people)
                           :total-eugene-users (multiple-value-bind (people people-count)
                                                 (local-members)
                                                 (declare (ignore people))
                                                 people-count)
                           :total-requests requests
                           :total-offers offers
                           :total-gratitudes gratitudes)
                     file))
            (terpri)))))))



(defun metric-system-loop (metric-system)
  ;; run stop and start on the metric-system after adding a new metric
  ;; e.g. (stop (acceptor-metric-system *acceptor*))
  (labels ((record (slot id)
             (setf (gethash id (slot-value metric-system slot)) t))
           (clear (&rest slots)
             (dolist (slot slots)
               (clrhash (slot-value metric-system slot)))))
    (loop
      (ignore-errors
        (let ((message (receive-message (metric-system-mailbox metric-system))))
          (case (first message)
            (:start
              (let ((time (local-time:now)))
                (when (>= (local-time:timestamp-hour time) 10)
                  (local-time:adjust-timestamp! time (offset :day 1)))
                (local-time:adjust-timestamp! time
                                              (set :hour 10)
                                              (set :minute 0)
                                              (set :sec 0))
                (schedule-timer (slot-value metric-system 'timer)
                                (local-time:timestamp-to-universal time)
                                :absolute-p t)))
            (:stop
              (unschedule-timer (slot-value metric-system 'timer))
              (save-metrics metric-system)
              (return-from metric-system-loop))
            (:active (record 'active-today (second message)))
            (:checked-mailbox (record 'checked-mailbox (second message)))
            (:used-search (record 'used-search (second message)))
            (:got-offers (record 'got-offers (second message)))
            (:got-requests (record 'got-requests (second message)))
            (:message-sent (record 'messages-sent (second message)))
            (:daily
              (save-metrics metric-system)
              (clear 'active-today 'checked-mailbox 'used-search 'got-offers 'got-requests)
              (schedule-timer (slot-value metric-system 'timer)
                              (local-time:timestamp-to-universal
                                (local-time:adjust-timestamp (local-time:now)
                                                             (offset :day 1)
                                                             (set :hour 10)
                                                             (set :minute 0)
                                                             (set :sec 0)))
                              :absolute-p t))))))))
