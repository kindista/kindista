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

; number of active users (&key date location distance)
; number of offers (&key date location distance)
; number of requests (&key date location distance)
; number of gratitudes (&key date location distance)
; number of conversations (&key date location distance)
; number of users who have commented on a conversation (&key period-start period-end)
; number of users who have used kindista (&key period-start period-end)

(defun active-people (&optional date)
  "Returns number of active accounts on Kindista.
   When a date is specified, returns the number of people who are active now
   and had signed up before midnight on that date."
  (if date
    (let ((time (cond
                  ((integerp date) date)
                  ((stringp date)
                   (+ +day-in-seconds+ (parse-datetime date))))))
      (length
        (loop for person in *active-people-index*
              when (< (db person :created) time)
              collect person)))
    (length *active-people-index*)))

(defun sharing-items-count (&optional date)
  "Returns number of current offers, requests, and gratitudes.
   When a date is specified, returns the number of current offers, requests,
   and gratitudes that had been posted before midnight on that date."
  (let ((offers 0)
        (requests 0)
        (gratitudes 0))
    (if date
      (let ((time (cond
                    ((integerp date) date)
                    ((stringp date)
                     (+ +day-in-seconds+ (parse-datetime date))))))
        (dolist (result (hash-table-values *db-results*))
          (case (result-type result)
            (:offer (when (< (db (result-id result) :created) time)
                      (incf offers)))
            (:request (when (< (db (result-id result) :created) time)
                        (incf requests)))
            (:gratitude (when (< (db (result-id result) :created) time)
                          (incf gratitudes))))))

      (dolist (result (hash-table-values *db-results*))
        (case (result-type result)
          (:offer (incf offers))
          (:request (incf requests))
          (:gratitude (incf gratitudes)))))

    (values offers requests gratitudes)))

(defun outstanding-invitations-count ()
  (hash-table-count *invitation-index*))


(defun gratitude-texts ()
  (loop for result in (hash-table-values *db-results*)
        when (eq (result-type result) :gratitude)
        collect (cons (result-id result) (db (result-id result) :text))))

(defun eugene-members ()
  (with-location
    (length
      (iter (for person in *active-people-index*)
        (let* ((person (db person))
               (lat (getf person :lat))
               (long (getf person :long)))
          (when (and lat
                     long
                     (< (air-distance *latitude* *longitude* lat long) 25))
            (collect person)))))))
