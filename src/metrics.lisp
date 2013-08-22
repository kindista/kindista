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

(defun get-admin-metrics ()
  (require-admin
    (standard-page
      "Admin"
       (html
         (:p (:a :href "/admin" "back to admin"))
         (:img :src "/admin/metrics/metrics.png")))))

(defun get-admin-metrics-png ()
  (setf (content-type*) "image/png")
  (with-output-to-string (str)
    (with-chart (:line 600 600)
      (add-series "Active Kindista Accounts"
        (active-kindista-accounts-over-time))
        (set-axis :y "Accounts")
        (set-axis :x "Date")
        (save-stream str))))

(defun active-kindista-accounts-over-time ()
  (let ((day nil))
    (loop for i
          from (floor (/ (parse-datetime "05/01/2013") +day-in-seconds+))
          to (floor (/ (get-universal-time) +day-in-seconds+))
          do (setf day (* i +day-in-seconds+))
          collect (list
                    (multiple-value-bind (time date formatted)
                        (humanize-exact-time day)
                      (declare (ignore time formatted))
                      date)
                    (active-people day)))))

(defun basic-chart ()
  (with-open-file (file (s+ *metrics-path* "active-accounts")
                        :direction :output
                        :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*print-pretty* t))
        (prin1
          (active-kindista-accounts-over-time)
          file)))))

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
