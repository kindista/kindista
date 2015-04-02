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

(defun get-admin-metrics-png ()
  (setf (content-type*) "image/png")
  (with-chart (:line 600 600)
   (add-series "Active Kindista Accounts"
     (active-kindista-accounts-over-time))
     (set-axis :y "Accounts" :data-interval 20)
     (set-axis :x "Date"
               :label-formatter #'chart-date-labels
               :angle 90)
     (save-stream (send-headers))))

(defun chart-date-labels (timecode)
  (multiple-value-bind (time date-name formatted-date)
    (humanize-exact-time timecode)
    (declare (ignore time date-name))
    formatted-date))

(defun active-kindista-accounts-over-time ()
  (let ((day nil))
    (loop for i
          from (floor (/ (parse-datetime "05/01/2013") +day-in-seconds+))
          to (floor (/ (get-universal-time) +day-in-seconds+))
          do (setf day (* i +day-in-seconds+))
          collect (list day (active-people day)))))

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

(defun completed-transactions-report
  (&optional (start (- (get-universal-time) (* +day-in-seconds+ 365)))
             (end (get-universal-time))
   &aux (transactions))

  (dolist (transaction (safe-sort *completed-transactions-index*
                                  #'<
                                  :key #'(lambda (transaction) (getf transaction :time))))
    (when (and (< (getf transaction :time) end)
               (> (getf transaction :time) start))
      (push transaction transactions)))

;; if we want to include gratitudes w/o transactions
; (setf transaction-gratitude-ids
;       (mapcar (lambda (transaction) (getf transaction :gratitude)) transactions))

; (setf orphan-gratitudes
;       (loop for result in (hash-table-values *db-results*)
;             when (and (eq (result-type result) :gratitude)
;                       (< (result-time result) end)
;                       (> (result-time result) start)
;                       (not (find (result-id result) transaction-gratitude-ids)))
;             collect result))

 (with-open-file (s (s+ +db-path+ "/metrics/transactions-report")
                    :direction :output
                    :if-exists :supersede)
   (with-standard-io-syntax
     (prin1 "######### Completed Transactions #########" s)
     (fresh-line s)
     (dolist (transaction transactions)
       (let* ((gratitude (db (getf transaction :gratitude)))
              (recipient (db (getf gratitude :author))))
         (format s "DATE: ~A" (humanize-exact-time (getf transaction :time) :detailed t))
         (fresh-line s)
         (format s "LOCATION: ~A" (getf recipient :address))
         (fresh-line s)
         (format s "RECIPIENT: ~A (~A)"
                        (getf recipient :name)
                        (getf gratitude :author))
         (fresh-line s)
         (format s "GIFT GIVER(S): ")
         (format s *english-list*
                    (mapcar (lambda (subject)
                              (strcat (db subject :name) " (" subject ")"))
                            (getf gratitude :subjects)))
         (fresh-line s)
         (let ((inventory-item (db (getf transaction :on))))
           (format s "~A text:" (symbol-name (getf inventory-item :type)))
           (fresh-line s)
           (prin1 (getf inventory-item :details) s)
           (fresh-line s))

         (let ((gratitude (db (getf transaction :gratitude))))
           (format s "GRATITUDE TEXT:")
           (fresh-line s)
           (prin1 (getf gratitude :text) s)))
       (fresh-line s)
       (format s "------------------------")
       (fresh-line s)
       ))))

(defun local-members (&key focal-point-id (distance 25))
"Provides values for a list of people within :distance (default=25 miles) 
of a given :focal-point-id (default=Eugene OR), followed by the length of that list.
Any id can be used as long as (getf id :lat/long) provides meaningful result."
  (let* ((focal-point-data (if (db focal-point-id :lat)
                             (db focal-point-id)
                             (db +kindista-id+)))
         (lat (getf focal-point-data :lat))
         (long (getf focal-point-data :long))
         (people (mapcar #'result-id
                   (geo-index-query *people-geo-index* lat long distance))))
    (values people (length people))))
