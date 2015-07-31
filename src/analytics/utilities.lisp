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

(defun transactions-report ()
  (with-standard-io-syntax
    (with-open-file (s (merge-pathnames *metrics-path* "transaction-report")
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (let ((*print-readably* nil))
        (format s "######### Completed Transactions #########~%" ))
        (dolist (transaction *completed-transactions-index*)
          (let* ((gratitude-id (getf transaction :gratitude))
                 (gratitude (db gratitude-id))
                 (recipient-id (getf gratitude :author))
                 (recipient (db recipient-id))
                 (item-id (getf transaction :on))
                 (item (db item-id)))
            (format s "~A~%" (strcat "DATE: " (humanize-exact-time (getf transaction :time)
                                                     :detailed t))
                   )
            (format s "~A~%" (strcat "LOCATION: " (getf recipient :address)))
            (format s "~A~%" (strcat "RECIPIENT: " (getf recipient :name)
                                       " (" recipient-id ")")
                   )
            (format s "~A~%" (strcat "GIFT GIVER(S): "
                       (format nil
                               *english-list*
                               (loop for id in (getf gratitude :subjects)
                                     collect (strcat (db id :name)
                                                     " ("
                                                     id
                                                     ")"))))
                   )
            (format s "~A~%" (strcat (symbol-name (getf item :type))
                       " TITLE: "
                       (getf item :title))
                   )
            (format s "~A~%" (strcat (symbol-name (getf item :type)) " DETAILS: ") )
            (format s "~A~%" (getf item :details))
            (format s "~A~%" (strcat (symbol-name (getf gratitude :type)) " TEXT:") )
            (format s "~A~%" (getf gratitude :text))
            (format s "~A~%" "------------------" ))))))

(defun outstanding-invitations-count ()
  (hash-table-count *invitation-index*))

(defun gratitude-texts ()
  (loop for result in (hash-table-values *db-results*)
        when (eq (result-type result) :gratitude)
        collect (cons (result-id result) (db (result-id result) :text))))

(defun user-email-subscriptions-analysis
  (&aux (active-users 0)
        (message 0)
        (reminders 0)
        (expired-invites 0)
        (blog 0)
        (kindista 0)
        (all 0)
        (any 0))
  (dolist (userid *active-people-index*)
    (let ((user (db userid)))
      (incf active-users)
      (when (getf user :notify-message) (incf message))
      (when (getf user :notify-reminders) (incf reminders))
      (when (getf user :notify-expired-invites) (incf expired-invites))
      (when (getf user :notify-blog) (incf blog))
      (when (getf user :notify-kindista) (incf kindista))
      (when (and (getf user :notify-reminders)
                 (getf user :notify-blog)
                 (getf user :notify-kindista))
        (incf all))
      (when (or (getf user :notify-reminders)
                (getf user :notify-blog)
                (getf user :notify-kindista))
        (incf any))))
  (list :active active-users
        :message message
        :reminders reminders
        :expired-invites expired-invites
        :blog blog
        :kindista kindista
        :all all
        :any any
        ))

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


(defun update-metrics-chart
  (&key (start-month 1)
        (start-year 2014)
   &aux (chart-data)
        (now (local-time:now))
        (current-year (timestamp-year now))
        (current-month (timestamp-month now)))

  (loop for year from start-year to current-year
        do (dolist (month '("01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12"))
             (let ((file (pathname (strcat *metrics-path* year "/" month "/monthly-summary")))
                   (time (encode-universal-time 0 0 1 15 (parse-integer month) year))
                   (file-data))
               (when (and (file-exists-p file)
                          (or (and (= year start-year)
                                   (>= (parse-integer month) start-month))
                              (> year start-year))
                          (or (< year current-year)
                              (< (parse-integer month) current-month)
                              (and (= (parse-integer month) current-month)
                                   (= (local-time:timestamp-day now)
                                      (days-in-month now)))))
                 (with-standard-io-syntax
                   (with-open-file (summary file :direction :input)
                     (setf file-data (read summary))))

                 (flet ((record-data (data-type &aux (data (getf file-data data-type)))
                          (push (list time (if (listp data)
                                             (length data)
                                             data))
                                (getf chart-data data-type))))
                   (mapcar #'record-data (list :active-users
                                               :used-search
                                               :new-offers
                                               :new-requests
                                               :got-offers
                                               :got-requests
                                               :messages-sent
                                               :completed-transactions)))))))

  (with-chart (:line 1200 600)
    (doplist (key val chart-data)
      (add-series (string-capitalize
                    (string-downcase
                      (ppcre:regex-replace-all "-" (symbol-name key) " ")))
                  val))
    (set-axis :y "" :data-interval 10)
    (set-axis :x "Month"
              :label-formatter #'format-month-for-activity-charts
              )
   ;(add-title "Kindista Usage over time")
   ;(add-feature :label)
    (save-file (pathname (strcat *metrics-path* "/all-time-metrics.png")))))


(defun create-past-monthly-activity-reports (years)
  (dolist (year years)
    (loop for month from 1 to 12
          for dir = (strcat *metrics-path* year
                                           "/"
                                           (if (< (/ month 10) 1)
                                             (strcat "0" month)
                                             (strcat month))
                                           "/")
          when (cl-fad::directory-exists-p dir)
          do (monthly-activity-report month year))))

(defun send-progress-report-email (title)
  (cl-smtp:send-email
    +mail-server+
    "Kindista <info@kindista.org>"
    "Progress Reports <progress-reports@kindista.org>"
    title
    (strcat
      "Please see the attached file for a chart of various metrics we are collecting for Kindista usage. "
      #\linefeed #\linefeed
      "Please note: Due to a bug in the graphing library we are using, some of the dates may be repeated on the x-axis. "
      "The data points should be correct and there is one data point per month for each metric . "
      "Also, we didn't start collecting metrics for new offers/requests until July/2015.")
    :attachments (merge-pathnames (s+ +db-path+ "metrics/") "all-time-metrics.png")))
