;;; Copyright 2012-2021 CommonGoods Network, Inc.
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

;(defun get-admin-active-accounts-png ()
;  (setf (content-type*) "image/png")
;  (with-chart (:line 600 600)
;   (add-series "Active Kindista Accounts"
;     (active-kindista-accounts-over-time))
;     (set-axis :y "Accounts" :data-interval 20)
;     (set-axis :x "Date"
;               :label-formatter #'chart-date-labels
;               :angle 90)
;     (save-stream (send-headers))))

;(defun get-admin-metrics-chart-png ()
;  (handle-static-file (merge-pathnames *metrics-path* "kindista-metrics-chart.png")
;                      "image/png"))

;(defun chart-date-labels (timecode)
;  (multiple-value-bind (time date-name formatted-date)
;    (humanize-exact-time timecode)
;    (declare (ignore time date-name))
;    formatted-date))

(defun active-kindista-accounts-over-time ()
  (let ((day nil))
    (loop for i
          from (floor (/ (parse-datetime "05/01/2013") +day-in-seconds+))
          to (floor (/ (get-universal-time) +day-in-seconds+))
          do (setf day (* i +day-in-seconds+))
          collect (list day (active-people day)))))

(defun most-active-users (&key (count 20) time-period &aux users active-users)
  (dolist (userid *active-people-index*)
    (push (cons userid
                (length (remove-if-not (lambda (type) (eq type :gratitude))
                                       (gethash userid *profile-activity-index*)
                                       :key #'result-type)))
          users))
  (setf users (sort (copy-list users) #'> :key #'cdr))
  (setf active-users (mapcar (lambda (user) (list (db (car user) :name) :userid (car user) :total-gratitudes (cdr user)))
                             (subseq users 0 (- count 1))))
  active-users)

;(defun basic-chart ()
;  (with-open-file (file (s+ *metrics-path* "active-accounts")
;                        :direction :output
;                        :if-exists :supersede)
;    (with-standard-io-syntax
;      (let ((*print-pretty* t))
;        (prin1
;          (active-kindista-accounts-over-time)
;          file)))))

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
          (let* ((data (db (result-id result)))
                 (active-p (getf data :active)))
            (case (result-type result)
              (:offer (when (and active-p
                                 (< (db (result-id result) :created) time))
                        (incf offers)))
              (:request (when (and active-p
                                   (< (db (result-id result) :created) time))
                          (incf requests)))
              (:gratitude (when (< (db (result-id result) :created) time)
                            (incf gratitudes)))))))

      (dolist (result (hash-table-values *db-results*))
        (let* ((data (db (result-id result)))
               (active-p (getf data :active)))
          (case (result-type result)
            (:offer (when active-p (incf offers)))
            (:request (when active-p (incf requests)))
            (:gratitude (incf gratitudes))))))

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
     (prin1 (strcat (length transactions) " completed transactions"))
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


;(defun update-metrics-chart
;  (&key (start-month 1)
;        (start-year (- (current-year) 3))
;   &aux (chart-data)
;        (now (local-time:now))
;        (current-year (timestamp-year now))
;        (current-month (timestamp-month now)))

;  (loop for year from start-year to current-year
;        do (dolist (month '("01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12"))
;             (let ((file (pathname (strcat *metrics-path* year "/" month "/monthly-summary")))
;                   (time (encode-universal-time 0 0 1 15 (parse-integer month) year))
;                   (file-data))
;               (when (and (file-exists-p file)
;                          (or (and (= year start-year)
;                                   (>= (parse-integer month) start-month))
;                              (> year start-year))
;                          (or (< year current-year)
;                              (< (parse-integer month) current-month)
;                              (and (= (parse-integer month) current-month)
;                                   (= (local-time:timestamp-day now)
;                                      (days-in-month now)))))
;                 (with-standard-io-syntax
;                   (with-open-file (summary file :direction :input)
;                     (setf file-data (read summary)))) (flet ((record-data (data-type &aux (data (getf file-data data-type)))
;                          (push (list time (if (listp data)
;                                             (length data)
;                                             data))
;                                (getf chart-data data-type))))
;                   (mapcar #'record-data (list :active-users
;                                               :used-search
;                                               :new-offers
;                                               :new-requests
;                                               :got-offers
;                                               :got-requests
;                                              ;:messages-sent
;                                               :completed-transactions)))))))

;  (with-open-file (s (strcat *metrics-path* "/kindista-metrics-chart.png")
;                     :direction :output
;                     :if-does-not-exist :create
;                     :if-exists :supersede
;                     :element-type '(unsigned-byte 8))
;    (with-chart (:line 1200 600)
;      (doplist (key val chart-data)
;        (add-series (string-capitalize
;                      (string-downcase
;                        (ppcre:regex-replace-all "-" (symbol-name key) " ")))
;                    val))
;      (set-axis :y "" :data-interval 10)
;      (set-axis :x "Month"
;                :label-formatter #'format-month-for-activity-charts
;                )
      ;(add-title "Kindista Usage over time")
      ;(add-feature :label)
;      (save-stream s))
;    (finish-output s)))

(defun monthly-statistic
  (year month statistic
  &key (return-list nil)
  &aux dir summary-file)
  (setf month (if (< (/ month 10) 1)
                (strcat "0" month)
                (strcat month)))
  (setf dir (strcat *metrics-path* year "/" month "/"))
  (setf summary-file (merge-pathnames dir "/monthly-summary"))

  (when (file-exists-p summary-file)
    (with-standard-io-syntax
        (with-open-file (s summary-file)
          (let* ((data (read s))
                 (stat (getf data statistic)))
            (cond
             ((and (listp stat) return-list)
              stat)
             ((listp stat)
              (length stat))
             (t stat)))))))

(defun transactions-completed-in-year (year)
  (average-statistic-in-year year :completed-transactions))

(defun average-statistic-in-year
  (year statistic
  &key (total-annual-count nil)
  &aux (return-value)
       (monthly-counts))
  (unless total-annual-count (setf return-value 0))
  (loop for month from 1 to 12
        do (asetf return-value
                  (if total-annual-count
                    (let ((monthly-statistic (monthly-statistic year
                                                                month
                                                                statistic
                                                                :return-list t)))

                      (setf monthly-counts
                            (append monthly-counts
                                    (list (length monthly-statistic))))
                      (remove-duplicates
                        (append it monthly-statistic)))
                    (+ it
                       (or (monthly-statistic year
                                              month
                                              statistic) 0)))))
  (when total-annual-count (asetf return-value (length it)))
  (values (list :monthly-counts monthly-counts)
          (list :total return-value)
          (list :average (coerce (if monthly-counts
                                   (/ (apply '+ monthly-counts)
                                      (length monthly-counts))
                                   (/ return-value 12))
                                 'float))))

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

;(defun send-progress-report-email (title)
;  (cl-smtp:send-email
;    +mail-server+
;    "Kindista <info@kindista.org>"
;    (if *productionp*
;      "Progress Reports <progress-reports@kindista.org>"
;      *error-message-email*)
;    title
;    (strcat
;      "Please see the attached file for a chart of various metrics we are collecting for Kindista usage. "
;      #\linefeed #\linefeed
;      "Please note: Due to a bug in the graphing library we are using, some of the dates may be repeated on the x-axis. "
;      "The data points should be correct and there is one data point per month for each metric . "
;      "Also, we didn't start collecting metrics for new offers/requests until July/2015.")
;    :attachments (merge-pathnames *metrics-path* "kindista-metrics-chart.png")))
