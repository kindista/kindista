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

(define-constant +week-in-seconds+ 604800)
(define-constant +day-in-seconds+ 86400)
(define-constant +day-names+
             '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday") :test #'equal)
(define-constant +month-names+
             '("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December") :test #'equal)
(define-constant +positions-of-day-in-month+
                 '("first" "second" "third" "fourth" "last") :test #'equal)

(defun parse-datetime (date &optional time)
  (multiple-value-bind (seconds minutes hours date month year)
    (let ((chronicity-date (chronicity:parse
                             (s+ date (awhen time (s+ " at " it)))
                             :endian-preference :middle)))
      (if chronicity-date
        (values (chronicity:sec-of chronicity-date)
                (chronicity:minute-of chronicity-date)
                (chronicity:hour-of chronicity-date)
                (chronicity:day-of chronicity-date)
                (chronicity:month-of chronicity-date)
                (chronicity:year-of chronicity-date))
        (error 'local-time::invalid-time-specification)))
    (encode-universal-time seconds minutes hours date month year)))

(defun humanize-universal-time (then)
  (let ((delta (- (get-universal-time) then)))
    (cond
      ((< delta 60)
       "just now")
      ((< delta 120)
       "a minute ago")
      ((< delta 3300)
       (format nil "~a minutes ago" (round (/ delta 60))))
      ((< delta 5400)
       "about an hour ago")
      ((< delta 129600)
       (format nil "~a hours ago" (round (/ delta 3600))))
      ((< delta 4320000)
       (format nil "~a days ago" (round (/ delta 86400))))
      (t
       (format nil "~a months ago" (round (/ delta 2492000)))))))

(defun humanize-future-time (time)
  (let* ((now (get-universal-time))
         (seconds (- time now)))
    (cond
      ((< seconds 60)
       "in less than a minute")
      ((< seconds 120)
       "in about a minute")
      ((< seconds 3600)
       (strcat "in " (floor (/ seconds 60)) " minutes"))
      ((< seconds 7200)
       "in about an hour")
      ((< seconds 86400)
       (strcat "in " (floor (/ seconds 3600)) " hours"))
      ((< seconds 172800)
       "tomorrow")
      ((< seconds 2678400)
       (strcat "in " (floor (/ seconds 86400)) " days"))
      ((< seconds 5270400)
       "next month")
      ((< seconds 31536000)
       (strcat "in " (floor (/ seconds 2628000)) " months"))
      ((< seconds 63072000)
       "next year")
      (t
       (strcat "in " (floor (/ seconds 31536000)) " years")))))

(defun humanize-exact-time (universal-time &key detailed year-first weekday)
 (multiple-value-bind (seconds minutes hours date month year day-of-week)
     (decode-universal-time universal-time)
   (declare (ignore seconds))
   (let* ((month-name (nth (- month 1) +month-names+))
          (day-name (nth day-of-week +day-names+))
          (m (if (> hours 11) "PM" "AM"))
          (hour (cond 
                  ((= hours 0) 12)
                  ((> hours 12) (- hours 12))
                  (t hours)))
          (minute (if (< minutes 10)
                    (strcat "0" minutes)
                    minutes))
          (time (strcat hour ":" minute " " m))
          (formatted-date (strcat month "/" date "/" year))
          (date-name (strcat day-name ", " month-name " " date ", " year )))

     (cond
      (weekday (string-downcase day-name))
      (detailed (s+ date-name " at " time))
      (year-first (strcat year "-" month "-" date))
      (t (values time date-name formatted-date))))))

(defun day-of-month (datetime &key formatted-date)
  (timestamp-day (universal-to-timestamp (if formatted-date
                                           (parse-datetime datetime)
                                           datetime))))

(defun days-in-month (datetime)
  (let ((timestamp (universal-to-timestamp
                     (typecase datetime (integer datetime)
                                        (string (parse-datetime datetime))))))
   (local-time:days-in-month (timestamp-month timestamp)
                             (timestamp-year timestamp))))

(defun position-of-day-in-month (datetime &key formatted-date)
  (let* ((timestamp (universal-to-timestamp
                      (if formatted-date
                        (parse-datetime datetime)
                        datetime)))
         (day-of-month (timestamp-day timestamp))
         (days-in-month (local-time:days-in-month
                          (timestamp-month timestamp)
                          (timestamp-year timestamp))))
    (cond
      ((< day-of-month 8) "first")
      ((< day-of-month 15) "second")
      ((< day-of-month 22) "third")
      ((<= (+ day-of-month 7) days-in-month) "fourth")
      (t "last"))))

(defun inline-timestamp (time &key type url)
  (let ((inner (html
                 (when type
                   (htm (str type) " "))
                 (str (humanize-universal-time time)))))
    (html
      (:span :class "timestamp" :data-time time :data-type type
        (if url
          (htm (:a :href url (str inner)))
          (str inner))))))
