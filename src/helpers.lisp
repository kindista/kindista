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

(defparameter +number-scanner+ (create-scanner "^\\d+$"))
(defparameter +full-name-scanner+ (create-scanner "^([a-zA-Z]+\\.? )[a-zA-Z]+"))
(defparameter +email-scanner+ (create-scanner
                                 "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,6}$"))
(defparameter *english-list*
  "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}")
(defparameter +zip-scanner+ (create-scanner "^(\\d{5})((-)(\\d{4}))?$"))
(defparameter +phone-scanner+ (create-scanner
                                "(?:(?:\\+?1[\\(\\s]*(?:[.-]\\s*)?)?(?:(\\s*([2-9]1[02-9]|[2-9][02-8]1|[2-9][02-8][02-9])[\\)\\s]*)|([2-9]1[02-9]|[2-9][02-8]1|[2-9][02-8][02-9]))\\s*(?:[.-]\\s*)?)([2-9]1[02-9]|[2-9][02-9]1|[2-9][02-9]{2})\\s*(?:[.-]\\s*)?([0-9]{4})"))

(defparameter +date-scanner+ (create-scanner "^(0[1-9]|1[0-2]|[1-9])([/-])(0[1-9]|3[01]|[12][0-9]|[0-9])([/-])(19[0-9][0-9]|[2-9][0-9][0-9][0-9])$"))

(defparameter +time-scanner+ (create-scanner "^(0[1-9]|1[0-2]|[1-9])(:[0-5][0-9])? ?(a|A)|(p|P)\\.?(m|M)\\.? ?$"))

(defun strcat (&rest items)
  (format nil "~{~A~}" items))

(defmacro s+ (&rest strings)
  `(concatenate 'string ,@strings))

(defun validate-name (string)
  (scan +full-name-scanner+ string))

(defun validate-email (string)
  (scan +email-scanner+ string))

(defun fsync (stream)
  (finish-output stream)
  (sb-posix:fsync (sb-posix:file-descriptor stream)))

(defmacro with-file-lock ((path &key interval) &body body)
  "Get an exclusive lock on a file. If lock cannot be obtained, keep
   trying after waiting a while"
  (let ((lock-path (gensym))
        (lock-file (gensym)))
    `(let ((,lock-path (format nil "~a.lock" (namestring ,path))))
       (unwind-protect
         (progn
           (loop
             :for ,lock-file = (open ,lock-path :direction :output
                                     :if-exists nil
                                     :if-does-not-exist :create)
             :until ,lock-file
             :do (sleep ,(or interval 0.1))
             :finally (close ,lock-file))
           ,@body)
         (ignore-errors
           (delete-file ,lock-path))))))

(defmacro html (&body body)
  (let ((sym (gensym)))
    `(with-html-output-to-string (,sym)
       ,@body)))

(defmacro asetf (place value)
  `(anaphora::symbolic setf ,place ,value))

(defun sublist (list &optional start count)
  (when start
    (setf list (nthcdr start list)))
  (let ((length (length list)))
    (cond
      ((or (not count)
           (>= count length))
       (values list nil))
      (t
       (values (subseq list 0 count) t)))))

(defun intersection-fourth (list1 list2)
  (intersection list1 list2 :key #'fourth))

(defun string-intersection (list1 list2)
  (intersection list1 list2 :test #'string=))

(defun emails-from-string (string)
  (iter (for email in (split " " (ppcre:regex-replace-all ",|>|<" (string-downcase string) " ")))
        (when (ppcre:scan +email-scanner+ email)
          (collect email))))

(defun activity-rank (item)
  (let ((contacts (getf *user* :following))
        (age (- (get-universal-time) (or (result-time item) 0)))
        (distance (air-distance *latitude* *longitude*
                                (result-latitude item) (result-longitude item))))
    (round (- age
             (/ 120000 (log (+ (if (intersection contacts (result-people item)) 1 distance) 4)))
             (* (length (loves (result-id item))) 50000)))))

(defun stale-eventp (item)
  (let ((staleness (- (or (result-time item) 0)
                      (- (get-universal-time) +day-in-seconds+))))
    (when (< staleness 0) t)))

(defun event-rank (item)
  (let ((contacts (getf *user* :following))
        (currentness (abs (- (or (result-time item) 0) (get-universal-time))))
        (distance (air-distance *latitude* *longitude*
                                (result-latitude item) (result-longitude item))))
    (round (- currentness
              (/ 120000
                 (log (+ (if (intersection contacts (result-people item))
                             1
                             distance)
                         4)))
              (* (length (loves (result-id item))) 50000)))))

(defun inventory-rank (item)
  (let ((age (- (get-universal-time) (or (result-time item) 0)))
        (loves (max 1 (length (loves (result-id item))))))
    (* (/ 50 (log (+ (/ age 86400) 6)))
       (expt loves 0.3))))

(defun refresh-item-time-in-indexes (id &key (time (get-universal-time)))
  (let* ((result (gethash id *db-results*))
         (type (result-type result))
         (item (db id))
         (by (case type
               ((or :offer :request)
                (getf item :by))
               (:gratitude (getf item :author)))))

    (when (and (eql *userid* by)
               (or (eq type :gratitude)
                   (eq type :offer)
                   (eq type :request)))

      (setf (result-time result) time)
      (with-locked-hash-table (*activity-person-index*)
        (asetf (gethash by *activity-person-index*)
               (sort it #'> :key #'result-time)))

      (unless (< (result-time result) (- (get-universal-time) 15552000))
        (unless (< (result-time result) (- (get-universal-time) 2592000))
          (with-mutex (*recent-activity-mutex*)
            (setf *recent-activity-index*
                  (sort (pushnew result *recent-activity-index*)
                        #'> :key #'result-time))))
        (geo-index-insert *activity-geo-index* result)))))

(defun url-compose (base &rest params)
  (do ((param-strings ()))
      ((not params) (format nil "~a~a~{~a~^&~}" base (if param-strings "?" "") param-strings))
      (when (cadr params)
        (push (if (consp (cadr params))
                (format nil "~a=~{~a~^+~}" (car params) (cadr params))
                (format nil "~a=~a" (car params) (cadr params)))
              param-strings))
      (setf params (cddr params))))

(defun ellipsis (text &optional (length 160) &key see-more)
  (let ((newtext (subseq text 0 (min (length text) length))))
    (if (> (length text) length)
      (html
        (str newtext)
        "..." 
        (when see-more
          (htm (:a :href see-more " see more"))))
      newtext)))

(defun html-text (string)
  (if string
    (regex-replace-all "\\n" (escape-for-html string) "<br>")
    ""))

(defun distance-string (miles)
  (let ((distance (/ (round miles 0.5) 2)))
    (cond
      ((<= distance 1/2)
       "1/2 mile")
      ((eql distance 1)
       "1 mile")
      ((typep distance 'ratio)
       (format nil "~1$ miles" (coerce distance 'float)))    
      (t
       (format nil "~d miles" distance)))))

(defvar *state-options*
  (html
    (dolist (state '("AL" "AK" "AZ" "AR" "CA" "CO" "CT" "DE" "DC" "FL" "GA" "HI" "ID" "IL" "IN" "IA" "KS" "KY" "LA" "ME" "MD" "MA" "MI" "MN" "MS" "MO" "MT" "NE" "NV" "NH" "NJ" "NM" "NY" "NC" "ND" "OH" "OK" "OR" "PA" "RI" "SC" "SD" "TN" "TX" "UT" "VT" "VA" "WA" "WV" "WI" "WY"))
      (htm
        (:option :value state (str state))))))

(defun state-options (&optional selected)
  (html
    (dolist (state '("AL" "AK" "AZ" "AR" "CA" "CO" "CT" "DE" "DC" "FL" "GA" "HI" "ID" "IL" "IN" "IA" "KS" "KY" "LA" "ME" "MD" "MA" "MI" "MN" "MS" "MO" "MT" "NE" "NV" "NH" "NJ" "NM" "NY" "NC" "ND" "OH" "OK" "OR" "PA" "RI" "SC" "SD" "TN" "TX" "UT" "VT" "VA" "WA" "WV" "WI" "WY"))
      (htm
        (:option :value state :selected (if (equalp selected state) "selected" nil) (str state))))))

(defun empty-string-p (string)
  (or (not string) (string= string "")))

(defun alpha-people-links (userid-list)
  (mapcar
    #'cdr
    (sort
      (iter (for id in (copy-list userid-list))
            (let* ((name (db id :name))
                   (link (html (:a :href (strcat "/people/" (username-or-id id))
                                         (str name)))))
              (collect (cons name link))))
     #'string-lessp :key #'car)))

(defun person-link (id)
  (html
    (:a :href (s+ "/people/" (username-or-id id)) (str (getf (db id) :name)))))

(defun name-list (ids &key (func #'person-link) (minimum-links 3))
  (let ((links (mapcar func (subseq ids 0 (min minimum-links (length ids))))))
    (format nil *english-list* (cond
                                ((> (length ids) (+ minimum-links 1))
                                 (append links (list (strcat "and " (- (length ids) minimum-links) " others"))))
                                ((eql (length ids) (+ minimum-links 1))
                                 (append links (list (funcall func (nth 3 ids)))))
                                (t links)))))

(defun name-list-all (ids)
  (format nil *english-list* (mapcar #'person-link ids)))

(defun pending-flash (action)
  (flash (s+ "Your account hasn't been fully activated yet. "
             "If you haven't already done so, please post some offers. "
             "After we review your initial activity you will be able to "
             action ".")
         :error t))

(defun pending-disclaimer (&optional type)
  (when (getf *user* :pending)
    (html
      (:p :class "err"
        (:em (:strong "Please note: ")
          "This "
          (str (aif type it "item"))
          " will be displayed on Kindista after we have a chance to review "
          "your account and confirm that you're not a spammer. "
          (:strong "You won't be able to send messages to other Kindista members "
          "until you post some offers and we have a chance to review your "
          "initial activity.")))
      (:br))))
