(in-package :kindista)

(defun strcat (&rest items)
  (format nil "~{~A~}" items))

(defmacro s+ (&rest strings)
  `(concatenate 'string ,@strings))

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

(defun sublist (list &optional start length)
  (when start
    (dotimes (i start)
      (setf list (cdr list))))
  (if length
    (when list
      (iter (for i from 1 to length)
            (collect (car list))
            (if (cdr list)
              (setf list (cdr list))
              (finish))))
    (copy-list list)))

(defun intersection-fourth (list1 list2)
  (intersection list1 list2 :key #'fourth))

(defun string-intersection (list1 list2)
  (intersection list1 list2 :test #'string=))

(defun activity-rank (item)
  ; is it from a friend?
  ; how many people like it
  ; how recent
  ; how close is it?
  
  (let ((friends (getf *user* :following))
        (age (- (get-universal-time) (or (result-created item) 0)))
        (distance (air-distance *latitude* *longitude*
                                (result-latitude item) (result-longitude item))))
    (round (- age
             (/ 120000 (log (+ distance 4)))
             (if (intersection friends (result-people item)) 86400 0)
             (* (length (loves (result-id item))) 100000)))))

(defun inventory-rank (item)
  (let ((age (- (get-universal-time) (or (result-created item) 0)))
        (loves (max 1 (length (loves (result-id item))))))
    (* (/ 50 (log (+ (/ age 86400) 6)))
       (expt loves 0.3))))

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

(defun url-compose (base &rest params)
  (do ((param-strings ()))
      ((not params) (format nil "~a~a~{~a~^&~}" base (if param-strings "?" "") param-strings))
      (when (cadr params)
        (push (if (consp (cadr params))
                (format nil "~a=~{~a~^+~}" (car params) (cadr params))
                (format nil "~a=~a" (car params) (cadr params)))
              param-strings))
      (setf params (cddr params))))

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
