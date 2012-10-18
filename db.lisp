(in-package :kindista)

(defvar *db* (make-hash-table :synchronized t :size 1000 :rehash-size 1.25))
(defvar *db-top* 0)
(defvar *db-top-lock* (make-mutex :name "db top"))
(defvar *db-log* nil)
(defvar *db-log-lock* (make-mutex :name "db log"))

(defvar *geo-index* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))
(defvar *stem-index*)
(defvar *metaphone-index*)
(defvar *email-index* (make-hash-table :test 'equalp :synchronized t :size 500 :rehash-size 1.25))
(defvar *username-index* (make-hash-table :test 'equalp :synchronized t :size 500 :rehash-size 1.25))

(defvar *auth-tokens* (make-hash-table :test 'equal :synchronized t :size 200 :rehash-size 1.25))

;; locks

(defvar *make-user-lock* (make-mutex :name "make user"))
(defvar *make-token-lock* (make-mutex :name "make auth token"))


(defun fsync (stream)
  (finish-output stream)
  (sb-posix:fsync (sb-posix:file-descriptor stream)))

;;; {{{ geo stuff
(define-constant earth-radius 6372.8) ; km

(defun air-distance (lat1 long1 lat2 long2)
  "The great circle distance between two cities."
  (* 0.621371
     (apply #'dist-rad (mapcar #'deg->rad (list lat1 long1 lat2 long2)))))

(defun dist-rad (lat1 long1 lat2 long2)
  (let* ((hlat (haversine (- lat2 lat1)))
         (hlng (haversine (- long2 long1)))
         (root (sqrt (+ hlat (* (cos lat1) (cos lat2) hlng)))))
    (* 2 earth-radius (asin root))))

(defun haversine (x)
  (expt (sin (/ x 2)) 2))

(defun deg->rad (deg)
  "Convert degrees and minutes to radians."
  (* deg (/ pi 180)))

(defun geocode (lat long)
  (+ (ash (floor (* (/ (+ 90 lat) 180) (ash 1 10))) 10)
  (floor (* (/ (+ 180 long) 360) (ash 1 10)))))

(defun geocode-neighbors (geocode distance)
  (let* ((latcode (ash geocode -10))
         (longcode (- geocode (ash latcode 10))))
    (flatten
      (iter (for lat from (- latcode distance) to (+ latcode distance))
            (collect
              (iter
                (for long from (- longcode distance) to (+ longcode distance))
                (cond
                  ((> lat 1023)
                   (setf lat(- lat 1024)))
                  ((< lat 0)
                   (setf lat (+ lat 1024))))
                (cond
                  ((> long 1023)
                   (setf long (- long 1024)))
                  ((< long 0)
                   (setf long (+ long 1024))))
                (collect (+ (ash lat 10) long))))))))

(defun query-geo-index (index geocode distance)
  (iter (for code in (geocode-neighbors geocode distance))
        (appending (gethash code index))))

; }}}

(defun load-tokens ()
  (with-open-file (in (s+ +db-path+ "tokens") :if-does-not-exist nil)
    (when in
      (with-standard-io-syntax
        (with-locked-hash-table (*auth-tokens*)
          ;(clrhash *auth-tokens*)
          (loop
            (handler-case
              (let ((item (read in)))
                (setf (gethash (car item) *auth-tokens*) (cdr item)))
              (end-of-file (e) (declare (ignore e)) (return)))))))))


(defun save-tokens ()
  (with-open-file (out (s+ +db-path+ "tokens-tmp")
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (prin1 *auth-tokens* out))
    (fsync out))
  (rename-file (s+ +db-path+ "tokens-tmp") "tokens"))

(defun save-db ()
  (with-mutex (*db-log-lock*)
    (with-open-file (out (s+ +db-path+ "db-tmp")
                         :direction :output
                         :if-exists :supersede)
      (with-standard-io-syntax
        (prin1 (get-universal-time) out)
        (fresh-line out)
        (with-locked-hash-table (*db*)
          (maphash #'(lambda (key value)
                       (prin1 (cons key value) out)
                       (fresh-line out)
                       )
                   *db*)))
      (fsync out))
    (rename-file (s+ +db-path+ "db-tmp") "db")
    (when *db-log*
      (close *db-log*)
      (setf *db-log* (open (s+ +db-path+ "db-log")
                           :if-exists :supersede
                           :if-does-not-exist :create
                           :direction :output))
      (fsync *db-log*))))

(defun load-db ()
  (with-mutex (*db-log-lock*)
    (let (latest)
      (when *db-log*
        (close *db-log*))
      (with-open-file (in (s+ +db-path+ "db") :if-does-not-exist nil)
        (when in
          (with-standard-io-syntax
            (setf latest (read in))
            (with-locked-hash-table (*db*)
              (clrhash *db*)
              (loop
                (handler-case
                  (let ((item (read in)))
                    (setf *db-top* (max (car item) *db-top*))
                    (setf (gethash (car item) *db*) (cdr item))
                    (index-item (car item) (cdr item)))
                  (end-of-file (e) (declare (ignore e)) (return))))))))
      (with-open-file (in (s+ +db-path+ "db-log") :if-does-not-exist nil)
        (when in
          (with-standard-io-syntax
            (with-locked-hash-table (*db*)
              (loop
                (handler-case
                  (let ((item (read in)))
                    (when (>= (first item) latest)
                      (if (eq (third item) nil)
                        (remhash (second item) *db*)
                        (setf (gethash (second item) *db*) (cddr item)))))
                  (end-of-file (e) (declare (ignore e)) (return)))))))))
    (setf *db-log* (open (s+ +db-path+ "db-log") :if-exists :append :if-does-not-exist :create :direction :output))))


(defun db (id)
  (gethash id *db*))

(defun update-db (id data)
  (with-mutex (*db-log-lock*)
    (with-standard-io-syntax
      (prin1 (append (list (get-universal-time) id) data) *db-log*)
      (fresh-line *db-log*))
    (fsync *db-log*))
  (with-locked-hash-table (*db*)
    (setf (gethash id *db*) data)))

(defun insert-db (data)
  (let ((id (with-mutex (*db-top-lock*) (incf *db-top*))))
    (update-db id data)
    (index-item id data)
    id))

(defun remove-from-db (id)
  (with-mutex (*db-log-lock*)
    (with-standard-io-syntax
      (prin1 (list (get-universal-time) id nil) *db-log*)
      (fresh-line *db-log*))
    (fsync *db-log*))
  (with-locked-hash-table (*db*)
    (remhash id *db*)))

(defun index-item (id data)
  (case (getf data :type)
    (:person (index-person id data))))
