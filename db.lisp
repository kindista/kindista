(in-package :kindista)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defvar *db* (make-hash-table :synchronized t :size 1000 :rehash-size 1.25))
(defvar *db-top* 0)
(defvar *db-top-lock* (make-mutex :name "db top"))
(defvar *db-log* nil)
(defvar *db-log-lock* (make-mutex :name "db log"))

(defvar *geo-index* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))
(defvar *love-index* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))
(defvar *comment-index* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))
(defvar *gratitude-index* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))
(defvar *stem-index*)
(defvar *metaphone-index* (make-hash-table :test 'equalp :synchronized t :size 500 :rehash-size 1.25))
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

(defun person-distance (one &optional (two *user*))
  (air-distance (getf one :lat) (getf one :long) (getf two :lat) (getf two :long)))

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
                   (asetf lat(- it 1024)))
                  ((< lat 0)
                   (asetf lat (+ it 1024))))
                (cond
                  ((> long 1023)
                   (asetf long (- it 1024)))
                  ((< long 0)
                   (asetf long (+ it 1024))))
                (collect (+ (ash lat 10) long))))))))

(defun geo-index-query (lat long distance &key (index *geo-index*) type)
  (let ((results (delete-duplicates
                   (iter (for code in (geocode-neighbors (geocode lat long) distance))
                         (appending (gethash code index)))
                   :key #'fourth)))
    (if type
      (iter (for item in results)
            (when (eql type (getf (db (fourth item)) :type))
              (collect item)))
      results)))

(defun geo-index-insert (lat long id created &key (index *geo-index*))
  (let ((geocode (geocode lat long)))
    (with-locked-hash-table (index)
      (asetf (gethash geocode index)
             (cons (list created lat long id) it)))))

; }}}

; {{{ metaphone

(defun simple-cartesian-product (set list)
  (loop for elm in set
        nconc (loop for set in list
                    collect (cons elm set))))

(defun cartesian-product (list-of-sets)
  (reduce #'simple-cartesian-product list-of-sets
          :from-end t
          :initial-value '(())))

(defun name-to-metaphone-codes (name)
  (iter (for word in (split " " name))
        (collect (multiple-value-list (double-metaphone word)))))

(defun metaphone-index-insert (index name id)
  (with-locked-hash-table (index)
    (iter (for code in (delete-duplicates
                         (iter (for word in (split " " name))
                               (nunioning
                                 (iter (for i from 1 to (length word))
                                       (nunioning
                                         (flatten
                                           (name-to-metaphone-codes (subseq word 0 i)))
                                         test #'string=))
                                 test #'string=))
                         :test #'string=))

          (unless (string= code "")
            (let ((list (gethash code index)))
              (unless (member id list)
                (setf (gethash code index) (cons id list))))))))

(defun metaphone-index-query (index name)
  (when (< 0 (length name))
    (delete-duplicates
      (iter (for codes in (cartesian-product (name-to-metaphone-codes name)))
            (reducing (iter (for code in codes)
                            (unless (string= code "")
                              (nconcing (gethash code index))))
                      by #'intersection)))))

; }}}

(defun load-tokens ()
  (declare (optimize (speed 0) (safety 3) (debug 3)))
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
      (with-locked-hash-table (*auth-tokens*)
        (iter (for (key value) in-hashtable *auth-tokens*)
              (prin1 (cons key value) out)
              (fresh-line out))))
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
          (iter (for (key value) in-hashtable *db*)
                (prin1 (cons key value) out)
                (fresh-line out))))
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
                    (asetf *db-top* (max (car item) it))
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

(defun modify-db (id &rest items)
  (with-locked-hash-table (*db*)
    (let ((data (db id)))
      (do
        ((item items (cddr item)))
        ((not (and (car item) (symbolp (car item)))))
        (setf (getf data (car item)) (cadr item)))
      (update-db id data))))

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

(defun clear-indexes ()
  (dolist (index (list
                   *geo-index*
                   *love-index*
                   *comment-index*
                   *timeline-index*
                   *metaphone-index*
                   *username-index*
                   *email-index*))
    (clrhash index)))

(defun index-item (id data)
  (case (getf data :type)
    (:comment (index-comment id data))
    (:gratitude (index-gratitude id data))
    (:offer (index-offer id data))
    (:person (index-person id data))))

(defun friends-alphabetically (&optional (user *user*))
  (sort (iter (for friend in (getf user :friends))
              (collect (list friend (getf (db friend) :name))))
        #'string-lessp :key #'cadr))

