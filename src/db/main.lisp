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

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defvar *db* (make-hash-table :synchronized t :size 1000 :rehash-size 1.25))
(defvar *db-top* 0)
(defvar *db-top-lock* (make-mutex :name "db top"))
(defvar *db-log* nil)
(defvar *db-log-lock* (make-mutex :name "db log"))
(defvar *db-results* (make-hash-table :synchronized t :size 1000 :rehash-size 1.25))

;(defvar *auth-tokens* (make-hash-table :test 'equal :synchronized t :size 200 :rehash-size 1.25))
(defvar *tokens* (make-hash-table :test 'equal :synchronized t :size 200 :rehash-size 1.25))

(defstruct token
  userid created donate-info)

(defstruct donate-info
  amount type name address city state zip email phone token)

(defun donate-info-amount* (&optional (donate-info *donate-info*))
  (donate-info-amount donate-info))

(defun donate-info-type* (&optional (donate-info *donate-info*))
  (donate-info-type donate-info))

(defun donate-info-name* (&optional (donate-info *donate-info*))
  (donate-info-name donate-info))

(defun donate-info-address* (&optional (donate-info *donate-info*))
  (donate-info-address donate-info))

(defun donate-info-city* (&optional (donate-info *donate-info*))
  (donate-info-city donate-info))

(defun donate-info-state* (&optional (donate-info *donate-info*))
  (donate-info-state donate-info))

(defun donate-info-zip* (&optional (donate-info *donate-info*))
  (donate-info-zip donate-info))

(defun donate-info-email* (&optional (donate-info *donate-info*))
  (donate-info-email donate-info))

(defun donate-info-phone* (&optional (donate-info *donate-info*))
  (donate-info-phone donate-info))

(defun donate-info-token* (&optional (donate-info *donate-info*))
  (donate-info-token donate-info))

(defun (setf donate-info-amount*) (new-value &optional (donate-info *donate-info*))
  (setf (donate-info-amount donate-info) new-value))

(defun (setf donate-info-type*) (new-value &optional (donate-info *donate-info*))
  (setf (donate-info-type donate-info) new-value))

(defun (setf donate-info-name*) (new-value &optional (donate-info *donate-info*))
  (setf (donate-info-name donate-info) new-value))

(defun (setf donate-info-address*) (new-value &optional (donate-info *donate-info*))
  (setf (donate-info-address donate-info) new-value))

(defun (setf donate-info-city*) (new-value &optional (donate-info *donate-info*))
  (setf (donate-info-city donate-info) new-value))

(defun (setf donate-info-state*) (new-value &optional (donate-info *donate-info*))
  (setf (donate-info-state donate-info) new-value))

(defun (setf donate-info-zip*) (new-value &optional (donate-info *donate-info*))
  (setf (donate-info-zip donate-info) new-value))

(defun (setf donate-info-email*) (new-value &optional (donate-info *donate-info*))
  (setf (donate-info-email donate-info) new-value))

(defun (setf donate-info-phone*) (new-value &optional (donate-info *donate-info*))
  (setf (donate-info-phone donate-info) new-value))

(defun (setf donate-info-token*) (new-value &optional (donate-info *donate-info*))
  (setf (donate-info-token donate-info) new-value))

(defstruct result
  latitude longitude time tags people id type)

(defstruct alias
  alias result)

(defun result-id-intersection (list1 list2)
  (intersection list1 list2 :key #'result-id))

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
  (setf distance (min 10 distance))
  (let* ((latcode (ash geocode -10))
         (longcode (- geocode (ash latcode 10))))
    (iter (for lat from (- latcode distance) to (+ latcode distance))
          (nconcing
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
              (collect (+ (ash lat 10) long) at beginning))))))

(defun geo-index-query (index lat long distance)
  (setf distance (min 10 (ceiling (/ distance 12.4274))))
  (iter (for item in (delete-duplicates
                       (iter (for code in (geocode-neighbors (geocode lat long) distance))
                             (appending (gethash code index)))
                       :key #'result-id))
        (let ((item-distance (air-distance lat long (result-latitude item) (result-longitude item))))
          (when (<= item-distance distance)
            (collect item at beginning)))))

(defun geo-index-insert (index item)
  (let ((geocode (geocode (result-latitude item) (result-longitude item))))
    (with-locked-hash-table (*geo-index-index*)
      (push geocode (gethash (cons index item) *geo-index-index*))) 
    (with-locked-hash-table (index)
      (push item (gethash (geocode (result-latitude item) (result-longitude item)) index)))))

(defun geo-index-remove (index item)
  (with-locked-hash-table (*geo-index-index*)
    (with-locked-hash-table (index)
      (dolist (geocode (delete-duplicates (gethash (cons index item) *geo-index-index*))) 
        (asetf (gethash geocode index)
               (remove item it))))
    (remhash (cons index item) *geo-index-index*)))

(defun stem-index-query (index query)
  (iter (for stem in (stem-text query))
        (reducing (gethash stem index) by #'result-id-intersection)))
          
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


(defun map-metaphone-codes (function string)
  (iter (for code in (delete-duplicates
                       (iter (for word in (split " " string))
                           (nunioning
                             (iter (for i from 1 to (length word))
                                   (nunioning
                                     (flatten
                                       (name-to-metaphone-codes (subseq word 0 i)))
                                     test #'string=))
                             test #'string=))
                     :test #'string=))
        (unless (string= code "")
          (funcall function code))))

(defun metaphone-index-insert (new-names result)
  (with-locked-hash-table (*person-alias-index*)
    (with-locked-hash-table (*metaphone-index*)
      (dolist (alias (gethash (result-id result) *person-alias-index*))
        (labels ((remove-alias (code)
                   (asetf (gethash code *metaphone-index*)
                          (remove alias it))))
          (map-metaphone-codes #'remove-alias (alias-alias alias))))

      (remhash (result-id result) *person-alias-index*)

      (dolist (name new-names)
        (let ((alias (make-alias :alias name :result result)))
          (push alias (gethash (result-id result) *person-alias-index*))
          (labels ((add-alias (code)
                     (push alias (gethash code *metaphone-index*))))
            (map-metaphone-codes #'add-alias name)))))))

(defun metaphone-index-query (name)
  (when (< 0 (length name))
    (labels ((lev-distance (alias)
               (levenshtein:distance name (alias-alias alias))))
      (remove-duplicates
        (sort
          (remove-duplicates
            (iter (for codes in (cartesian-product
                                  (name-to-metaphone-codes name)))
                  (appending (iter (for code in codes)
                                  (unless (string= code "")
                                    (reducing (gethash code *metaphone-index*)
                                              by #'intersection))))))
          #'< :key #'lev-distance)
        :key #'alias-result))))

; }}}

(defun load-tokens ()
  (with-open-file (in (s+ +db-path+ "tokens") :if-does-not-exist nil)
    (when in
      (with-standard-io-syntax
        (with-locked-hash-table (*tokens*)
          ;(clrhash *tokens*)
          (loop
            (handler-case
              (let ((item (read in)))
                (setf (gethash (car item) *tokens*) (cdr item)))
              (end-of-file (e) (declare (ignore e)) (return)))))))))


(defun save-tokens ()
  (with-open-file (out (s+ +db-path+ "tokens-tmp")
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (with-locked-hash-table (*tokens*)
        (iter (for (key value) in-hashtable *tokens*)
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
                    (setf (gethash (car item) *db*) (cdr item)))
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
                        (progn
                          (asetf *db-top* (max (second item) it))
                          (setf (gethash (second item) *db*) (cddr item))))))
                  (end-of-file (e) (declare (ignore e)) (return)))))))))
    (setf *db-log* (open (s+ +db-path+ "db-log") :if-exists :append :if-does-not-exist :create :direction :output)))
  (clear-indexes)
  (maphash #'index-item *db*))


(defun db (id &optional key)
  (if key
    (getf (gethash id *db*) key) 
    (gethash id *db*)))

(defun created (id)
  (db id :created))

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

(defmacro amodify-db (id &rest items)
"anaphoric macro used to modify the database. 
 'items' is a list of key/value pairs. 
 the implicitly bound symbol 'it' can be used within the value-form 
 to refer to the current value of key which is to be modified, 
 i.e. it == (getf (db id) :key). 
 'it' is lexically bound within the context of the key/value pair 
 in which it is invoked."

  (let ((data (gensym)))
    `(with-locked-hash-table (*db*)
       (let ((,data (db ,id)))
         ,@(do
             ((statements ())
              (item items (cddr item)))
             ((not (and (car item) (symbolp (car item)))) statements)
             
             (push `(let ((it (getf ,data ,(car item)))) 
                      (setf (getf ,data ,(car item)) ,(cadr item))) statements))
         (update-db ,id ,data)))))

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
    (:comment (index-comment id data))
    (:invitation (index-invitation id data))
    (:gratitude (index-gratitude id data))
    (:gift (index-gift id data))
    ((or :offer :request) (index-inventory-item id data))
    (:person (index-person id data))
    (:contact-n (index-contact-notification id data))
    (:conversation (index-conversation id data))))

(defun contacts-alphabetically (&optional (user *user*))
  (sort (iter (for contact in (getf user :following))
              (collect (list contact (getf (db contact) :name))))
        #'string-lessp :key #'cadr))

(defun user-distance (&optional (user *user*))
  (if user
    (or (getf user :distance) 25)
    100))

(defun user-rdist (&optional (user *user*))
  (or (getf user :rdist) 25))
