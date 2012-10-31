(in-package :kindista)

(defvar *timeline-index* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))

(defun timeline-insert (userid time objectid)
  "insert objectid at time into userid's timeline and sort"

  (setf (gethash userid *timeline-index*)
        (sort (cons (cons objectid time) (gethash userid *timeline-index*))
              #'> :key #'cdr)))
