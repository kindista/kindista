(in-package :kindista)

(defvar *timeline-index* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))

(defun timeline-insert (userid result)
  "insert objectid at time into userid's timeline and sort"

  (with-locked-hash-table (*timeline-index*)
    (asetf (gethash userid *timeline-index*)
           (sort (cons result it)
                 #'> :key #'result-created))))

(defun sort-timeline (userid)
  (with-locked-hash-table (*timeline-index*)
    (asetf (gethash userid *timeline-index*)
           (sort it #'> :key #'result-created))))

(defun timeline-remove (userid result)
  "insert objectid at time into userid's timeline and sort"

  (with-locked-hash-table (*timeline-index*)
    (asetf (gethash userid *timeline-index*)
           (remove result it))))
