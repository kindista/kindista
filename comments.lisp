(in-package :kindista)

(defun create-comment (&key on by text)
  (insert-db (list :type :comment
                         :on on
                         :by by
                         :text text
                         :created (get-universal-time))))

(defun index-comment (id data)
  (setf (gethash (getf data :on) *comment-index*)
        (cons id (gethash (getf data :on) *comment-index*))))

(defun comments (id)
  (gethash id *comment-index*))
