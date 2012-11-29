(in-package :kindista)

(defun love (id &key (userid *userid*))
  (with-locked-hash-table (*db*)
    (let* ((user (db userid))
           (loves (getf user :loves)))
      (unless (member id loves)
        (setf (getf user :loves) (cons id loves))
        (update-db userid user))))

  (with-locked-hash-table (*love-index*)
    (let* ((loves (gethash id *love-index*)))
      (unless (member userid loves)
        (setf (gethash id *love-index*) (cons userid loves))))))

(defun unlove (id &key (userid *userid*))
  (with-locked-hash-table (*db*)
    (let* ((user (db userid))
           (loves (getf user :loves)))
      (when (member id loves)
        (setf (getf user :loves) (remove id loves))
        (update-db userid user))))

  (with-locked-hash-table (*love-index*)
    (let* ((loves (gethash id *love-index*)))
      (when (member userid loves)
        (setf (gethash id *love-index*) (remove userid loves))))))

(defun loves (id)
  (gethash id *love-index*))

