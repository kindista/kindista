(in-package :kindista)

(defun create-person (&key name email password)
  (insert-db `(:type :person
               :name ,name
               :email ,email
               :pass ,(new-password password)
               :created ,(get-universal-time))))

(defun index-person (id data)
  (setf (gethash (getf data :email) *email-index*) id)
  (setf (gethash (getf data :username) *username-index*) id)
  (metaphone-index-insert *metaphone-index* (getf data :name) id)
  (when (getf data :loves)
    (with-locked-hash-table (*love-index*)
      (dolist (item (getf data :loves))
        (setf (gethash item *love-index*)
              (cons id (gethash item *love-index*))))))
  (when (and (getf data :lat) (getf data :long) (getf data :created))
    (geo-index-insert (getf data :lat) (getf data :long) id (getf data :created)))
  ; TODO create activity for creation time
  )

(defun username-or-id (&optional (id *userid*))
  (or (getf (db id) :username)
      (write-to-string id)))

(defun loves (id)
  (gethash id *love-index*))
