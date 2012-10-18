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
  ; TODO create activity for creation time
  )

(defun username-or-id ()
  (or (getf (db *user*) :username)
      (write-to-string *user*)))
