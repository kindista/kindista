(in-package :kindista)

(defun create-offer (&key by text)
  (insert-db (list :type :offer
                             :by by
                             :text text
                             :created (get-universal-time))))

(defun index-offer (id data)
  (when (and (getf data :lat) (getf data :long) (getf data :created))
    (geo-index-append (getf data :lat) (getf data :long) id (getf data :created))))
