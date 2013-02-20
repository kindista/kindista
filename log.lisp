(defpackage :kindista.log
  (:use :cl)
  (:import-from :sb-thread :make-mutex :with-mutex)
  (:import-from :kindista :+db-path+)
  (:import-from :kindista.events :add-event-handler :*event*))

(in-package :kindista.log)

(defvar *lock* (make-mutex :name "metrics log"))
(defvar *log*  (open (concatenate 'string +db-path+ "metrics")
                     :if-exists :append
                     :if-does-not-exist :create
                     :direction :output)) 

(defun log-event ()
  (with-mutex (*lock*)
    (with-standard-io-syntax
      ; probably need an error handler in here in case the log gets moved
      (prin1 *event* *log*)
      (fresh-line *log*))
    (fsync *log*)))

(add-event-handler :all #'log-event)
