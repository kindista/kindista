(in-package :kindista)

(defvar *log-lock* (make-mutex :name "log stream"))
(defvar *log-stream*  (open (concatenate 'string +db-path+ "log")
                            :if-exists :append
                            :if-does-not-exist :create
                            :direction :output)) 

(defun log-notice ()
  (with-mutex (*log-lock*)
    (with-standard-io-syntax
      ; probably need an error handler in here in case the log gets moved
      (prin1 *notice* *log-stream*)
      (fresh-line *log-stream*))
    (fsync *log-stream*)))

