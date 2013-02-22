(in-package :kindista)

(defvar *metrics-lock* (make-mutex :name "metrics log"))
(defvar *metrics-log*  (open (s+ +db-path+ "metrics")
                             :if-exists :append
                             :if-does-not-exist :create
                             :direction :output)) 

(defstruct metric
           ;;;;;;
           type
           timestamp
           text
           user
           ip)


(defun log-metric (metric)
  (with-mutex (*metrics-lock*)
    (with-standard-io-syntax
      ; probably need an error handler in here in case the log gets moved
      (prin1 metric *metrics-log*)
      (fresh-line *metrics-log*))
    (fsync *metrics-log*)))
