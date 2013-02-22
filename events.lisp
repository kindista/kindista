(in-package :kindista)

(defvar *event*) ; used as a dynamic variable for event handlers
(defvar *event-thread* nil)
(defvar *event-mailbox* (make-mailbox :name "event reader"))
(defvar *event-handlers* (make-hash-table :synchronized t :size 100 :test 'eq))

(defstruct event
  (type :info :type symbol)
  (time (get-universal-time) :type integer)
  (text "" :type string)
  (userid -1 :type integer)
  (ip "" :type string))

(defun event (type text &key (time (get-universal-time))
                             (userid (or *userid* -1))
                             (ip (or (header-in* :x-real-ip) "")))
  (pprint (headers-in*)) (terpri)
  (send-message *event-mailbox* (make-event :type type
                                            :time time
                                            :text text
                                            :userid userid
                                            :ip ip)))

(defun add-event-handler (event thunk)
  (with-locked-hash-table (*event-handlers*)
    (push thunk (gethash event *event-handlers*))))

(defun test-handler ()
  (pprint *event*)
  (terpri))

(defun thread-loop ()
  (loop
    (let ((*event* (receive-message *event-mailbox*)))
      (if (eq *event* 'exit)
        (return)
        (progn
          (dolist (handler (gethash (event-type *event*) *event-handlers*))
            (funcall handler)) 
          (dolist (handler (gethash :all *event-handlers*))
            (funcall handler)))))))

(defun start-event-thread ()
  (or *event-thread* (setf *event-thread* (make-thread #'thread-loop))))

(defun stop-event-thread ()
  (when *event-thread*
    (send-message *event-mailbox* 'exit)
    (join-thread *event-thread*) 
    (setf *event-thread* nil)))
