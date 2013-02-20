(defpackage :kindista.events
  (:use :cl)
  (:nicknames :k.ev)
  (:import-from :sb-ext :with-locked-hash-table)
  (:import-from :sb-thread :make-thread :join-thread)
  (:import-from :sb-concurrency :make-mailbox :receive-message :send-message)
  (:import-from :hunchentoot :header-in*)
  (:import-from :kindista :*userid*)
  (:export :*event*
           :event
           :add-event-handler
           :start-event-handler
           :page-view))

(in-package :kindista.events)

(defvar *event*) ; used as a dynamic variable for event handlers
(defvar *thread* nil)
(defvar *mailbox* (make-mailbox :name "event reader"))
(defvar *handlers* (make-hash-table :synchronized t :size 100 :test 'eq))

(defstruct event
  (type :info :type symbol)
  (time (get-universal-time) :type integer)
  (text "" :type string)
  (userid -1 :type integer)
  (ip "" :type string))

(defun event (type text &key (time (get-universal-time))
                             (userid *userid*)
                             (ip (header-in* :x-forwarded-for)))
  (send-message *mailbox* (make-event :type type
                                      :time time
                                      :text text
                                      :userid userid
                                      :ip ip)))

(defun add-event-handler (event thunk)
  (with-locked-hash-table (*handlers*)
    (push thunk (gethash event *handlers*))))

(defun test-handler ()
  (pprint *event*)
  (terpri))

(defun thread-loop ()
  (loop
    (let ((*event* (receive-message *mailbox*)))
      (if (eq *event* 'exit)
        (return)
        (progn
          (dolist (handler (gethash (event-type *event*) *handlers*))
            (funcall handler)) 
          (dolist (handler (gethash :all *handlers*))
            (funcall handler)))))))

(defun start-event-thread ()
  (or *thread* (setf *thread* (make-thread #'thread-loop))))

(defun stop-event-thread ()
  (when *thread*
    (send-message *mailbox* 'exit)
    (join-thread *thread*) 
    (setf *thread* nil)))
