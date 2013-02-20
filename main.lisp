(defpackage :kindista.main
  (:use :cl)
  (:import-from :hunchentoot :start :stop :*acceptor*)
  (:import-from :kindista :load-db :save-db :load-tokens :save-tokens)
  ;(:import-from :kindista.db :load-db :save-db)
  ;(:import-from :kindista.tokens :load-tokens :save-tokens)
  (:import-from :kindista.events :start-event-thread :stop-event-thread)
  (:export :run
           :quit))

(in-package :kindista.main)

(defun run ()
  (load-db)
  (load-tokens)
  (start *acceptor*)
  (start-event-thread))

(defun quit ()
  (save-db)
  (save-tokens)
  (stop *acceptor*)
  (stop-event-thread)
  (sb-ext:exit))
