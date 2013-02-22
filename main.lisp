(in-package :kindista)

(defun run ()
  (add-event-handler :all #'log-event)
  (load-db)
  (load-tokens)
  (start *acceptor*)
  (start-event-thread))

(defun end ()
  (save-db)
  (save-tokens)
  (stop *acceptor*)
  (stop-event-thread))

(defun quit ()
  (end)
  (sb-ext:exit))
