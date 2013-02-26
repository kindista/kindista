(in-package :kindista)

(defun run ()
  (add-notice-handler :all #'log-notice)
  (load-db)
  (load-tokens)
  (start *acceptor*)
  (start-notice-thread))

(defun end ()
  (save-db)
  (save-tokens)
  (stop *acceptor*)
  (stop-notice-thread))

(defun quit ()
  (end)
  (sb-ext:exit))
