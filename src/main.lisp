;;; Copyright 2012-2013 CommonGoods Network, Inc.
;;;
;;; This file is part of Kindista.
;;;
;;; Kindista is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Affero General Public License as published
;;; by the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Kindista is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public
;;; License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with Kindista.  If not, see <http://www.gnu.org/licenses/>.

(in-package :kindista)

(defun run ()
  (dolist (symbol '(*convert-path*
                    *original-images*
                    *images-path*
                    *images-base*
                    *avatar-not-found*
                    *metrics-path*))
    (assert symbol))
  (load-notice-handlers)
  (load-db)
  (load-tokens)
  (start *acceptor*)
  (make-thread #'(lambda () (start (acceptor-metric-system *acceptor*))))
  (automatic-invitation-reminders)
  (start-notice-thread))

(defun load-notice-handlers ()
  (clrhash *notice-handlers*)
  (add-notice-handler :all #'log-notice)
  (add-notice-handler :account-approval #'account-approval-notice-handler)
  (add-notice-handler :new-comment #'new-comment-notice-handler)
  (add-notice-handler :new-feedback #'new-feedback-notice-handler)
  (add-notice-handler :send-invitation #'send-invitation-notice-handler)
  (add-notice-handler :new-pending-offer #'new-pending-offer-notice-handler)
  (add-notice-handler :new-invite-request #'new-invite-request-notice-handler)
  (add-notice-handler :new-gratitude #'new-gratitude-notice-handler))

(defun end ()
  (save-db)
  (save-tokens)
  (stop *acceptor*)
  (stop (acceptor-metric-system *acceptor*))
  (when *auto-invite-reminder-timer* (unschedule-timer *auto-invite-reminder-timer*))
  (stop-notice-thread))

(defun quit ()
  (end)
  (sb-ext:exit))
