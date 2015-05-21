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

(defvar *acceptor-thread* nil)

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
  (setf *acceptor-thread* (make-thread #'(lambda () (start *acceptor*))))
  (start (acceptor-metric-system *acceptor*))
  (start-notice-thread))

(defun load-notice-handlers ()
  (clrhash *notice-handlers*)
  (add-notice-handler :all #'log-notice)
  (add-notice-handler :account-approval #'account-approval-notice-handler)
  (add-notice-handler :broadcast-email #'broadcast-email-notice-handler)
  (add-notice-handler :new-comment #'new-comment-notice-handler)
  (add-notice-handler :new-feedback #'new-feedback-notice-handler)
  (add-notice-handler :send-invitation #'send-invitation-notice-handler)
  (add-notice-handler :new-pending-offer #'new-pending-offer-notice-handler)
  (add-notice-handler :new-matching-offer #'new-matching-offer-notice-handler)
  (add-notice-handler :new-invite-request #'new-invite-request-notice-handler)
  (add-notice-handler :error #'new-error-notice-handler)
  (add-notice-handler :new-transaction-action #'new-transaction-action-notice-handler)
  (add-notice-handler :new-group-membership-request
                      #'new-group-membership-request-notice-handler)
  (add-notice-handler :new-group-membership-invitation
                      #'new-group-membership-invitation-notice-handler)
  (add-notice-handler :new-gratitude #'new-gratitude-notice-handler)
  (add-notice-handler :updated-notifications #'updated-notifications-handler))

;; first check to make sure *notice-mailbox* is empty
(defun end ()
  (stop (acceptor-metric-system *acceptor*))
  ;;there is currently no way to stop the metric system after the *acceptor* stops
  (stop *acceptor*)
  (save-db)
  (save-tokens)
  (stop-notice-thread))

(defun reboot-acceptor ()
  (stop *acceptor*)
  (terminate-thread *acceptor-thread*)
  (setf *acceptor-thread* (make-thread #'(lambda () (start *acceptor*)))))

(defun reboot-notice-thread ()
  (when (and *notice-thread* (thread-alive-p *notice-thread*))
    (terminate-thread *notice-thread*))
  (setf *notice-thread* nil)
  (start-notice-thread))

(defun quit ()
  (end)
  (sb-ext:exit))
