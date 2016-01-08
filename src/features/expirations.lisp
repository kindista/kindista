;;; Copyright 2016 CommonGoods Network, Inc.
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

(defun index-inventory-refresh-time (result)
  (with-mutex (*inventory-refresh-timer-mutex*)
       (setf *inventory-refresh-timer-index*
             (safe-sort (push result *inventory-refresh-timer-index*)
                        #'<
                        :key #'result-time))))

(defun deindex-inventory-refresh-time (result)
  (with-mutex (*inventory-refresh-timer-mutex*)
       (setf *inventory-refresh-timer-index*
             (remove result *inventory-refresh-timer-index*))))

(defun index-inventory-expiration (id data)
  (when (getf data :expires)
    (with-mutex (*inventory-expiration-timer-mutex*)
      (setf *inventory-expiration-timer-index*
            (safe-sort (push (list :expires (getf data :expires)
                                   :id id
                                   :expiration-notice (getf data :expiration-notice))
                             *inventory-expiration-timer-index*)
                       #'<
                       :key #'(lambda (item) (getf item :expires)))))))

(defun deindex-inventory-expiration (id data)
  (when (getf data :expires)
    (with-mutex (*inventory-expiration-timer-mutex*)
        (asetf *inventory-expiration-timer-index*
               (remove id
                       it
                       :count 1
                       :key #'(lambda (item) (getf item :id)))))))

(defun add-expiration-dates-to-existing-inventory-items
  (&aux (now (get-universal-time))
        (month-in-seconds (* 30 +day-in-seconds+))
        (3-months (* 13 +week-in-seconds+))
        (3-months-ago (- now 3-months)))
  "Should only be run once on the live server to add expiration dates to inventory items that don't already have them"
  (dolist (id (hash-table-keys *db*))
    (let* ((item (db id))
           (type (getf item :type))
           (created (getf item :created)))
      (when (and (or (eq type :offer)
                     (eq type :request))
                 (getf item :active))
        (let ((rand (random month-in-seconds)))
          (if (< created 3-months-ago)
            (modify-db id :expires (+ now +week-in-seconds+ rand))
            (modify-db id :expires (+ now 3-months rand)))))

      (when (and (eq (getf item :type) :person)
                 (getf item :active))
        (modify-db id :notify-inventory-expiration t)))))

(defun get-inventory-expiration-reminders
  (&aux expiring-soon
        expired
        (now (get-universal-time))
        (10days (+ now (* 10 +day-in-seconds+)))
        (4days (+ now (* 4 +day-in-seconds+))))
  (when (or (not *productionp*)
            (getf *user* :admin)
            (server-side-request-p))
    (flet ((remind-user (id &optional data)
             (send-inventory-expiration-notice id)
             (modify-db id :expiration-notice now)
             ;; expiration-notice time changes in the expiration-index
             (when data
               (deindex-inventory-expiration id data)
               (index-inventory-expiration id data))))
      (dolist (item (copy-list *inventory-expiration-timer-index*))
        (let* ((time (getf item :expires))
               (id (getf item :id))
               (userid (db id :by)))
          (if (< (getf item :expires) 4days)
            (cond
             ((< time now)
              (if (= userid 1)
                (remind-user id)
                (push (cons id (humanize-universal-time time)) expired))
              (deactivate-inventory-item id))
             ;; send reminders for items expiring soon
             ((> time now)
              (when (= userid 1)
                (let* ((item (db id))
                       (recent-reminder (getf item :expiration-notice)))
                  (when (or (not recent-reminder)
                            (> recent-reminder 10days))
                    (remind-user id item))))
              (push (cons id (humanize-future-time time)) expiring-soon)))
            (return)))))
    (if *productionp*
      (see-other "/home")
      (values expired expiring-soon))))

(defun get-inventory-refresh
  (&aux refreshed-items
        (now (get-universal-time))
        (-1hour (- now 3600)))
  (when (or (not *productionp*)
            (getf *user* :admin)
            (server-side-request-p))
    (dolist (result *inventory-refresh-timer-index*)
      (if (< (result-time result)
             (- now (* 4 +week-in-seconds+)))
        (let ((id (result-id result)))
          (refresh-item-time-in-indexes id
                                        :time -1hour
                                        :server-side-trigger-p t)
          (modify-db id :refreshed -1hour))
        (return))
    (if *productionp*
      (see-other "/home")
      refreshed-items))))
