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

(defun create-event (&key (by *userid*) time lat long title)
  (insert-db (list :type :event
                   :by by
                   :time time
                   :lat lat
                   :long long
                   :title title
                   :created (get-universal-time))))

(defun index-event (id data)
  (let* ((by (getf data :by))
         (type (getf data :type))
         (result (make-result :latitude (or (getf data :lat) (getf (db (getf data :by)) :lat))
                              :longitude (or (getf data :long) (getf (db (getf data :by)) :long))
                              :id id
                              :type :event
                              :people (list by)
                              :time (or (getf data :edited) (getf data :created)))))

    (with-locked-hash-table (*db-results*)
      (setf (gethash id *db-results*) result))

    (geo-index-insert *event-geo-index* result)
    (geo-index-insert *activity-geo-index* result)))

(defun upcoming-events (&key (page 0) (count 20) (distance 50))
  (with-location
    (let ((items ;(sort
                   (geo-index-query *event-geo-index*
                                    *latitude*
                                    *longitude*
                                    distance)
                       ;#'< :key #'activity-rank)
          )
          (start (* page 20)))
      (iter (for i from 0 to (+ start count))
            (cond
              ((< i start)
               (setf items (cdr items)))

              ((and (>= i start) items)
               (collect (car items))
               (setf items (cdr items)))

              (t
               (finish)))))))
