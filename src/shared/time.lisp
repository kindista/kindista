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

(defun humanize-universal-time (then)
  (let ((delta (- (get-universal-time) then)))
    (cond
      ((< delta 60)
       "just now")
      ((< delta 120)
       "a minute ago")
      ((< delta 3300)
       (format nil "~a minutes ago" (round (/ delta 60))))
      ((< delta 5400)
       "about an hour ago")
      ((< delta 129600)
       (format nil "~a hours ago" (round (/ delta 3600))))
      ((< delta 4320000)
       (format nil "~a days ago" (round (/ delta 86400))))
      (t
       (format nil "~a months ago" (round (/ delta 2492000)))))))

(defun humanize-future-time (time)
  (let* ((now (get-universal-time))
         (seconds (- time now)))
    (cond
      ((< seconds 60)
       "in less than a minute")
      ((< seconds 120)
       "in about a minute")
      ((< seconds 3600)
       (strcat "in " (floor (/ seconds 60)) " minutes"))
      ((< seconds 7200)
       "in about an hour")
      ((< seconds 86400)
       (strcat "in " (floor (/ seconds 3600)) " hours"))
      ((< seconds 172800)
       "tomorrow")
      ((< seconds 2678400)
       (strcat "in " (floor (/ seconds 86400)) " days"))
      ((< seconds 5270400)
       "next month")
      ((< seconds 31536000)
       (strcat "in " (floor (/ seconds 2628000)) " months"))
      ((< seconds 63072000)
       "next year")
      (t
       (strcat "in " (floor (/ seconds 31536000)) " years")))))

(defun inline-timestamp (time &key type url)
  (let ((inner (html
                 (when type
                   (htm (str type) " "))
                 (str (humanize-universal-time time)))))
    (html
      (:span :class "timestamp" :data-time time :data-type type
        (if url
          (htm (:a :href url (str inner)))
          (str inner))))))
