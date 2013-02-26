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


(let ((months (vector nil
                      "January"
                      "February"
                      "March"
                      "April"
                      "May"
                      "June"
                      "July"
                      "August"
                      "September"
                      "October"
                      "November"
                      "December")))

  (defun humanize-universal-time (then)
    (let* ((then-date (multiple-value-list (decode-universal-time then)))
           (now (get-universal-time))
           (now-date (multiple-value-list (decode-universal-time now))))
      (if (eql (sixth then-date) (sixth now-date))
        (let ((delta (- now then)))
          (cond
            ((< delta 60)
             "just now")
            ((< delta 120)
             "a minute ago")
            ((< delta 3300)
             (format nil "~a minutes ago" (round (/ delta 60))))
            ((< delta 5400)
             "about an hour ago")
            ((< delta 86400)
             (format nil "~a hours ago" (round (/ delta 3600))))
            ((< delta 172800)
             (format nil "yesterday at ~a:~2,'0d" (third then-date) (second then-date)))
            (t
             (format nil "~a ~a at ~a:~2,'0d" (elt months (fifth then-date))
                                              (fourth then-date)
                                              (third then-date)
                                              (second then-date)))))
        (format nil "~a ~a, ~a at ~a:~2,'0d" (elt months (fifth then-date))
                                             (fourth then-date)
                                             (sixth then-date)
                                             (third then-date)
                                             (second then-date))))))


