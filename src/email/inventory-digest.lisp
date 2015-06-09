;;; Copyright 2012-2015 CommonGoods Network, Inc.
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


(defun plain-text-inventory-item
  (id
   &key (item (db id))
        distance
   &aux (type (getf item :type))
        (typestring (symbol-name type))
        (author (db (getf item :by))))

  (strcat*
    "--------------------------------------------------------"
    #\linefeed
    typestring
    (awhen (getf item :title)
      (s+ ": " it))
    #\linefeed
    (string-capitalize (string-downcase typestring))
    "ed by "
    (getf author :name)
    (awhen distance
      (strcat " (within " it " miles)"))
    #\linefeed
    #\linefeed
    (ellipsis (getf item :details) :plain-text t)
    #\linefeed
    "Link: "
    +base-url+
    (string-downcase typestring)
    "s/"
    id))

(defun recent-local-inventory
  (userid
   &key (user (db userid))
        (timeframe +week-in-seconds+)
   &aux (distance (min 25 (max 5 (or (getf user :rdist) 25))))
        (lat (getf user :lat))
        (long (getf user :long))
        (now (get-universal-time))
        offer-count
        request-count
        offers
        requests)
  "Returns up to 24 recent inventory items."

  (when (and (getf user :location) lat long)
    (labels ((rank (item)
               (activity-rank item :contacts (getf user :following)
                                   :lat lat
                                   :long long))
             (get-inventory (index)
               (sort (remove-if #'(lambda (result)
                                    (or (find userid (result-people result))
                                        (< (result-time result)
                                           (- now timeframe))
                                        (item-view-denied
                                          (result-privacy result)
                                          userid)))
                                (geo-index-query index lat long distance))
                     #'>
                     :key #'rank)))

      (setf offers (get-inventory *offer-geo-index*))
      (setf offer-count (length offers))
      (setf requests (get-inventory *request-geo-index*))
      (setf request-count (length requests))

      (cond
        ((and (> offer-count 11)
              (> request-count 11))
         (asetf offers (subseq it 0 12))
         (asetf requests (subseq it 0 12)))
        ((< request-count 12)
         (asetf offers (subseq it 0 (min (- 25 request-count)
                                         offer-count))))
        ((< offer-count 12)
         (asetf requests (subseq it 0 (min (- 25 offer-count)
                                           request-count)))))

      (list :offers offers
            :requests requests
            ))))


