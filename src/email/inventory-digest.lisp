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

(defun subscribe-current-users-to-inventory-digest
  (&aux (subscribed-count 0))
  (dolist (userid *active-people-index*)
    (let ((user (db userid)))
      (incf active-users)
      (when (getf user :notify-message) (incf message))
      (when (getf user :notify-reminders) (incf reminders))
      (when (getf user :notify-expired-invites) (incf expired-invites))
      (when (getf user :notify-blog) (incf blog))
      (when (getf user :notify-kindista) (incf kindista))
      (when (and (getf user :notify-expired-invites)
                 (getf user :notify-reminders)
                 (getf user :notify-blog)
                 (getf user :notify-kindista)
                 (and (getf user :location)
                   (< (air-distance lat long
                                  (getf user :lat)
                                  (getf user :long)
                                  )
                    50
                    ))
                 )
        (incf all))
      (when (or (getf user :notify-expired-invites)
                (getf user :notify-reminders)
                (getf user :notify-blog)
                (getf user :notify-kindista))
        (incf any))) 
    )
  subscribed-count
  )

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
                                   :contact-multiplier 10
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

      (list :offers (mapcar #'result-id  offers)
            :requests (mapcar #'result-id requests)))))


(defun daily-inventory-digest-mailer
  (&aux (day (local-time:timestamp-day-of-week
               (universal-to-timestamp (get-universal-time)))))
  (with-open-file (file (s+ "/home/ben/kindista/data/tmp/inventory-digest")
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (with-standard-io-syntax
      (dolist (userid *active-people-index*)
        ;; get 1/7th of the userbase
        (when (= (mod userid day) 0)
          (prin1 (recent-local-inventory userid) file))))))

