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
  "subscribe for users who receive either kindista notifications or activity reminders."
  (dolist (userid *active-people-index*)
    (let ((user (db userid)))
      (if (or (getf user :notify-kindista)
                (getf user :notify-reminders))
        (progn
          (modify-db userid :notify-inventory-digest t)
          (incf subscribed-count))
        (modify-db userid :notify-inventory-digest nil))))
  subscribed-count)

(defun html-email-inventory-item
  (id
   &key (item (db id))
        distance
   &aux (type (getf item :type))
        (title (getf item :title))
        (typestring (string-downcase (symbol-name type)))
        (response-type (if (eq type :offer) "request" "offer"))
        (url (strcat *email-url* typestring "s/" id))
        (author (db (getf item :by))))
  "Link title, action button, no other links"

  (html
    (:div :style (s+ *style-p* "border-top: 1px solid #eee;")
      (:div :style "margin: 0.7em 0 0.3em;"
        (awhen title
          (htm
            (:img :src (s+ "http://media.kindista.org/"
                           typestring
                           "s.png")
                  :alt typestring
                  :style "width: 1.47em; height: 1.47em; margin-right: 0.3em;")
            (:h3 :style "font-size: 1.1em; margin-bottom: 0.3em; display: inline;"
              (:a :href url
                  :style "color: #5c8a2f; font-weight: bold; text-decoration: none;"
                  (str it))))))
      (:div :style "margin-bottom: 1em;"
        (htm
          (str (s+ typestring "ed by "))
          (str (getf author :name))
          (awhen distance
            (htm
              (:span :style "font-size: 0.8em;"
                (str (strcat " (within " it " miles)")))))))
      (:div
        (str (ellipsis (getf item :details) :see-more url :email t)))

      (:div
        (:form :method "post" :action url
               (:button :type "submit"
                        :style "text-shadow: 1px 1px rgba(0,0,0,0.4);
                                margin: 0.9em 0.5em 0 0;
                                font-size: 0.8em;
                                padding: 0.3em 0.4em;
                                background: #3c6dc8;
                                vertical-align: middle;
                                cursor: pointer;
                                background: -moz-linear-gradient(
                                 top,
                                 #3c6dc8 0%,
                                 #29519c);
                                background: -ms-linear-gradient(
                                 top,
                                 #3c6dc8 0%,
                                 #29519c);
                                background: -o-linear-gradient(
                                 top,
                                 #3c6dc8 0%,
                                 #29519c);
                                background: -webkit-linear-gradient(
                                 top,
                                 #3c6dc8 0%,
                                 #29519c);
                                background: -webkit-gradient(
                                 linear, left top, left bottom, 
                                 from(#3c6dc8),
                                 to(#29519c));
                                border: 1px solid #474747;
                                text-shadow:
                                 1px 1px 2px rgba(0,0,0,0.4);
                                border-radius: 0.35em;
                                color: #fff;
                                box-shadow: 1px 1px 0px rgba(255,255,255,0.2), inset 1px 1px 0px rgba(209,209,209,0.3);"
                        :name "action-type"
                        :value typestring
                        (:img :src (s+ "http://media.kindista.org/white-"
                                       response-type
                                       ".png")
                              :alt response-type
                              :style "vertical-align: middle; width: 1.47em; height: 1.47em; margin-right: 0.3em;" 
                         ) 
                        ;; following needs div instead of span because of a
                        ;; firefox hover/underline bug
                        (:div :style "display: inline; font-weight: bold;"
                          (str (s+ (string-capitalize response-type) " This")))))))))

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
    (awhen (getf item :title) it)
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
   &aux (distance (min 50 (max 5 (or (getf user :rdist) 25))))
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
        (when (= (mod userid 7) day)
          (prin1 (recent-local-inventory userid) file))))))

