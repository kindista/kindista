;;; Copyright 2015-2021 CommonGoods Network, Inc.
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

;(defun subscribe-current-users-to-inventory-digest
;  (&aux (subscribed-count 0))
;  "subscribe for users who receive either kindista notifications or activity reminders."
;  (dolist (userid *active-people-index*)
;    (let ((user (db userid)))
;      (if (or (getf user :notify-kindista)
;                (getf user :notify-reminders))
;        (progn
;          (modify-db userid :notify-inventory-digest t)
;          (incf subscribed-count))
;        (modify-db userid :notify-inventory-digest nil))))
;  subscribed-count)


(defvar *last-inventory-digst-mailer-time* 0)

(defun get-daily-inventory-digest-mailer
  (&aux (day (local-time:timestamp-day-of-week (local-time:now))) )
  (declare (optimize (space 2) (speed 1) (debug 0) (safety 1)))
  (when (and (or (getf *user* :admin)
                 (server-side-request-p))
             ;; wait if last called less than 22 hours ago
             ;;(in case of daylight savings time)
             (< *last-inventory-digst-mailer-time*
                (- (get-universal-time) 79200)))

    (setf *last-inventory-digst-mailer-time* (get-universal-time))

    (dolist (userid *active-people-index*)
      ;; get 1/7th of the userbase
      (when (= (mod userid 7) day)
        (let ((user (db userid)))
          (when (getf user :notify-inventory-digest)

            ;; only send emails in a production environment
            (if *productionp*
              (send-inventory-digest-email userid :user user)
              (test-inventory-digest-for-user userid user))))))
    (see-other "/home")))

(defun test-inventory-digest-for-user (userid &optional (user (db userid)))
  (with-open-file (s (strcat +db-path+
                             "/tmp/inventory-digest-test/"
                             userid)
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
    (let* ((inventory-items (recent-local-inventory userid :user user))
           (offers (getf inventory-items :offers))
           (requests (getf inventory-items :requests))
           (text (inventory-digest-email-text userid
                                              inventory-items
                                              :user user))
           (html (inventory-digest-email-html userid
                                              inventory-items
                                              :user user)))
      (when (or offers requests)
        (print (list (first (getf user :emails)) text html) s)))))


(defun send-inventory-digest-email
  (userid
   &key (user (db userid))
   &aux (inventory-items (recent-local-inventory userid :user user))
        (offers (getf inventory-items :offers))
        (requests (getf inventory-items :requests))
        (email (first (getf user :emails)))
        (text (inventory-digest-email-text userid
                                           inventory-items
                                           :user user))
        (html (inventory-digest-email-html userid
                                           inventory-items
                                           :user user)))
  (when (and email (getf user :notify-inventory-digest)
             (or offers requests))
    (cl-smtp:send-email +mail-server+
                        "Kindista <info@kindista.org>"
                         (format nil "\"~A\" <~A>" (getf user :name) email)
                         (strcat* "Recent Kindista "
                                  (when offers " Offers ")
                                  (when (and offers requests) "and")
                                  (when requests " Requests ")
                                  "in Your Area")
                         text
                         :html-message html)))

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
    (labels
      ((rank (item)
         (activity-rank item :user user
                             :userid userid
                             :contact-multiplier 5
                             :distance-multiplier 5))
       (get-inventory (index)
         (sort
           (mapcar (lambda (result) (cons result (rank result)))
                   (remove-if
                     (lambda (result)
                       (or (find userid (result-people result))
                           (< (result-time result)
                              (- now timeframe))
                           (item-view-denied
                             (result-privacy result)
                             userid)
                           (db (result-id result) :refreshed)))
                     (geo-index-query index lat long distance)))
           #'>
           :key #'cdr)))

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

      (let* ((offer-ids (mapcar (lambda (result-cons)
                                  (result-id (car result-cons)))
                                offers))
             (request-ids (mapcar (lambda (result-cons)
                                    (result-id (car result-cons)))
                                  requests))
             (featured-offers-count (length offer-ids))
             (featured-requests-count (length request-ids)))

       (list :offers offer-ids
             :more-offers (when (> offer-count featured-offers-count)
                            (- offer-count featured-offers-count))
             :requests request-ids
             :more-requests (when (> request-count featured-requests-count)
                              (- request-count featured-requests-count)))))))

(defun inventory-digest-email-text
  (userid
   recent-items
   &key (user (db userid))
   &aux (name (getf user :name))
        (offers (getf recent-items :offers))
        (requests (getf recent-items :requests))
        (more-offers (getf recent-items :more-offers))
        (more-requests (getf recent-items :more-requests)))

(labels ((item-text (item-id)
           (email-inventory-item-plain-text item-id :user user)))

  (strcat* "Hi " name ","
           #\linefeed #\linefeed
           "Here are some"
           (when offers " offers ")
           (when (and offers requests) "and")
           (when requests " requests ")
           "your neighbors have posted to Kindista during the past week. "
           "You are currently subscribed to receive notifications about items posted within "
           (getf user :rdist)
           " miles. You can change this distance on your settings page: "
           *email-url*
           "settings/communication#digest-distance"
           (when offers
             (strcat #\linefeed #\linefeed
                     "OFFERS"
                     #\linefeed
                     (apply #'strcat (mapcar #'item-text offers))))
           (when more-offers
             (strcat* #\linefeed
                      "See "
                      more-offers
                      " more recent offer"
                      (when (> more-offers 1) "s")
                      ": "
                      *email-url*
                      "offers"
                       #\linefeed))
           (when requests
             (strcat* #\linefeed
                      (unless offers #\linefeed)
                      "REQUESTS"
                      #\linefeed
                      (apply #'strcat (mapcar #'item-text requests))))
           (when more-requests
             (strcat* #\linefeed
                      "See "
                      more-requests
                       " more recent request"
                       (when (> more-requests 1) "s")
                       ": "
                      *email-url*
                      "requests"
                       #\linefeed))

           #\linefeed #\linefeed
           *integrity-reminder*
           #\linefeed #\linefeed
           (amazon-smile-reminder)
           (unsubscribe-notice-ps-text (getf user :unsubscribe-key)
                                       (car (getf user :emails))
                                       "email summaries of new offers and requests in your area"
                                       :unsub-type "inventory-digest"))))

(defun inventory-digest-email-html
  (userid
    recent-items
    &key (user (db userid))
    &aux (offers (getf recent-items :offers))
         (requests (getf recent-items :requests))
         (more-offers (getf recent-items :more-offers))
         (more-requests (getf recent-items :more-requests)) )

  (html-email-base
    (html
      (:p :style *style-p*
       "Hi " (str (getf user :name)) ",")

      (:p :style *style-p*
       "Here are some "
       (str (s+ (when offers " offers ")
                (when (and offers requests) "and")
                (when requests " requests ")))
       "your neighbors have posted to Kindista during the past week. "
       "You are currently subscribed to receive notifications about items posted within "
       (str (getf user :rdist))
       " miles. You can change this distance on your "
       (:a :href (s+ *email-url* "settings/communication#digest-distance")
           :style *style-a*
        "settings page")
       ".")

      (:h2 "OFFERS")

      (dolist (offer offers)
        (str (email-inventory-item-html offer :user user)))

      (awhen more-offers
        (htm (:a :href (s+ *email-url* "offers")
                 :style *style-a*
               (str (strcat* "See "
                             more-offers
                             " more recent offer"
                             (when (> more-offers 1) "s"))))))

      (:h2 "REQUESTS")

      (dolist (request requests)
        (str (email-inventory-item-html request :user user)))

      (awhen more-requests
        (htm (:a :href (s+ *email-url* "requests")
                 :style *style-a*
               (str (strcat* "See "
                             more-requests
                             " more recent request"
                             (when (> more-requests 1) "s"))))))

      (:p :style *style-p* (str *integrity-reminder*))

      (str (amazon-smile-reminder t))

      (str (unsubscribe-notice-ps-html
             (getf user :unsubscribe-key)
             (car (getf user :emails))
             "email summaries of new offers and requests in your area"
             :unsub-type "inventory-digest")))))

(defun email-inventory-item-html
  (id
   &key (item (db id))
        user
   &aux (type (getf item :type))
        (result (gethash id *db-results*))
        (item-lat (result-latitude result))
        (item-long (result-longitude result))
        (user-lat (getf user :lat))
        (user-long (getf user :long))
        (distance (when (and item-lat item-long user-lat user-long)
                    (distance-string
                      (air-distance item-lat item-long user-lat user-long))))
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
            (:img :src (s+ *email-url* "media/icons/"
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
                (str (strcat " (within " it ")")))))))
      (:div :style "margin-bottom: 1em;"
        (str (ellipsis (getf item :details) :see-more url :email t)))

      (str (email-action-button
             (s+ url "?action-type=" response-type)
             (s+ (string-capitalize response-type) " This")
             :style *small-style-button* 
             :image (html (:img :src (s+ *email-url* "media/icons/white-"
                                    response-type
                                    ".png")
                                :alt response-type
                                :style "vertical-align: middle;
                                        width: 1.2em;
                                        height: 1.2em;
                                        margin-right: 0.3em;")))))))

(defun email-inventory-item-plain-text
  (id
   &key (item (db id))
        user
   &aux (type (getf item :type))
        (result (gethash id *db-results*))
        (item-lat (result-latitude result))
        (item-long (result-longitude result))
        (user-lat (getf user :lat))
        (user-long (getf user :long))
        (distance (when (and item-lat item-long user-lat user-long)
                    (distance-string
                      (air-distance item-lat item-long user-lat user-long))))
        (typestring (symbol-name type))
        (author (db (getf item :by))))

  (strcat*
    #\linefeed
    (awhen (getf item :title) it)
    #\linefeed
    (string-capitalize (string-downcase typestring))
    "ed by "
    (getf author :name)
    (awhen distance
      (strcat " (within " it ")"))
    #\linefeed
    #\linefeed
    (ellipsis (getf item :details) :plain-text t)
    #\linefeed
    "Link: "
    +base-url+
    (string-downcase typestring)
    "s/"
    id
    #\linefeed
    "------------------------------------"
    #\linefeed))

