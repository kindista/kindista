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

(defun create-event (&key (host *userid*) lat long title details local-time address recurring frequency interval days-of-week by-day-or-date days-of-month local-end-date)
  (insert-db (remove-nil-plist-pairs (list :type :event
                                           :hosts (list host)
                                           :lat lat
                                           :long long
                                           :address address
                                           :title title
                                           :details details
                                           :local-time local-time
                                           :recurring recurring
                                           :frequency frequency
                                           :interval interval
                                           :days-of-week days-of-week
                                           :by-day-or-date by-day-or-date
                                           :days-of-month days-of-month
                                           :local-end-date local-end-date
                                           :created (get-universal-time)))))

(defun index-event (id data)
  (let* ((by (getf data :hosts))
         (now (get-universal-time))
         (result (make-result :latitude (or (getf data :lat) (getf (db (getf data :by)) :lat))
                              :longitude (or (getf data :long) (getf (db (getf data :by)) :long))
                              :id id
                              :type :event
                              :people by
                              :time (getf data :local-time))))

    (with-locked-hash-table (*db-results*)
      (setf (gethash id *db-results*) result))

    (when (getf data :recurring)
      (with-locked-hash-table (*recurring-events-index*)
        (flet ((get-property-value (property)
                 (cons property (getf data property))))
          (let (event-details)
            (dolist (pair (mapcar #'get-property-value
                                  (list :frequency
                                        :interval
                                        :days-of-week
                                        :by-day-or-date
                                        :days-of-month
                                        :end-date)))
              (awhen (cdr pair)
                (nconcf event-details (list (car pair) it))))
             (setf (gethash id *recurring-events-index*) event-details)))))

    (unless (< (result-time result) (- now +day-in-seconds+))
      (with-mutex (*event-mutex*)
        ; remove past events from event-index
        ; add this event and sort the index
        (flet ((past-event (item) (< (result-time item)
                                     (- now (* 3 +day-in-seconds+)))))
          (asetf *event-index* (sort (cons result (remove-if #'past-event it))
                                 #'< :key #'result-time))))
      (geo-index-insert *event-geo-index* result))

    (let ((stems (stem-text (s+ (getf data :title) " " (getf data :details)))))
      (with-locked-hash-table (*event-stem-index*)
        (dolist (stem stems)
          (push result (gethash stem *event-stem-index*)))))))

(defun modify-event (id &key lat long title details local-time address recurring frequency interval days-of-week by-day-or-date days-of-month local-end-date)
  (let* ((result (gethash id *db-results*))
         (data (db id))
         (old-address (getf data :address))
         (old-title (getf data :title))
         (old-details (getf data :details))
         (old-time (getf data :local-time))
         (old-lat (getf data :lat))
         (old-long (getf data :long))
         (old-recurring (getf data :recurring))
         (old-frequency (getf data :frequency))
         (old-interval (getf data :interval))
         (old-days-of-week (getf data :days-of-week))
         (old-by-day-or-date (getf data :by-day-or-date))
         (old-days-of-month (getf data :days-of-month))
         (old-end-date (getf data :local-end-date))
         (now (get-universal-time))
         (new-data nil))

    (flet ((update-unless (clause &rest body)
             (unless clause body (setf new-data t))))

      (update-unless (and (equalp title old-title)
                          (equalp details old-details))
        (let* ((oldstems (stem-text (s+ old-title " " old-details)))
               (newstems (stem-text (s+ title " " details)))
               (common (intersection oldstems newstems :test #'string=)))

          (flet ((commonp (stem)
                   (member stem common :test #'string=)))
            (setf oldstems (delete-if #'commonp oldstems))
            (setf newstems (delete-if #'commonp newstems))
            (with-locked-hash-table (*event-stem-index*)
              (dolist (stem oldstems)
                (asetf (gethash stem *event-stem-index*)
                       (remove result it))))
              (dolist (stem newstems)
                (push result (gethash stem *event-stem-index*))))))

      (update-unless (and (= lat old-lat)
                          (= long old-long))
        (geo-index-remove *event-geo-index* result)
        (setf (result-latitude result) lat)
        (setf (result-longitude result) long)
        (geo-index-insert *event-geo-index* result))

      (update-unless (= local-time (getf data :local-time))
        (setf (result-time result) local-time)
        (with-mutex (*event-mutex*)
          ; remove past events from event-index
          ; add this event and sort the index
          (flet ((past-event (item) (< (result-time item)
                                       (- now (* 3 +day-in-seconds+)))))
            (asetf *event-index* (sort (remove-if #'past-event it)
                                   #'< :key #'result-time))))))

    (when new-data
      (modify-db id :title (or title old-title)
                    :details (or details old-details)
                    :lat (or lat old-lat)
                    :long (or long old-long)
                    :address (or address old-address)
                    :local-time (or local-time old-time)
                    :edited (unless (and (getf *user* :admin)
                                         (member *userid* (getf data :host)))
                              now)))))

(defun upcoming-events (items &key (page 0) (count 20) paginate url (location t) (sidebar nil))
  (let ((start (* page count))
        (calendar-date nil))
    (html
      (iter (for i from 0 to (- (+ start count) 1))
            (cond
              ((< i start)
               (setf items (cdr items)))

              ((and (>= i start) items)
               (let* ((item (car items))
                      (item-time (result-time item)))
                 (multiple-value-bind (time date)
                     (humanize-exact-time item-time)
                   (unless (string= date calendar-date)
                     (setf calendar-date date)
                     (htm (:h3 :class "event-date" (str calendar-date))))

                   (htm (str (event-activity-item item
                                                  :sidebar sidebar
                                                  :time time
                                                  :truncate t
                                                  :show-distance location)))))
               (setf items (cdr items)))

              (t
               (when (and sidebar
                          (> (length items) count))
                 (htm (:a :href "/events" "see more events")))

               (when (and (not sidebar)
                          (< (user-distance) 100)
                          (> (user-distance) 0))
                 (htm
                   (:div :class "item small"
                    (:em "Increasing the ")(:strong "show activity within")(:em " distance may yield more results."))))
               (finish)))

            (finally
              (when (and paginate (or (> page 0) (cdr items)))
                (htm
                  (:div :class "item"
                   (when (> page 0)
                     (htm
                       (:a :href (strcat url "?p=" (- page 1)) "< previous page")))
                   "&nbsp;"
                   (when (cdr items)
                     (htm
                       (:a :style "float: right;" :href (strcat url "?p=" (+ page 1)) "next page >")))))))))))

(defun local-upcoming-events (&key (page 0) (count 20) (url "/events") (paginate t) (sidebar nil))
  (with-location
    (let ((distance (user-distance)))
      (flet ((show-local-events (list)
               (upcoming-events list :page page
                                     :count count
                                     :url url
                                     :sidebar sidebar
                                     :paginate paginate)))
        (if (= distance 0)
          (show-local-events
            (remove-if #'stale-eventp
                       (sort (copy-list *event-index*)
                             #'< :key #'result-time)))

          (show-local-events
            (remove-if #'stale-eventp
                       (sort (geo-index-query *event-geo-index*
                                              *latitude*
                                              *longitude*
                                              distance)
                               #'< :key #'result-time))))))))

(defun events-rightbar ()
  (html
    (str (login-sidebar))
    (str (donate-sidebar))
    (str (invite-sidebar))))

(defun get-events-new ()
  (require-active-user
    (if (getf *user* :pending)
      (progn
        (pending-flash "post events on Kindista")
        (see-other (or (referer) "/home")))
      (enter-event-details))))

(defun enter-event-details (&key error date time location title groupid details existing-url recurring frequency interval days-of-week by-day-or-date days-of-month end-date local-day-of-week)
  (standard-page
    (if existing-url "Edit your event details" "Create a new event")
    (html
      (when error
        (flash error :error t))
      (:div :class "item"
       (:h2 (str (if existing-url "Edit your event details"
                                  "Create a new event")))
       (:div :class "item create-event"
        (:form :method "post"
               :action (or existing-url "/events/new")
         (:input :type "hidden" :name "next" :value (referer))

         (:div
           (:label "When (date & time)")
           (:input :type "text"
                   :name "date"
                   :placeholder "mm/dd/yyyy"
                   :value date)
           (:br)
           (:input :type "text"
                   :name "time"
                   :placeholder "Add a time? (ex. 2:30 PM)"
                   :value time))

         (:div
           (:input :type "checkbox"
                   :name "recurring"
                   :checked (when recurring "checked")
                   :onclick "this.form.submit()"
                   "Repeat..."))

         (when recurring
           (htm
             (:div
               (:label "Repeat Every")
               (str (number-selection-html "interval" 12
                                           :selected (or interval 1)
                                           :auto-submit t)))

             (:div
               (:select :onchange  "this.form.submit()"
                        :name "frequency"
                 (:option :value "weekly"
                          :selected (unless (string= frequency "monthly") "")
                          "Week"
                          (unless (or (not interval) (= interval 1))
                            (htm "s")))
                 (:option :value "monthly"
                          :selected (when (string= frequency "monthly") "")
                          "Month"
                          (unless (or (not interval) (= interval 1))
                            (htm "s")))))

             (when (and (or (not frequency)
                            (string= frequency "weekly"))
                        (or (not interval)
                            (= interval 1)))
               (htm
                 (:div
                   (:label "Repeat on")
                     (dolist (day +day-names+)
                       (htm
                         (:input :type "checkbox"
                                 :name "days-of-week"
                                 :checked (when (or (find day
                                                          days-of-week
                                                          :test #'equalp)
                                                    (equalp local-day-of-week
                                                            day))
                                            "checked")
                                 :value (string-downcase day)
                                 (str (elt day 0))))))))

             (when (string= frequency "monthly")
               (htm
                 (:div
                   (:label "Repeat by")
                   (:input :type "radio"
                           :name "by-day-or-date"
                           :value "day"
                           :onclick "this.form.submit()"
                           :checked (unless (string= by-day-or-date "date")
                                      ""))
                   "day of the week"
                   (:input :type "radio"
                           :name "by-day-or-date"
                           :value "date"
                           :onclick "this.form.submit()"
                           :checked (when (string= by-day-or-date "date") ""))
                   "date (the "
                   (str (humanize-number
                          (day-of-month date :formatted-date t)))
                   " of the month)")

                 (unless (string= by-day-or-date "date")
                   (htm
                     (:div
                       (:label "Days of the month")
                         (dolist (option +positions-of-day-in-month+)
                           (htm
                             (:input :type "checkbox"
                                     :name "days-of-month"
                                     :checked (when
                                                (or (find option
                                                          days-of-month
                                                          :test #'string=)
                                                    (string=
                                                      option
                                                      (position-of-day-in-month
                                                         date
                                                         :formatted-date t)))
                                                "checked")
                                     :value option
                                     (str option) " " (str local-day-of-week)))))))))

             (:div
               (:label "End Date (optional)")
               (:input :type "text"
                       :name "end-date"
                       :placeholder "mm/dd/yyyy"
                       :value end-date))))

         (:div :class "long"
           (:label "Event title")
           (:input :type "text"
                   :name "title"
                   :placeholder "ex: Community Garden Work Party"
                   :value (awhen title (escape-for-html it))))
         (unless existing-url
           (awhen (groups-with-user-as-admin)
             (htm
               (:div
                 (:label "Posted by")
                 (str (identity-selection-html (or groupid *userid*)
                                               it
                                               :class "identity event-host"))))))

         (:div
           (:label "Details")
           (:textarea :rows "8"
                       :name "details"
                       :placeholder "Add some more info..."
                       (awhen details (str (escape-for-html it)))))

         (:div :class "long"
           (:label "Where")
           (:input :type "text"
                   :name "location"
                   :placeholder "Street address for the event"
                   :value location))
         (:p (:em "Please note: the address you enter for your event will be visible to anyone using Kindista."))

         (:p
           (:strong "Important: ")
           "Please read the "
           (:a :href "/faq#event-guidelines"
             "Guidelines for Posting Events on Kindista")
           " before posting any events to the calendar. "
           "All events on Kindista are subject to these guidelines.")

         (:button :class "cancel"
                  :type "submit"
                  :name "cancel"
                  "Cancel")

         (if existing-url
           (htm (:button :class "yes"
                         :type "submit"
                         :name "submit-edits"
                         :value "1"
                         "Submit changes"))
           (htm (:button :class "yes"
                         :type "submit"
                         :value "1"
                         :name "confirm-location"
                         "Create")))))))))

(defun get-event (id)
  (setf id (parse-integer id))
  (aif (db id)
    (require-user
      (standard-page
        "Event"
        (html
          (str (event-activity-item (make-result :id id
                                                 :time (getf it :local-time)
                                                 :people (getf it :hosts )))))
        :selected "events"))
    (not-found)))

(defun post-event (&optional id)
  (require-active-user
    (let* ((id (when id (parse-integer id)))
           (item (db id))
           (groupid (or (post-parameter-integer "identity-selection")
                        (post-parameter-integer "groupid")))
           (new-event-admin-p (unless id (group-admin-p groupid)))
           (new-host (or new-event-admin-p *userid*))
           (hosts (getf item :hosts))
           (group-adminp (when id
                           (loop for host in hosts
                                 thereis (group-admin-p host))))
           (old-location (getf item :location))
           (url (when id (strcat "/events/" id))))

       (cond
        ((post-parameter "love")
          (love id)
          (see-other (or (post-parameter "next") (referer))))

        ((post-parameter "unlove")
         (unlove id)
         (see-other (or (post-parameter "next") (referer))))

        (t
         (require-test ((or (member *userid* hosts)
                           group-adminp
                           (getf *user* :admin))
                       (s+ "You can only edit your own events."))

          (multiple-value-bind (old-time date-name old-date)
            (aif (getf item :local-time)
              (humanize-exact-time it)
              (values nil nil nil))
            (declare (ignore date-name))

            (let* ((new-date-p (scan +date-scanner+ (post-parameter "date")))
                   (new-time-p (scan +time-scanner+ (post-parameter "time")))
                   (date (or (when new-date-p (post-parameter "date"))
                             old-date))
                   (time (or (when new-time-p (post-parameter "time"))
                             old-time))
                   (local-time (when date
                                 (handler-case (parse-datetime date time)
                                   (local-time::invalid-time-specification ()
                                     nil))))
                   (local-day-of-week (when date
                                        (humanize-exact-time local-time
                                                             :weekday t)))
                   (end-date-string (post-parameter-string "end-date"))
                   (local-end-time (or (awhen end-date-string
                                         (handler-case
                                           (parse-datetime it time)
                                           (local-time::invalid-time-specification () nil)))
                                       (getf item :end-date)))
                   (title (or (post-parameter-string "title")
                              (getf item :title)))
                   (details (or (post-parameter-string "details")
                                (getf item :details)))
                   (location (or (post-parameter-string "location")
                                 old-location))
                   (lat (post-parameter-float "lat"))
                   (long (post-parameter-float "long"))
                   (recurring (or (when (post-parameter "recurring") t)
                                  (getf item :recurring)))
                   (frequency (or (post-parameter-string "frequency")
                                  (awhen (getf item :frequency)
                                    (symbol-name it))))
                   (symbol-frequency (awhen frequency
                                       (k-symbol
                                         (or-string= it '("weekly" "monthly")))))
                   (interval (or (post-parameter-integer "interval")
                                 (getf item :interval)))
                   (days-of-week (when (string= frequency "weekly")
                                   (or (post-parameter-string-list
                                          "days-of-week"
                                          #'(lambda (day)
                                              (find day +day-names+
                                                    :test #'equalp)))
                                       (awhen (getf item :days-of-week)
                                         (mapcar #'symbol-name it))
                                       (list local-day-of-week))))
                   (symbol-days-of-week (awhen days-of-week
                                          (mapcar #'k-symbol it)))
                   (by-day-or-date (when (string= frequency "monthly")
                                     (or (post-parameter-string
                                           "by-day-or-date")
                                         (awhen (getf item :by-day-or-date)
                                           (symbol-name it)))))
                   (symbol-day-or-date (awhen by-day-or-date
                                         (k-symbol (or-string= it
                                                   '("day" "date")))))
                   (days-of-month (when (string= frequency "monthly")
                                    (or (post-parameter-string-list
                                          "days-of-month"
                                          #'(lambda (day)
                                              (find day
                                                    +positions-of-day-in-month+
                                                    :test #'string=)))
                                        (awhen (getf item :days-of-month)
                                          (mapcar #'symbol-name it)))))
                   (symbol-days-of-month (awhen days-of-month
                                           (mapcar #'k-symbol it))))

              (labels ((try-again (&optional e)
               ;; needs to be labels not flet because of
               ;; submit-data's parameter precedence
                         (enter-event-details :title title
                                              :existing-url url
                                              :details details
                                              :location location
                                              :groupid new-event-admin-p
                                              :recurring recurring
                                              :frequency frequency
                                              :interval interval
                                              :days-of-week days-of-week
                                              :by-day-or-date by-day-or-date
                                              :days-of-month days-of-month
                                              :local-day-of-week local-day-of-week
                                              :end-date (awhen local-end-time(humanize-exact-time it))
                                              :date date
                                              :time time
                                              :error e))

                       (submit-data (&key lat long address)
                         (cond
                           (id
                            (modify-event id :lat lat
                                             :long long
                                             :address address
                                             :local-time local-time
                                             :recurring recurring
                                             :frequency symbol-frequency
                                             :interval interval
                                             :days-of-week symbol-days-of-week
                                             :by-day-or-date symbol-day-or-date
                                             :days-of-month symbol-days-of-month
                                             :local-end-date local-end-time
                                             :details details
                                             :title title)
                            (flash "Your event has been updated")
                            (see-other url))
                           (t
                            (flash "Your event has been added to the calendar")
                            (see-other
                              (strcat "/events/"
                                      (create-event
                                        :lat lat
                                        :host new-host
                                        :long long
                                        :local-time local-time
                                        :title title
                                        :details details
                                        :address location
                                        :recurring recurring
                                        :frequency symbol-frequency
                                        :interval interval
                                        :days-of-week symbol-days-of-week
                                        :by-day-or-date symbol-day-or-date
                                        :days-of-month symbol-days-of-month
                                        :local-end-date local-end-time)))))))
                (cond
                 ((post-parameter "cancel")
                  (see-other (or (script-name*) "/home")))

                 ((and (string= (post-parameter "frequency") "weekly")
                       (post-parameter "interval")
                       (> (parse-integer (post-parameter "interval")) 1)
                       (not (= (length days-of-week) 1)))
                  (try-again "You can only select multiple days of the week if your event repeats every week."))

                 ((or (post-parameter "edit")
                      (post-parameter "reset-location"))
                  (try-again nil))

                 ((post-parameter "delete")
                  (confirm-delete :url (script-name*)
                                  :type "event"
                                  :text (getf item :title)
                                  :next-url (referer)))

                 ((post-parameter "really-delete")
                  (delete-inventory-item id)
                  (flash "Your event has been deleted!")
                  (see-other (or (post-parameter "next") "/home")))

                 ((< (length title) 4)
                  (try-again "Please enter a longer title for your event"))

                 ((< (length details) 8)
                  (try-again "Please enter some more details for your event"))

                 ((not new-date-p)
                  (try-again "Dates must be in the form of mm/dd/yyyy (e.g. 12/30/2013)"))

                 ((not new-time-p)
                  (try-again "Please enter a valid starting time for your event (e.g. 2:30 pm)"))

                 ((not local-time)
                  (try-again "Please enter a valid date and time"))

                 ((not location)
                  (try-again "Please add a location for your event"))

                 ((< local-time (- (get-universal-time) +day-in-seconds+))
                  ; local-time for any given time zone
                  ; must not be earlier than today.
                  (try-again "Please enter a future date for your event"))

                 ((and (not (and lat long))
                       (not (equalp location old-location)))
                  ; if there's a new location
                  (multiple-value-bind (latitude longitude address)
                    (geocode-address location)
                    (if (equalp address old-location)
                      ; if the geocoded new location is the same as
                      ; the old address
                      (submit-data)
                      (verify-location url
                                       "Please verify the location of your event."
                                       latitude
                                       longitude
                                       "location" address
                                       "title" title
                                       "details" details
                                       "date" date
                                       "time" time
                                       "groupid" new-event-admin-p
                                       "recurring" recurring
                                       "frequency" frequency
                                       "interval" interval
                                       "days-of-week" days-of-week
                                       "by-day-or-date" by-day-or-date
                                       "days-of-month" days-of-month
                                       "end-date" (awhen local-end-time (humanize-exact-time it))))))

                 ((post-parameter "submit-edits")
                  (submit-data))

                 ((post-parameter "confirm-location")
                  (submit-data :lat lat :long long :address location))

                 (t (try-again))))))))))))

(defun get-events-all ()
  (standard-page
    "Events"
      (html
        (:div :class "activity event"
          (str
            (menu-horiz "actions"
                        (html (:a :href "/events/new" "+add an event"))))
          (str (distance-selection-html "/events"
                                        :text "show events within "
                                        :class "item"))
          (let* ((page (if (scan +number-scanner+ (get-parameter "p"))
                         (parse-integer (get-parameter "p"))
                         0))
                 (events (local-upcoming-events :page page)))
            (str events)
            (if (string= events "")
              (htm
                (:p :class "small"
                 "There are not any events posted in your area at this time. "
                 (unless (= (user-distance) 0)
                   (htm "To see more events, increase the \"show events within\" distance."))))))))
     :right (events-rightbar)
     :selected "events"))
