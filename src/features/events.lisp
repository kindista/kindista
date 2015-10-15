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

(defun activate-incorrectly-inactive-events
  (&aux (events '(17726 17711 17477 17290 16769)))
"To fix a bug introduced in commit f24b715bd4bb893f8bc6fc037910ee677b3e59dd which required events to be marked active, but didn't do so for new events"
  (dolist (id events) (modify-db id :active t)))

(defun find-incorrectly-inactive-events
  (&aux (time (parse-datetime "08/04/2014"))
        (events))
"To find events to add to activate-incorrectly-inactive-events from the last day or so"
  (dolist (id (hash-table-keys *db*))
    (let* ((data (db id))
           (type (getf data :type))
           (created (getf data :created)))
      (when (and (eq type :event)
                 (> created time))
        (push id events))))
  (sort events #'>))

(defun create-event (&key (host *userid*) lat long title details privacy local-time address recurring frequency interval days-of-week by-day-or-date weeks-of-month local-end-date custom-url)
  (insert-db (remove-nil-plist-pairs (list :type :event
                                           :hosts (list host)
                                           :lat lat
                                           :long long
                                           :address address
                                           :title title
                                           :details details
                                           :privacy privacy
                                           :local-time local-time
                                           :recurring recurring
                                           :frequency frequency
                                           :interval interval
                                           :days-of-week days-of-week
                                           :by-day-or-date by-day-or-date
                                           :weeks-of-month weeks-of-month
                                           :local-end-date local-end-date
                                           :custom-url custom-url
                                           :active t
                                           :created (get-universal-time)))))

(defmacro ensuring-eventid ((event-id base-url) &body body)
  (let ((is-number (gensym))
        (custom-url (gensym))
        (event-data (gensym)))
    `(let ((,is-number (scan +number-scanner+ ,event-id)))
       (if ,is-number
         (let* ((,event-id (parse-integer ,event-id))
                (,event-data (db ,event-id))
                (,custom-url (getf ,event-data :custom-url)))
           (cond
            ((not ,event-data)
             (not-found))

            (,custom-url
             (see-other (apply #'url-compose
                               (format nil ,base-url ,custom-url)
                               (flatten (get-parameters*)))))
            (t (progn ,@body))))
         (let ((,event-id (result-id (gethash ,event-id *eventname-index*))))
           (if ,event-id
             (progn ,@body)
             (not-found)))))))

(defun eventname-or-id (id &aux (event (db id)))
  (or (getf event :custom-url)
      (write-to-string id)))

(defun stale-eventp (item &optional time)
  (let ((staleness (- (or (result-time item) 0)
                      (- (or time (get-universal-time)) +day-in-seconds+))))
    (when (< staleness 0) t)))

(defun event-index-insert (result)
  (with-mutex (*event-mutex*)
    (asetf *event-index* (safe-sort (push result it) #'< :key #'result-time))))

(defun event-index-update (result)
  (with-mutex (*event-mutex*)
    (asetf *event-index* (safe-sort (pushnew result it) #'< :key #'result-time))))

(defun index-event (id data)
  (let* ((by (getf data :hosts))
         (result (make-result :latitude (getf data :lat)
                              :longitude (getf data :long)
                              :id id
                              :type :event
                              :people by
                              :privacy (getf data :privacy)
                              :time (or (getf data :auto-updated-time)
                                        (getf data :local-time)))))

    (with-locked-hash-table (*db-results*)
      (setf (gethash id *db-results*) result))

    (dolist (by-id by)
       (with-locked-hash-table (*profile-activity-index*)
         (asetf (gethash by-id *profile-activity-index*)
                (safe-sort (push result it) #'> :key #'result-time))))

    (cond
     ((stale-eventp result); when it needs an :auto-updated-time
      (when (getf data :recurring)
        (update-recurring-event-time id data result)))
     ((getf data :active)
       (event-index-insert result)
       (geo-index-insert *event-geo-index* result)))

    (awhen (getf data :custom-url)
      (with-locked-hash-table (*eventname-index*)
        (setf (gethash it *eventname-index*) result)))

    (when (getf data :active)
      (let ((stems (stem-text (s+ (getf data :title) " " (getf data :details)))))
        (with-locked-hash-table (*event-stem-index*)
          (dolist (stem stems)
            (push result (gethash stem *event-stem-index*))))))))

(defun modify-event (id &key lat long title custom-url details privacy local-time address recurring frequency interval days-of-week by-day-or-date weeks-of-month local-end-date)
  (bind-db-parameters
    (event id
     (address title details custom-url privacy lat long recurring frequency interval days-of-week by-day-or-date weeks-of-month end-date auto-updated-time local-time)
     old
     result)
    (let ((auto-updated-time)
          (now (get-universal-time))
          (new-data (or (not (eq recurring old-recurring))
                        (not (equal frequency old-frequency))
                        (not (equal interval old-interval))
                        (not (equal days-of-week old-days-of-week))
                        (not (equal by-day-or-date old-by-day-or-date))
                        (not (equal weeks-of-month old-weeks-of-month)))))

      (when (or (and lat (not (eql lat old-lat)))
                (and long (not (eql long old-long))))
        (geo-index-remove *event-geo-index* result)
        (setf (result-latitude result) lat)
        (setf (result-longitude result) long)
        (geo-index-insert *event-geo-index* result)
        (setf new-data t))

      (when (or (and title (not (equalp title old-title)))
                (and details (not (equalp details old-details))))
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
                (push result (gethash stem *event-stem-index*)))))
        (setf new-data t))

      (when (not (eql local-time old-local-time))
        (setf (result-time result) local-time)
        (setf auto-updated-time nil)
        (event-index-update result)
        (setf new-data t))

      (when (not (equalp custom-url old-custom-url))
        (with-locked-hash-table (*eventname-index*)
          (remhash old-custom-url *eventname-index*)
          (setf (gethash custom-url *eventname-index*) result)))

      (unless (eql old-privacy privacy)
        (setf (result-privacy result) privacy)
        (setf new-data t))

      (when new-data
        (modify-db id :title (or title old-title)
                      :details (or details old-details)
                      :custom-url (or custom-url old-custom-url)
                      :lat (or lat old-lat)
                      :long (or long old-long)
                      :address (or address old-address)
                      :privacy (or privacy old-privacy)
                      :local-time (or local-time old-local-time)
                      :recurring (or recurring old-recurring)
                      :frequency (or frequency old-frequency)
                      :interval (or interval old-interval)
                      :days-of-week (or days-of-week old-days-of-week)
                      :by-day-or-date (or by-day-or-date old-by-day-or-date)
                      :weeks-of-month (or weeks-of-month old-weeks-of-month)
                      :local-end-date (or local-end-date old-end-date)
                      :auto-updated-time (or auto-updated-time old-auto-updated-time)
                      :edited (when (find *userid* (getf event :hosts)) now))))))

(defun next-recurring-event-time (id &key data (time (get-universal-time)) prior-time)
  (let* ((event (or data (db id)))
         (frequency (getf event :frequency))
         (weekly (eql frequency 'weekly))
         (by-date (eq (getf event :by-day-or-date) 'date))
         (interval (getf event :interval))
         ;local-time can't offset by the week for 'monthly
         (offset-count (if (or (and weekly (eql interval 1))
                               by-date)
                         interval
                         (* interval 7)))
         (days-of-week (getf event :days-of-week))
         (weeks-of-month (getf event :weeks-of-month))
         (prior-time (or prior-time
                       (getf event :auto-updated-time)
                       (getf event :local-time)))
         (prior-timestamp (universal-to-timestamp prior-time))
         (timestamp (universal-to-timestamp time))
         (end-timestamp (awhen (getf event :local-end-date)
                          (universal-to-timestamp it)))
         (new-timestamp)
         (new-day-of-week))

    (labels ((update-timestamp (offset-period)
               (asetf new-timestamp
                      (adjust-timestamp (or it prior-timestamp)
                        (offset offset-period offset-count)))
               (cond
                 ((and end-timestamp
                       (local-time:timestamp> new-timestamp end-timestamp))
                  (setf new-timestamp nil))
                 ((local-time:timestamp<= new-timestamp timestamp)
                  (update-timestamp offset-period))))

             (update-timestamp-until-day ()
               (update-timestamp :day)
               (when new-timestamp
                 (setf new-day-of-week
                       (k-symbol (humanize-exact-time
                                   (timestamp-to-universal new-timestamp)
                                   :weekday t))) ;day name
                 (unless (find new-day-of-week days-of-week)
                   (update-timestamp-until-day))))

             (update-timestamp-until-week ()
               (update-timestamp :day)
               (when (and new-timestamp
                          weeks-of-month
                          (car weeks-of-month)) ;prevent comparing an empty list
                 (let ((day (timestamp-day new-timestamp))
                       (days-in-month (local-time:days-in-month
                                        (timestamp-month new-timestamp)
                                        (timestamp-year new-timestamp)))
                       (test-weeks-of-month))
                   (cond
                     ((< day 8) (push 'first test-weeks-of-month))
                     ((< day 15) (push 'second test-weeks-of-month))
                     ((< day 22) (push 'third test-weeks-of-month))
                     (t (when (< day 29)
                          (push 'fourth test-weeks-of-month))
                        (when (>= day (- days-in-month 7))
                          (push 'last test-weeks-of-month))))
                   (unless (intersection weeks-of-month test-weeks-of-month)
                     (update-timestamp-until-week))))))

      (if (or (and (> interval 1) weekly)
              by-date)
        (update-timestamp (if weekly :day :month)) ;local-time can't offset weekly
        (if weekly
          (update-timestamp-until-day)
          (update-timestamp-until-week))))

    (awhen new-timestamp (timestamp-to-universal it))))

(defun update-recurring-event-time (id &optional data result-struct)
  (let ((result (or result-struct (gethash id *db-results*))))
    (awhen (next-recurring-event-time id :data data)
      (modify-db id :auto-updated-time it)
      (with-locked-hash-table (*db-results*)
        (setf (result-time result) it))
      (event-index-update result))))

(defun tentative-recurring-event-schedule (date weeklyp interval end-date days-of-week by-day-or-date local-day-of-week weeks-of-month)
  (let ((pluralize-frequency (if (> interval 1)
                               (strcat interval
                                       " "
                                       (if weeklyp "week" "month") "s on ")
                               (unless weeklyp "month on "))))
    (s+ "Every "
        pluralize-frequency
        (if weeklyp
          (format nil *english-list* days-of-week)
          (if (equalp by-day-or-date "date")
            (s+ "the " (humanize-number (day-of-month date :formatted-date t)) " day")
            (s+ "the "
                (format nil *english-list* (mapcar #'string-downcase weeks-of-month))
                " "
                local-day-of-week)))
        (awhen end-date
          (s+ " until " it)))))

(defun recurring-event-schedule (id &optional data end)
  (let* ((event (or data (db id)))
         (weekly (eql (getf event :frequency) 'weekly))
         (interval (getf event :interval))
         (end-date (getf event :local-end-date))
         (pluralize-frequency (if (> interval 1)
                                (strcat interval
                                        " "
                                        (if weekly "week" "month") "s on ")
                                (unless weekly "month on ")))
         (time (or (getf event :auto-updated-time)
                   (getf event :local-time))))
    (flet ((lowercase (words)
             (mapcar #'(lambda (word)
                         (string-downcase (symbol-name word)))
                     words)))
      (s+ "Every "
          pluralize-frequency
          (if weekly
            (format nil *english-list*
                        (mapcar #'string-capitalize
                                (lowercase (getf event :days-of-week))))
            (if (eql (getf event :by-day-or-date) 'date)
              (s+ "the "
                  (humanize-number (day-of-month time))
                  " day")
              (s+ "the "
                  (format nil *english-list*
                              (lowercase (getf event :weeks-of-month)))
                  " "
                  (string-capitalize (humanize-exact-time time :weekday t)))))
          (when (and end end-date)
            (s+ " until "
                (caddr (multiple-value-list (humanize-exact-time end-date)))))))))

(defun upcoming-events-html
  (items &key (page 0) (count 20) paginate (url "/events") (location t) (sidebar nil))
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
                 (multiple-value-bind (time date short-date)
                     (humanize-exact-time item-time)
                   (unless (string= date calendar-date)
                     (setf calendar-date date)
                     (htm (:h3 :class "event-date" (str calendar-date))))

                   (htm (str (event-activity-item item
                                                  :sidebar sidebar
                                                  :time time
                                                  :date short-date
                                                  :truncate t
                                                  :show-distance location)))))
               (setf items (cdr items)))

              (t
               (when (and sidebar
                          (> (length items) count))
                 (htm (:a :href "/events" "see more events")))

               (when (and (< (user-distance) 100)
                          (> (user-distance) 0))
                 (htm
                   (:div :class "item small"
                    (unless items
                      (htm (:em "There are no events scheduled on your local calendar at this time. ")))
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

(defun populate-calendar (items)
  (let (event-list)
    (flet ((add-event-occurance (result)
             (asetf event-list (stable-sort (copy-list (push result it)) #'< :key #'result-time))))
     (dolist (item items)
       (let* ((id (result-id item))
              (event (db id)))
         (if (getf event :recurring)
           (let ((event-repetition-count 1)
                 (next-occurance (next-recurring-event-time id :data event)) )
             (labels ((future-events ()
                        (when next-occurance
                          (add-event-occurance
                            (make-result :latitude (result-latitude item)
                                         :longitude (result-longitude item)
                                         :time next-occurance
                                         :tags (result-tags item)
                                         :people (result-people item)
                                         :id id
                                         :type 'event
                                         :privacy (result-privacy item))))
                        (incf event-repetition-count)
                        (asetf next-occurance
                               (next-recurring-event-time id :data event
                                                             :prior-time it))
                        (when (and (< event-repetition-count 7) next-occurance)
                          (future-events))))
               (add-event-occurance item)
               (future-events)))

           (add-event-occurance item)))))

    event-list))

(defun local-upcoming-events ()
  (with-location
    (let* ((distance (user-distance))
           (now (get-universal-time))
           (global-search (= distance 0))
           (updated-local-event-list)
           (local-events (sort (geo-index-query *event-geo-index*
                                                *latitude*
                                                *longitude*
                                                distance)
                                #'< :key #'result-time))
           (featured-events)
           (featured-local-events))

      (flet ((trim-and-update (results)
               ;; loop through results starting at oldest
               ;; update recurring events and trim old ones
               (do* ((events results (cdr results))
                     (event (car events)))
                    ((or (not events) (not (stale-eventp event now)))
                     (acond
                      (updated-local-event-list
                       (sort (remove nil (append it results) :from-end t :count 1)
                             #'< :key #'result-time))
                      (global-search *event-index*)
                      (t results)))
                    (let* ((id (result-id event))
                           (data (db id))
                           (end-date (getf data :local-end-date)))
                      (cond
                       ((and (getf data :recurring)
                             (or (not end-date)
                                 (> end-date (- now +day-in-seconds+))))
                        (update-recurring-event-time id data event)
                        (unless global-search
                          (push event updated-local-event-list)))
                       (t
                        (geo-index-remove *event-geo-index* event)
                        (with-mutex (*event-mutex*)
                          (asetf *event-index* (remove event it)))))))))

        (setf local-events (populate-calendar (remove-private-items
                                                (trim-and-update
                                                  (if global-search
                                                    *event-index*
                                                    local-events)))))
        (setf featured-events (remove-if (lambda (time)
                                           (< time now))
                                         (hash-table-values *eventname-index*)
                                         :key #'result-time))
        (setf featured-local-events
              (intersection local-events featured-events))

        (values local-events featured-local-events)))))

(defun events-rightbar ()
  (html
    (str (donate-sidebar))
    (str (invite-sidebar))))

(defun get-events-new ()
  (require-active-user
    (if (getf *user* :pending)
      (progn
        (pending-flash "post events on Kindista")
        (see-other (or (referer) "/home")))
      (enter-event-details))))

(defun enter-monthly-recurring-details
  (date frequent by-day-or-date local-day-of-week weeks-of-month)
  (let* ((day-of-month (day-of-month date :formatted-date t))
         (by-date (equalp by-day-or-date "date"))
         (nth-week-in-month (position-of-day-in-month date :formatted-date t))
         (days-in-month (days-in-month date)))

    (flet ((day-or-date-selector (&optional reload)
             (html
               (:fieldset
                 (:legend "Repeat by")
                   (:label
                      (:input :type "radio"
                              :name "by-day-or-date"
                              :value "day"
                              :onclick (when reload "this.form.submit()")
                              :checked (unless by-date ""))
                     (str
                       (if frequent
                         "the day of the week"
                         ;(s+ local-day-of-week "s")
                         (strcat "the " nth-week-in-month " " local-day-of-week " of the month" ))))
                 (unless frequent (htm (:br)))
                 (:label
                    (:input :type "radio"
                     :name "by-day-or-date"
                     :value "date"
                     :onclick (when reload "this.form.submit()")
                     :checked (when by-date ""))
                   (unless frequent "date (")
                   "the "
                   (str (humanize-number day-of-month))
                   " day of the month"
                   (unless frequent ")"))))))
      (html
        (:div
          (cond

           (frequent
             (str (day-or-date-selector t))
             (unless by-date
               (htm
                 (:fieldset
                   (:legend "Repeat on")
                   (dolist (option +positions-of-day-in-month+)
                      (htm
                        (:label
                          (:input :type "checkbox"
                                  :name "weeks-of-month"
                                  :checked (when (aif weeks-of-month
                                                   (find option it
                                                         :test #'equalp)
                                                   (equalp option
                                                           nth-week-in-month))
                                             "checked")
                                  :value option)
                            (str option) " "
                            (str local-day-of-week))))))))

           ((and (> day-of-month 21)
                 (>= day-of-month (- days-in-month 7)))
            ;;for last week of 28day february's
            (htm
              (:fieldset
                (:legend "Repeat on")
                (:label
                  (:input :type "radio"
                          :name "position-in-month"
                          :value "date"
                          :checked (when by-date ""))
                  "the "
                  (str (humanize-number day-of-month))
                  " day of the month")
                (:label
                  (:input :type "radio"
                          :name "position-in-month"
                          :value "fourth"
                          :checked (unless (or by-date
                                               (equal weeks-of-month '("last")))
                                     ""))
                   (str (s+ "the fourth " local-day-of-week " of the month")))
                (:label
                  (:input :type "radio"
                          :name "position-in-month"
                          :value "last"
                          :checked (when (and (not by-date)
                                              (equal weeks-of-month '("last")))
                                     ""))
                   (str (s+ "the last " local-day-of-week " of the month"))))))

           (t ; potential-long-month not-frequent
            (htm
             (str (day-or-date-selector)) ))))))))

(defun enter-event-details
  (&key error date time location title restrictedp (identity-selection *userid*) groups-selected details existing-url custom-url recurring frequency interval days-of-week by-day-or-date weeks-of-month end-date local-day-of-week editing-schedule
   &aux (groups-with-user-as-admin (groups-with-user-as-admin)))
  (standard-page
    (if existing-url "Edit your event details" "Create a new event")
    (html
      (when error (flash error :error t))
      (:div :class "item create-event"
        (:h2 (str (if existing-url "Edit your event details"
                                   "Create a new event")))
        (:form :method "post"
               :action (or existing-url "/events/new")
         (:input :type "hidden" :name "next" :value (referer))
         (when (and existing-url groups-selected)
           (dolist (group groups-selected)
             (htm (:input :type "hidden" :name "groups-selected" :value group))))
         (:input :type "hidden" :name "prior-identity" :value identity-selection)

         (:fieldset :id "date-and-time"
           (:legend "When (date & time)")
             (if editing-schedule
               (htm (:strong (str (s+ date " at " time)))
                    (:button :class "green simple-link"
                             :type "submit"
                             :name "edit-datetime"
                             "edit")
                    (:input :type "hidden" :name "date" :value date)
                    (:input :type "hidden" :name "time" :value time))

               (htm
                 (:label :for "date" "Date")
                 (:input :type "text"
                         :id "date"
                         :name "date" :placeholder "mm/dd/yyyy"
                         :value date)
                 (:label :for "time" "Time")
                 (:input :type "text"
                         :name "time"
                         :id "time"
                         :placeholder "Add a time? (ex. 2:30 PM)"
                         :value time))))

         (if editing-schedule
           (htm (:input :type :hidden :name "recurring" :value recurring))
           (htm (:div (:input :type "checkbox"
                              :name "recurring"
                              :checked (when recurring "checked")
                              :onclick "this.form.submit()"
                              "Repeat..."))))

         (when recurring
           (let ((frequent (or (not interval) (= interval 1)))
                 (monthly (equalp frequency "monthly"))
                 (local-day-of-week (string-capitalize local-day-of-week)))
             (if (not editing-schedule)
               (htm (:h2 "Event Schedule")
                    (:strong (str (tentative-recurring-event-schedule date
                                                                      (not monthly)
                                                                      interval
                                                                      end-date
                                                                      days-of-week
                                                                      by-day-or-date
                                                                      local-day-of-week
                                                                      weeks-of-month)))

                    (:input :type "hidden" :name "frequency" :value frequency)
                    (:input :type "hidden" :name "interval" :value interval)
                    (:input :type "hidden" :name "by-day-or-date" :value by-day-or-date)
                    (:input :type "hidden" :name "days-of-week" :value days-of-week)
                    (:input :type "hidden" :name "weeks-of-month" :value weeks-of-month)
                    (:input :type "hidden" :name "end-date" :value end-date)
                    (:button :class "green simple-link"
                             :type "submit"
                             :name "recurring"
                             "edit"))

               (htm
                 (:div :class "recurring-event-details"
                  (:fieldset :id "repeat-every"
                    (:legend "Repeat "
                              (when (and local-day-of-week
                                         (nor monthly frequent))
                                (htm " on " (str local-day-of-week)))
                              " every")
                    (:label :for "interval" "Interval")
                    (str (number-selection-html "interval" 12
                                                 :id "interval"
                                                 :selected (or interval 1)
                                                 :auto-submit t))

                   (:label :for "frequency" "Frequency")
                   (:select :onchange "this.form.submit()"
                            :id "frequency"
                            :name "frequency"
                     (:option :value "weekly"
                              :selected (unless monthly "")
                              "Week" (unless frequent (htm "s")))
                     (:option :value "monthly"
                              :selected (when monthly "")
                              "Month" (unless frequent (htm "s")))))

                    (when (and (or (not frequency) (not monthly))
                               frequent)
                      (htm
                        (:fieldset
                          (:legend "Repeat on")
                            (dolist (day +day-names+)
                              (htm
                                (:label
                                  (:input :type "checkbox"
                                          :name "days-of-week"
                                          :checked (when (or (find day
                                                                   days-of-week
                                                                   :test #'equalp)
                                                             (equalp local-day-of-week
                                                                     day))
                                                     "checked")
                                          :value (string-downcase day))
                                  (str (elt day 0))))))))

                    (when (and date monthly)
                      (str (enter-monthly-recurring-details date
                                                            frequent
                                                            by-day-or-date
                                                            local-day-of-week
                                                            weeks-of-month)))

                    (:div
                      (:label :for "end-date"
                       "End Date (optional)")
                      (:input :type "text"
                              :id "end-date"
                              :name "end-date"
                              :placeholder "mm/dd/yyyy"
                              :value end-date)))))))

         (:div :class "long"
           (:label :for "title" "Event title")
           (:input :type "text"
                   :id "title"
                   :name "title"
                   :placeholder "ex: Community Garden Work Party"
                   :value (awhen title (escape-for-html it))))

         (when (getf *user* :admin)
           (htm
             (:div :class "long"
               (:label :for "custom-url" "Custom Url (Optional. For major events only.)")
               (:input :type "text"
                       :id "custom-url"
                       :name "custom-url"
                       :placeholder "ex: 2015-Eugene-Holiday-Free-Market"
                       :value (awhen custom-url (escape-for-html it))))))

         (when (and (not existing-url) groups-with-user-as-admin)
           (htm (:label :for "identity-selection" "Posted by")
                (str (identity-selection-html identity-selection
                                              groups-with-user-as-admin
                                              :class "identity event-host"
                                              :onchange "this.form.submit()"))))
         (when (or (getf *user-group-privileges* :member)
                   (getf *user-group-privileges* :admin))
           (str (privacy-selection-html
                  "event"
                  restrictedp
                  (if (and identity-selection
                           (not (equal identity-selection *userid*)))
                    (list (cons identity-selection (db identity-selection :name)))
                    (append (groups-with-user-as-member)
                            (groups-with-user-as-admin)))
                  groups-selected
                  :onchange "this.form.submit()")))
         (:label :for "details" "Details")
         (:textarea :rows "8"
                    :id "details"
                    :name "details"
                    :placeholder "Add some more info..."
                    (awhen details (str (escape-for-html it))))

         (:div :class "long"
           (:label :for "location" "Where")
           (:input :type "text"
                   :id "location"
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
                         "Create"))))))))

(defun get-event (id)
  (ensuring-eventid (id "/events/~a")
    (if (and (not (find *userid* (remove nil ;just in case
                                         (db id :hosts))))
             (item-view-denied (result-privacy (gethash id *db-results*))))
      (permission-denied)
      (standard-page
        "Event"
        (html
          (str (event-activity-item (gethash id *db-results*))))
        :selected "events"))))

(defun post-event (&optional id)
  (require-active-user
    (let* ((id (when id (parse-integer id)))
           (item (db id))
           (hosts (getf item :hosts))
           (group-adminp (when id
                           (loop for host in hosts
                                 thereis (group-admin-p host))))
           (old-location (getf item :address))
           (url (when id (strcat "/events/" id))))

       (cond
        ((post-parameter "love")
          (love id)
          (see-other (or (post-parameter "next") (referer))))

        ((post-parameter "unlove")
         (unlove id)
         (see-other (or (post-parameter "next") (referer))))

        (t
         (require-test ((or (not id)
                            (member *userid* hosts)
                            group-adminp
                            (getf *user* :admin))
                       (s+ "You can only edit your own events."))

           (let* ((result (gethash id *db-results*))
                  (old-datetime (when item
                                  (multiple-value-list
                                    (humanize-exact-time
                                      (or (getf item :auto-updated-time)
                                          (getf item :local-time))))))
                  (new-date-p (scan +date-scanner+ (post-parameter "date")))
                  (new-time-p (scan +time-scanner+ (post-parameter "time")))
                  (test-date (or (when new-date-p (post-parameter "date"))
                                 (third old-datetime)))
                  (test-time (or (when new-time-p (post-parameter "time"))
                                 (first old-datetime)))
                  (local-time (awhen test-date
                                (handler-case (parse-datetime it test-time)
                                  (local-time::invalid-time-specification ()
                                    nil))))
                  (time (when local-time test-time))
                  (date (when local-time test-date))
                  (local-day-of-week (when date
                                       (humanize-exact-time local-time
                                                            :weekday t)))
                  (end-date-string (post-parameter-string "end-date"))
                  (local-end-time (or (awhen end-date-string
                                        (handler-case
                                          (parse-datetime it time)
                                          (local-time::invalid-time-specification () nil)))
                                      (getf item :local-end-date)))
                  (title (or (post-parameter-string "title")
                             (getf item :title)))
                  (custom-url (or (when (getf *user* :admin)
                                    (awhen (post-parameter-string "custom-url")
                                      (hyphenate it)))
                                  (getf item :custom-url)))
                  (details (or (post-parameter-string "details")
                               (getf item :details)))
                  (location (or (post-parameter-string "location")
                                old-location))
                  (new-lat (post-parameter-float "lat"))
                  (new-long (post-parameter-float "long"))
                  (lat (or new-lat (getf item :lat)))
                  (long (or new-long (getf item :long)))
                  (recurring (and (not (post-parameter "edit-datetime"))
                                  (or (post-parameter "recurring")
                                      (getf item :recurring))))
                  (frequency (or (post-parameter-string "frequency")
                                 (awhen (getf item :frequency)
                                   (symbol-name it))))
                  (symbol-frequency (awhen frequency
                                      (k-symbol
                                        (or-string= it '("weekly" "monthly")))))
                  (interval (or (post-parameter-integer "interval")
                                (getf item :interval)))
                  (days-of-week (when (equalp frequency "weekly")
                                  (if (and (= interval 1)
                                           (post-parameter "days-of-week"))
                                    (post-parameter-string-list
                                      "days-of-week"
                                      #'(lambda (day)
                                          (find day +day-names+
                                                :test #'equalp)))
                                    (or (awhen (getf item :days-of-week)
                                          (mapcar #'symbol-name it))
                                         (list local-day-of-week)))))
                  (symbol-days-of-week (awhen days-of-week
                                         (mapcar #'k-symbol it)))
                  (position-in-month
                    (post-parameter-string "position-in-month"))
                  (by-day-or-date (when (equalp frequency "monthly")
                                    (or (post-parameter-string
                                          "by-day-or-date")
                                        (when (equalp position-in-month
                                                      "date")
                                          "date")
                                        (awhen (getf item :by-day-or-date)
                                          (symbol-name it)))))
                  (symbol-day-or-date (awhen by-day-or-date
                                        (k-symbol (or-string= it
                                                  '("day" "date")))))
                  (weeks-of-month
                    (when (and (not (eql symbol-day-or-date 'date))
                               (equalp frequency "monthly"))
                      (or (post-parameter-string-list "weeks-of-month"
                             #'(lambda (day)
                                 (find day +positions-of-day-in-month+
                                       :test #'equalp)))
                          (when (find position-in-month
                                      +positions-of-day-in-month+
                                      :test #'equalp)
                            (list position-in-month))
                          (awhen (getf item :weeks-of-month)
                            (mapcar #'symbol-name it)))))
                  (symbol-weeks-of-month (awhen weeks-of-month
                                           (mapcar #'k-symbol it)))
                  (identity-selection (or (post-parameter-integer "identity-selection")
                                          (car (getf item :hosts))))
                  (new-host (if (group-admin-p identity-selection)
                              identity-selection
                              *userid*))
                  (prior-identity (post-parameter-integer "prior-identity"))
                  (privacy-selection (post-parameter-string "privacy-selection"))
                  (restrictedp (and (equalp privacy-selection "restricted")
                                    (or (not identity-selection)
                                        (eql identity-selection prior-identity))))
                  (groups-selected (or (post-parameter-integer-list "groups-selected")
                                       (getf item :privacy))))

             (labels ((submit-event (fn &key host id)
                        (let ((arguments (list :lat lat
                                               :long long
                                               :custom-url custom-url
                                               :local-time local-time
                                               :privacy groups-selected
                                               :title title
                                               :details details
                                               :address location
                                               :recurring recurring)))
                          (when (or recurring
                                    ;;reset parameters when :recurring t -> nil
                                    (getf item :recurring))
                            (asetf arguments
                                   (append
                                     (list :frequency symbol-frequency
                                           :interval interval
                                           :days-of-week symbol-days-of-week
                                           :by-day-or-date symbol-day-or-date
                                           :weeks-of-month symbol-weeks-of-month
                                           :local-end-date local-end-time)
                                    it)))
                          (when host
                            (asetf arguments (append (list :host host) it)))

                          (if id (apply fn id arguments)
                                 (apply fn arguments))))

                      (try-again (&optional e not-recurring)
              ;; needs to be labels not flet because of
              ;; submit-data's parameter precedence
                        (enter-event-details :title title
                                             :existing-url url
                                             :custom-url custom-url
                                             :details details
                                             :location location
                                             :recurring (unless not-recurring
                                                          recurring)
                                             :frequency frequency
                                             :interval interval
                                             :days-of-week days-of-week
                                             :by-day-or-date by-day-or-date
                                             :weeks-of-month weeks-of-month
                                             :local-day-of-week local-day-of-week
                                             :end-date (awhen local-end-time
                                                         (humanize-exact-time it))
                                             :date date
                                             :time time
                                             :restrictedp restrictedp
                                             :groups-selected groups-selected
                                             :identity-selection identity-selection
                                             :editing-schedule (and recurring
                                                                    (not not-recurring)
                                                                    (not (post-parameter
                                                                           "edit-datetime")))
                                             :error e))

                      (submit-data ()
                        (cond
                          (id
                            (submit-event #'modify-event :id id)
                            (flash "Your event has been updated")
                            (see-other url))
                          (t
                           (flash "Your event has been added to the calendar")
                           (see-other
                             (strcat "/events/"
                                     (submit-event #'create-event
                                                   :host new-host)))))))
               (cond
                ((post-parameter "cancel")
                 (see-other (or (script-name*) "/home")))

                ((and custom-url
                      (gethash custom-url *eventname-index*)
                      (not (eq (gethash custom-url *eventname-index*)
                               result)))
                 (try-again "This custom-url is already in use. Please use a different one."))


                ((and (string= (post-parameter "frequency") "weekly")
                      (post-parameter "interval")
                      (> (parse-integer (post-parameter "interval")) 1)
                      (not (= (length days-of-week) 1)))
                 (try-again "You can only select multiple days of the week if your event repeats every week."))

                ((or (post-parameter "edit")
                     (post-parameter "reset-location"))
                 (try-again))

                ((post-parameter "deactivate")
                 (confirm-delete :url (script-name*)
                                 :confirmation-question
                                   (s+ "Are you sure you want to deactivate this event?")
                                 :type "event"
                                 :text (getf item :title)
                                 :next-url (referer)))

                ((post-parameter "really-delete")
                 (deactivate-inventory-item id)
                 (flash "Your event has been deleted!")
                 (see-other (or (post-parameter "next") "/home")))

                ((and (post-parameter "date") (not new-date-p))
                 (try-again "Please enter your date with the format MM/DD/YYYY" t))

                ((and test-date (not date))
                 (try-again "Please enter a valid date." t))

                ((and recurring (not date)
                 (try-again "Please enter a date." t)))

                ((and recurring (not time)
                 (try-again "Please enter a time" t)))

                ((nor (post-parameter "confirm-location")
                      (post-parameter "submit-edits"))
                 (try-again))

                ((not new-date-p)
                 (try-again "Dates must be in the form of mm/dd/yyyy (e.g. 12/30/2013)"))

                ((not new-time-p)
                 (try-again "Please enter a valid starting time for your event (e.g. 2:30 pm)"))

                ((not local-time)
                 (try-again "Please enter a valid date and time"))

                ((< (length title) 4)
                 (try-again "Please enter a longer title for your event"))

                ((< (length details) 8)
                 (try-again "Please enter some more details for your event"))

                ((not location)
                 (try-again "Please add a location for your event"))

                ((< local-time (- (get-universal-time) +day-in-seconds+))
                 ; local-time for any given time zone
                 ; must not be earlier than today.
                 (try-again "Please enter a future date for your event"))

                ((or (not (and lat long))
                     (and (not (equalp location old-location))
                          (nor new-lat new-long)))
                 ; if there's a new location
                 (let* ((location-data (multiple-value-list (geocode-address location)))
                        (latitude (first location-data))
                        (longitude (second location-data))
                        (address (third location-data)))
                   (cond 
                     ((notevery #'identity (list latitude longitude))
                      (try-again "We are unable to understand the location you submitted. Please use an actual address. If your event doesn't have a specific address, please enter an approximate address and give more detailed directions in the details section."))

                     ((equalp address old-location)
                      ; if the geocoded new location is the same as
                      ; the old address
                      (submit-data))

                     (t
                      (verify-location url
                                       "Please verify the location of your event."
                                       latitude
                                       longitude
                                       "location" address
                                       "title" title
                                       "custom-url" custom-url
                                       "details" details
                                       "date" date
                                       "time" time
                                       "groups-selected" groups-selected
                                       "identity-selection" identity-selection
                                       "privacy-selection" privacy-selection
                                       "prior-identity" prior-identity
                                       "recurring" recurring
                                       "frequency" frequency
                                       "interval" interval
                                       "days-of-week" days-of-week
                                       "by-day-or-date" by-day-or-date
                                       "weeks-of-month" weeks-of-month
                                       "end-date" (awhen local-end-time
                                                    (humanize-exact-time it)))))))

                ((or (post-parameter "submit-edits")
                     (post-parameter "confirm-location"))
                 (submit-data))

                (t (try-again)))))))))))

(defun get-events-all
  (&aux (page (if (scan +number-scanner+ (get-parameter "p"))
                (parse-integer (get-parameter "p"))
                0)) )
  (standard-page
    "Events"
      (html
        (:div :class "activity event"
          (str
            (menu-horiz (html (:a :href "/events/new" "+add an event"))))
          (str (distance-selection-html "/events"
                                        :text "show events within "
                                        :class "item"))
          (multiple-value-bind (events featured-events)
            (local-upcoming-events)
            (when (and (= page 0)
                       featured-events)
              (htm
                (:div :class "featured events item"
                  (:div :class "featured header"
                    (:h3 (str (pluralize featured-events
                                        "Featured Event"
                                        :hidenum t))))
                 (dolist (fevent featured-events)
                   (str (event-activity-item fevent
                                             :featuredp t
                                             :truncate t))))))
            (str (upcoming-events-html
                   (if (> page 0)
                     events
                     (remove-if (lambda (result) (find result featured-events))
                                events))
                   :paginate t
                   :page page))
            (unless events
              (htm
                (:p :class "small"
                 "There are not any events posted in your area at this time. "
                 (unless (= (user-distance) 0)
                   (htm "To see more events, increase the \"show events within\" distance."))))))))
     :right (events-rightbar)
     :selected "events"))
