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

(defun create-event (&key (host *userid*) lat long title details privacy local-time address recurring frequency interval days-of-week by-day-or-date weeks-of-month local-end-date)
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
                                           :created (get-universal-time)))))

(defun stale-eventp (item &optional time)
  (let ((staleness (- (or (result-time item) 0)
                      (- (or time (get-universal-time)) +day-in-seconds+))))
    (when (< staleness 0) t)))

(defun event-index-insert (result)
  (with-mutex (*event-mutex*)
    (asetf *event-index* (sort (push result it) #'< :key #'result-time))))

(defun event-index-update (result)
  (with-mutex (*event-mutex*)
    (asetf *event-index* (sort (pushnew result it) #'< :key #'result-time))))

(defun index-event (id data)
  (let* ((by (getf data :hosts))
         (result (make-result :latitude (or (getf data :lat) (getf (db (getf data :by)) :lat))
                              :longitude (or (getf data :long) (getf (db (getf data :by)) :long))
                              :id id
                              :type :event
                              :people by
                              :privacy (getf data :privacy)
                              :time (or (getf data :auto-updated-time)
                                        (getf data :local-time)))))

    (with-locked-hash-table (*db-results*)
      (setf (gethash id *db-results*) result))

    (unless (stale-eventp result)
      ; remove past events from event-index
      ; add this event and sort the index
      (event-index-insert result)
      (geo-index-insert *event-geo-index* result))

    (let ((stems (stem-text (s+ (getf data :title) " " (getf data :details)))))
      (with-locked-hash-table (*event-stem-index*)
        (dolist (stem stems)
          (push result (gethash stem *event-stem-index*)))))))

(defun modify-event (id &key lat long title details privacy local-time address recurring frequency interval days-of-week by-day-or-date weeks-of-month local-end-date)
  (bind-db-parameters
    (event id
     (address title details privacy lat long recurring frequency interval days-of-week by-day-or-date weeks-of-month end-date auto-updated-time local-time)
     old
     result)
    (let ((auto-updated-time)
          (now (get-universal-time))
          (new-data (or (eq recurring old-recurring)
                        (equal frequency old-frequency)
                        (equal interval old-interval)
                        (equal days-of-week old-days-of-week)
                        (equal by-day-or-date old-by-day-or-date)
                        (equal weeks-of-month old-weeks-of-month))))

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

      (unless (eql old-privacy privacy)
        (setf (result-privacy result) privacy)
        (setf new-data t))

      (when new-data
        (modify-db id :title (or title old-title)
                      :details (or details old-details)
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

(defun populate-calendar (items &key (page 0) (count 20) paginate url (location t) (sidebar nil))
  (let (event-list)
    (flet ((add-event-occurance (result)
             (asetf event-list (sort (push result it) #'< :key #'result-time))))
     (dolist (item items)
       (let* ((id (result-id item))
              (event (db id)))
         (if (getf event :recurring)
           (let ((event-repetition-count 1)
                 (next-occurance (next-recurring-event-time id :data event)) )
             (labels ((future-events ()
                        (add-event-occurance
                          (make-result :latitude (result-latitude item)
                                       :longitude (result-longitude item)
                                       :time next-occurance
                                       :tags (result-tags item)
                                       :people (result-people item)
                                       :id id
                                       :type 'event
                                       :privacy (result-privacy item)))
                        (incf event-repetition-count)
                        (asetf next-occurance
                               (next-recurring-event-time id :data event
                                                             :prior-time it))
                        (when (and (< event-repetition-count 7) next-occurance)
                          (future-events))))
               (add-event-occurance item)
               (future-events)))

           (add-event-occurance item)))))

    (upcoming-events event-list :page page
                                :count count
                                :url url
                                :sidebar sidebar
                                :paginate paginate
                                :location location)))

(defun local-upcoming-events (&key (page 0) (count 20) (url "/events") (paginate t) (sidebar nil))
  (with-location
    (let* ((distance (user-distance))
           (now (get-universal-time))
           (global-search (= distance 0))
           (updated-local-event-list)
           (local-events (sort (geo-index-query *event-geo-index*
                                                *latitude*
                                                *longitude*
                                                distance)
                                #'< :key #'result-time)))

      (flet ((trim-and-update (results)
               (do* ((events results (cdr results))
                     (event (car events)))
                    ((not (stale-eventp event now))
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

        (populate-calendar (remove-private-items
                             (trim-and-update
                               (if global-search
                                 *event-index*
                                 local-events)))
                           :page page
                           :count count
                           :url url
                           :sidebar sidebar
                           :paginate paginate)))))

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

(defun enter-event-details (&key error date time location title restrictedp (identity-selection *userid*) groups-selected details existing-url recurring frequency interval days-of-week by-day-or-date weeks-of-month end-date local-day-of-week)
  (standard-page
    (if existing-url "Edit your event details" "Create a new event")
    (html
      (when error
        (flash error :error t))
       (:div :class "item create-event"
         (:h2 (str (if existing-url "Edit your event details"
                                    "Create a new event")))
         (:form :method "post"
                :action (or existing-url "/events/new")
          (:input :type "hidden" :name "next" :value (referer))
          (when (and existing-url groups-selected)
            (dolist (group groups-selected)
              (htm (:input :type "hidden" :name "groups-selected" :value group))))
          (:input :type "hidden"
                  :name "prior-identity"
                  :value identity-selection)
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
            (let ((frequent (or (not interval) (= interval 1))))
              (htm
                (:div :class "recurring-event-details"
                  (:div
                    (:label "Repeat Every")
                    (str (number-selection-html "interval" 12
                                                :selected (or interval 1)
                                                :auto-submit t)))

                  (:div
                    (:select :onchange "this.form.submit()"
                             :name "frequency"
                      (:option :value "weekly"
                               :selected (unless (equalp frequency "monthly")
                                           "")
                               "Week"
                               (unless frequent (htm "s")))
                      (:option :value "monthly"
                               :selected (when (equalp frequency "monthly") "")
                               "Month"
                               (unless frequent (htm "s")))))

                  (when (and (or (not frequency)
                                 (equalp frequency "weekly"))
                             frequent)
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

                  (when (and date (equalp frequency "monthly"))
                    (let ((by-date (equalp by-day-or-date "date"))
                          (local-day-of-week (string-capitalize local-day-of-week))
                          (day-of-month (day-of-month date :formatted-date t))
                          (nth-week-in-month (position-of-day-in-month
                                               date
                                               :formatted-date t)))
                      (htm
                        (:div
                          (:label "Repeat on")
                          (:div :class "inline-block"
                            (:input :type "radio"
                                    :name "by-day-or-date"
                                    :value "day"
                                    :onclick "this.form.submit()"
                                    :checked (unless by-date ""))
                            (if frequent
                              "day of the week"
                              (str
                                (strcat "the "
                                        nth-week-in-month
                                        " "
                                        local-day-of-week
                                        " of the month" ))))
                          (when (< day-of-month 29)
                            (htm
                              (:div :class "inline-block"
                                (:input :type "radio"
                                        :name "by-day-or-date"
                                        :value "date"
                                        :onclick "this.form.submit()"
                                        :checked (when by-date ""))
                                (unless frequent "date (")
                                "the "
                                (str (humanize-number day-of-month))
                                " day of the month"
                                (unless frequent ")")))))

                        (when (and (not by-date) frequent)
                          (htm
                            (:div
                              (:label "Days of the month")
                                (dolist (option +positions-of-day-in-month+)
                                  (htm
                                    (:div :class "inline-block"
                                      (:input :type "checkbox"
                                            :name "weeks-of-month"
                                            :checked (when
                                                       (aif weeks-of-month
                                                         (find option it
                                                               :test #'equalp)
                                                         (equalp
                                                           option
                                                           nth-week-in-month))
                                                       "checked")
                                            :value option)
                                      (str option) " " (str local-day-of-week))))))))))

                  (:div
                    (:label "End Date (optional)")
                    (:input :type "text"
                            :name "end-date"
                            :placeholder "mm/dd/yyyy"
                            :value end-date))))))

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
                  (str (identity-selection-html identity-selection
                                                it
                                                :class "identity event-host"
                                                :onchange "this.form.submit()")))))
            (when (or (getf *user-group-privileges* :member)
                      (getf *user-group-privileges* :admin))
              (str (privacy-selection-html
                     "event"
                     restrictedp
                     (if (not (equal identity-selection *userid*))
                       (list (cons identity-selection (db identity-selection :name)))
                       (append (groups-with-user-as-member)
                               (groups-with-user-as-admin)))
                     groups-selected
                     :onchange "this.form.submit()"))))
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
                          "Create"))))))))

(defun get-event (id)
  (setf id (parse-integer id))
  (aif (db id)
    (require-user
      (if (and (not (find *userid* (getf it :hosts)))
               (item-view-denied (result-privacy (gethash id *db-results*))))
        (permission-denied)
        (standard-page
          "Event"
          (html
            (str (event-activity-item (gethash id *db-results*)
                                      :time (when (getf it :recurring)
                                              (humanize-exact-time (getf it :local-time))))))
          :selected "events")))
      (not-found)))

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
                                       (getf item :local-end-date)))
                   (title (or (post-parameter-string "title")
                              (getf item :title)))
                   (details (or (post-parameter-string "details")
                                (getf item :details)))
                   (location (or (post-parameter-string "location")
                                 old-location))
                   (new-lat (post-parameter-float "lat"))
                   (new-long (post-parameter-float "long"))
                   (lat (or new-lat (getf item :lat)))
                   (long (or new-long (getf item :long)))
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
                   (by-day-or-date (when (equalp frequency "monthly")
                                     (or (post-parameter-string
                                           "by-day-or-date")
                                         (awhen (getf item :by-day-or-date)
                                           (symbol-name it)))))
                   (symbol-day-or-date (awhen by-day-or-date
                                         (k-symbol (or-string= it
                                                   '("day" "date")))))
                   (weeks-of-month (when (equalp frequency "monthly")
                                     (if (and (= 1 interval)
                                              (post-parameter "weeks-of-month"))
                                       (post-parameter-string-list
                                         "weeks-of-month"
                                         #'(lambda (day)
                                             (find day
                                                   +positions-of-day-in-month+
                                                   :test #'equalp)))
                                       (awhen (getf item :weeks-of-month)
                                         (mapcar #'symbol-name it)))))
                   (symbol-weeks-of-month (awhen weeks-of-month
                                           (mapcar #'k-symbol it)))
                   (identity-selection (post-parameter-integer "identity-selection"))
                   (groupid (or identity-selection
                                (post-parameter-integer "groupid")))
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

              (labels ((try-again (&optional e)
               ;; needs to be labels not flet because of
               ;; submit-data's parameter precedence
                         (enter-event-details :title title
                                              :existing-url url
                                              :details details
                                              :location location
                                              :recurring recurring
                                              :frequency frequency
                                              :interval interval
                                              :days-of-week days-of-week
                                              :by-day-or-date by-day-or-date
                                              :weeks-of-month weeks-of-month
                                              :local-day-of-week local-day-of-week
                                              :end-date (awhen local-end-time(humanize-exact-time it))
                                              :date date
                                              :time time
                                              :restrictedp restrictedp
                                              :groups-selected groups-selected
                                              :identity-selection identity-selection
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
                                             :privacy groups-selected
                                             :weeks-of-month symbol-weeks-of-month
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
                                        :privacy groups-selected
                                        :title title
                                        :details details
                                        :address location
                                        :recurring recurring
                                        :frequency symbol-frequency
                                        :interval interval
                                        :days-of-week symbol-days-of-week
                                        :by-day-or-date symbol-day-or-date
                                        :weeks-of-month symbol-weeks-of-month
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
                  (try-again))

                 ((post-parameter "delete")
                  (confirm-delete :url (script-name*)
                                  :type "event"
                                  :text (getf item :title)
                                  :next-url (referer)))

                 ((post-parameter "really-delete")
                  (delete-inventory-item id)
                  (flash "Your event has been deleted!")
                  (see-other (or (post-parameter "next") "/home")))

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
                                                    (humanize-exact-time it))))))

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
