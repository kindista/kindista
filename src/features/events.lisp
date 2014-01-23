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

(defun create-event (&key (host *userid*) lat long title details local-time address)
  (insert-db (list :type :event
                   :hosts (list host)
                   :lat lat
                   :long long
                   :address address
                   :title title
                   :details details
                   :local-time local-time
                   :created (get-universal-time))))

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

(defun modify-event (id &key lat long title details local-time address)
  (let* ((result (gethash id *db-results*))
         (data (db id))
         (old-address (getf data :address))
         (old-title (getf data :title))
         (old-details (getf data :details))
         (old-time (getf data :local-time))
         (old-lat (getf data :lat))
         (old-long (getf data :long))
         (new-lat (awhen lat (unless (= old-lat it) it)))
         (new-long (awhen long (unless (= old-long it) it)))
         (new-time (awhen local-time (unless (= old-time it) it)))
         (new-details (awhen details (unless (string= old-details it) it)))
         (new-address (awhen address (unless (string= old-address it) it)))
         (new-title (awhen title (unless (string= old-title it) it)))
         (now (get-universal-time)))

    (when (or new-title new-details)
      (let* ((oldstems (stem-text (s+ (getf data :title) " " (getf data :details))))
             (newstems (stem-text (s+ (or new-title (getf data :title))
                                      (or new-details (getf data :details)))))
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

    (when (or new-lat
              new-long)
      (geo-index-remove *event-geo-index* result)
      (setf (result-latitude result) new-lat)
      (setf (result-longitude result) new-long)
      (geo-index-insert *event-geo-index* result))

    (when new-time
      (setf (result-time result) new-time)
      (with-mutex (*event-mutex*)
        ; remove past events from event-index
        ; add this event and sort the index
        (flet ((past-event (item) (< (result-time item)
                                     (- now (* 3 +day-in-seconds+)))))
          (asetf *event-index* (sort (remove-if #'past-event it)
                                 #'< :key #'result-time)))))

    (when (or new-title new-details new-lat new-long new-address new-time)
      (modify-db id :title (or new-title old-title)
                    :details (or new-details old-details)
                    :lat (or new-lat old-lat)
                    :long (or new-long old-long)
                    :address (or new-address old-address)
                    :local-time (or new-time old-time)
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
                           (sort (copy-list *event-index*) #'< :key #'result-time)))


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

(defun enter-event-details (&key error date time location title groupid details existing-url recurring frequency interval days-of-week by-day-or-date days-of-month end-date)
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

         (:div
           (:label "Repeats:")
           (:input :type "radio"
                   :name "frequency"
                   :value "weekly"
                   :onclick "this.form.submit()"
                   :checked (unless (eql frequency 'monthly) ""))
           "Weekly"
           (:input :type "radio"
                   :name "frequency"
                   :value "monthly"
                   :onclick "this.form.submit()"
                   :checked (when (eql frequency 'monthly) ""))
           "Monthly")

         (:div
           (:label "Repeat Every")
           (str (number-selection-html "interval" 12 (or interval 1)))
           (str (if (eql frequency 'monthly) "months" "weeks")))

         (unless (eql frequency 'monthly)
           (htm
             (:div
               (:label "Repeat on")
               (dolist (day +day-names+)
                 (htm
                   (:input :type "checkbox"
                           :name "days-of-week"
                           :checked (when (find day
                                                days-of-week
                                                :test #'equalp)
                                      "checked")
                           :value (string-downcase day)
                           (str (elt day 0))))))))

         (:div
           (:label "Repeat by")
           (:input :type "radio"
                   :name "by-day-or-date"
                   :value "date"
                   :onclick "this.form.submit()"
                   :checked (unless (eql by-day-or-date 'date) ""))
           "day of the month"
           (:input :type "radio"
                   :name "by-day-or-date"
                   :value "day"
                   :onclick "this.form.submit()"
                   :checked (when (eql by-day-or-date 'date) ""))
           "day of the week")

         (:div
           (:label "Days of the month")
             (dolist (option +positions-of-day-in-month+)
               (pprint option)
               (htm (:input :type "checkbox"
                            :name "days-of-month"
                            :checked (when (find option days-of-month :test #'equalp)
                                       "checked")
                            :value option
                            (str option)))))

         (:div
           (:label "End Date")
           (:input :type "text"
                   :name "end-date"
                   :placeholder "mm/dd/yyyy"
                   :value end-date))

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

(defun post-events-new ()
  (require-active-user
    (let* ((title (post-parameter-string "title"))
           (groupid (or (post-parameter-integer "identity-selection")
                        (post-parameter-integer "groupid")))
           (adminp (group-admin-p groupid))
           (details (post-parameter-string "details"))
           (location (post-parameter-string "location"))
           (lat (post-parameter-float "lat"))
           (long (post-parameter-float "long"))
           (date (when (scan +date-scanner+ (post-parameter "date"))
                   (post-parameter "date")))
           (time (post-parameter "time"))
           (local-time (when (and date time)
                         (handler-case
                           (parse-datetime date time)
                           (local-time::invalid-time-specification () nil)))))

      (labels ((try-again (e) (enter-event-details :title title
                                                   :details details
                                                   :groupid (when adminp
                                                              groupid)
                                                   :location location
                                                   :date date
                                                   :time time
                                                   :error e)))
        (cond
         ((getf *user* :pending)
          (pending-flash "post events on Kindista")
          (see-other (or (referer) "/home")))

         ((post-parameter "cancel")
          (see-other (or (post-parameter "next") "/home")))

         ((not title)
          (try-again "Please enter a title for your event"))

         ((< (length title) 4)
          (try-again "Please enter a longer title for your event"))

         ((not details)
          (try-again "Please add some details for your event"))

         ((< (length details) 8)
          (try-again "Please enter some more details for your event"))

         ((not location)
          (try-again "Please add a location for your event"))

         ((not date)
          (try-again "Dates must be in the form of mm/dd/yyyy (e.g. 12/30/2013)"))

         ((not time)
          (try-again "Please enter a valid time (e.g. 2:30 pm)"))

         ((not local-time)
          (try-again "We couldn't understand your time. Please specify just one start time for the event (e.g. \"2:30pm\")"))

         ((< local-time (- (get-universal-time) +day-in-seconds+))
          ; local-time for any given time zone must not be earlier than today.
          (try-again "Please enter a future date for your event"))

         ((post-parameter "reset-location")
          (enter-event-details :title title
                               :groupid (when adminp groupid)
                               :details details
                               :date date
                               :time time))

         ((not (and lat long))
          (multiple-value-bind (latitude longitude address)
            (geocode-address location)
            (verify-location "/events/new"
                             "Please verify the location of your event."
                             latitude
                             longitude
                             "location" address
                             "groupid" (when adminp groupid)
                             "title" title
                             "details" details
                             "date" date
                             "time" time)))

         ((post-parameter "confirm-location")
          (flash "Your event has been added to the calendar")
          (see-other (format nil "/events/~A"
                                 (create-event :lat lat
                                               :host (when adminp groupid)
                                               :long long
                                               :local-time local-time
                                               :title title
                                               :details details
                                               :address location)))))))))

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

(defun post-event (id)
  (require-active-user
    (let* ((id (parse-integer id))
           (item (db id))
           (hosts (getf item :hosts))
           (group-adminp (loop for host in hosts
                               thereis (group-admin-p host)))
           (old-location (getf item :address))
           (old-title (getf item :title))
           (old-details (getf item :details))
           (old-local-time (getf item :local-time))
           (url (strcat "/events/" id)))

       (require-test ((or (member *userid* (getf item :hosts))
                          group-adminp
                          (getf *user* :admin))
                      (s+ "You can only edit your own events."))

         (multiple-value-bind (old-time date-name old-date)
           (humanize-exact-time old-local-time)
           (declare (ignore date-name))

           (let* ((title (or (post-parameter-string "title")
                             old-title))
                  (details (or (post-parameter-string "details")
                               old-details))
                  (location (or (post-parameter-string "location")
                                old-location))
                  (lat (post-parameter-float "lat"))
                  (long (post-parameter-float "long"))
                  (new-date-p (scan +date-scanner+ (post-parameter "date")))
                  (new-time-p (scan +time-scanner+ (post-parameter "time")))
                  (date (or (when new-date-p (post-parameter "date"))
                            old-date))
                  (time (or (when new-time-p (post-parameter "time"))
                            old-time))
                  (recurring (when (post-parameter "recurring") t))
                  (frequency (awhen (post-parameter-string "frequency")
                               (intern it)))
                  (interval (post-parameter-integer "interval"))
                  (days-of-week (post-parameter-string-list
                                  "days-of-week"
                                  #'(lambda (day)
                                      (find day +day-names+ :test #'equalp))))
                  (by-day-or-date (intern (post-parameter-string
                                            "by-day-or-date")))
                  (days-of-month (post-parameter-string-list
                                   "days-of-month"
                                   #'(lambda (day)
                                       (find day +positions-of-day-in-month+
                                             :test #'equalp))))



                  (local-time (handler-case (parse-datetime date time)
                                    (local-time::invalid-time-specification () nil))))
             (labels ((try-again (e) (enter-event-details :title title
                                                          :existing-url url
                                                          :details details
                                                          :location location
                                                          :date date
                                                          :time time
                                                          :error e))
                      (modify-data (&key lat long address)
                        (modify-event id :lat lat
                                         :long long
                                         :address address
                                         :local-time local-time
                                         :details details
                                         :title title)))
               (cond
                ((post-parameter "cancel")
                 (see-other (or (script-name*) "/home")))

                ((and (post-parameter "love"))
                 (love id)
                 (see-other (or (post-parameter "next") (referer))))

                ((and (post-parameter "unlove"))
                 (unlove id)
                 (see-other (or (post-parameter "next") (referer))))

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
                 (try-again "Please enter a valid time (e.g. 2:30 pm)"))

                ((not local-time)
                 (try-again "Please enter a valid date and time"))


                ((< local-time (- (get-universal-time) +day-in-seconds+))
                 ; local-time for any given time zone
                 ; must not be earlier than today.
                 (try-again "Please enter a future date for your event"))

                ((and (string= location old-location)
                      (string= details old-details)
                      (string= title old-title)
                      (= local-time old-local-time))
                 (try-again "You haven't entered any new details. Nothing has been changed."))

                ((and (not (and lat long))
                      (not (string= location old-location)))
                 ; if there's a new location
                 (multiple-value-bind (latitude longitude address)
                   (geocode-address location)
                   (if (string= address old-location)
                     ; if the geocoded new location is the same as
                     ; the old address
                     (progn
                       (modify-data)
                       (flash "Your event has been updated")
                       (see-other url))
                     (verify-location url
                                      "Please verify the location of your event."
                                      latitude
                                      longitude
                                      "location" address
                                      "title" title
                                      "details" details
                                      "date" date
                                      "time" time))))

                ((post-parameter "submit-edits")
                 (modify-data)
                 (flash "Your event has been updated")
                 (see-other url))

                ((post-parameter "confirm-location")
                 (modify-data :lat lat :long long :address location)
                 (flash "Your event has been updated")
                 (see-other url))))))))))

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
