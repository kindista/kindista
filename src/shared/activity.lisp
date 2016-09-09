;;; Copyright 2012-2016 CommonGoods Network, Inc.
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

(defun timestamp (time &key type verb class icon)
  (html
    (:h3 :class (strcat* "timestamp " class) :data-time time :data-type type
       (awhen icon (str it))
       (awhen verb (htm (str it) " "))
       (str (humanize-universal-time time)))))

(defun comment-button (url)
  (html
    (:form :method "GET" :action url
      (:input :type "submit" :value "Discuss"))))

(defun flag-button (url)
  (html
    (:form :method "GET" :action (s+ url "/flag")
      (:input :type "submit" :value "Flag"))))


(defun activity-icons (&key url hearts)
  (html
    (:a :class "icons" :href url
      (when (and hearts (> hearts 0))
        (htm
          (:img :alt "love:" :src "/media/icons/heart16.png") 
          ;(:span :class "unicon" "♥ ")
          (:span (str hearts)))) 
     ;(when (and comments (> comments 0))
     ;  (htm
     ;    (:img :alt "comments:" :src "/media/icons/comment16.png") 
     ;    ;(:span :class "unicon" " ✎ ")
     ;    (:span (str comments))))
      )))

(defun activity-item
  (&key id url publish-facebook content time primary-action loves comments distance delete deactivate reactivate image-text edit reply class admin-delete related-items matchmaker (show-actions t) event-time
   &aux (here (request-uri*))
        (next (if (find (strcat id) (url-parts here) :test #'string=)
                here
                (strcat here "#" id))))

  (html
    (:div :class (if class (s+ "activity card " class) "card")
          :id (strcat* id (awhen event-time (strcat ":" it)))
      (:div :class "activity item"
       ;(:img :src (strcat "/media/avatar/" user-id ".jpg"))
       (awhen time (str it))
       (when distance
         (htm
           (:p :class "distance"
            "within " (str (distance-string distance)))))
       (:div :class "item-text"(str content))
       (when (and *user* show-actions)
         (htm
           (awhen primary-action
             (htm
               (:form :class "primary-action" :method "post" :action url
                (:input :type "hidden"
                 :name "next"
                 ;; don't use anchor link. otherwise user can't see
                 ;; the success flash after they post a reply
                 :value (request-uri*))
                (dolist (item (getf it :hidden))
                  (htm (:input :type "hidden"
                        :name (car item)
                        :value (cdr item))))
                (:button :type "submit"
                 :class "small blue primary-action"
                 :name (getf it :name)
                 :value (getf it :value)
                 (when (getf it :image)
                   (str (getf it :image)))
                 ;; following needs div instead of span because of a
                 ;; firefox hover/underline bug
                 (:div (str (getf it :text)))))))
           (:div :class "actions"
             (:form :method "post" :action (strcat "/love/" id)
               (:input :type "hidden" :name "next" :value next)
               (if (find id (getf *user* :loves))
                 (htm (:input :type "submit" :name "unlove" :value "Loved"))
                 (htm (:input :type "submit" :name "love" :value "Love"))))
             (:form :method "post" :action url
               (:input :type "hidden"
                       :name "next"
                       ;; don't use anchor link. otherwise user can't see
                       ;; the success flash after they post a reply
                       :value (request-uri*))
               (when reply
                 (htm
                  " &middot; "
                  (:input :type "submit" :name "reply" :value "Reply")))
               (when matchmaker
                 (htm
                  " &middot; "
                  (:a :href (url-compose url "selected" "matchmaker")
                   "Matchmaker")))
               (when delete ;for gift-activity-items
                 (htm
                   " &middot; "
                   (:input :type "submit" :name "delete" :value "Delete")))
               (when (or edit reactivate)
                 (htm
                   " &middot; "
                   (:input :type "submit" :name "edit" :value "Edit")
                   " &middot; "
                   (if reactivate
                     (htm (:input :type "submit"
                           :name "reactivate"
                           :value "Reactivate"))
                     (cond
                       (admin-delete
                         (htm (:input :type "submit"
                               :name "inappropriate-item"
                               :value "Inappropriate")))
                       (deactivate
                         (htm (:input :type "submit"
                               :name "deactivate"
                               :value "Deactivate")))
                       (t
                        (htm (:input :type "submit"
                              :name "delete"
                              :value "Delete"))))))))

               (when image-text
                 (htm
                   " &middot; "
                   (str (new-image-form "/image/new"
                                        (script-name*)
                                        :on id
                                        :button image-text))))
               (when comments
                 (htm
                   " &middot; "
                   (str (comment-button url))))

               (when publish-facebook
                 (htm
                   (:form :method "post" :action url
                     " &middot; "
                     (:input :type "submit" :name "publish-facebook" :value "Share on Facebook"))))))))

      (when (or loves related-items)
        (htm
          (:div :class "related activity items"
            (when loves
              (str (users-who-love-item-html loves id url)))
            (when (and loves related-items)
              (htm (:hr)))
            (str related-items)))))))

      ;(unless (eql user-id *userid*)
      ;  (htm
      ;    " &middot; "
      ;    (str (flag-button url))))))))

(defun event-activity-item (result &key featuredp sidebar truncate (show-distance nil) time date)
  (let* ((host (first (result-people result)))
         (item-id (result-id result))
         (data (db item-id))
         (group-adminp (group-admin-p host))
         (local-time (getf data :local-time))
         (event-time (or time (humanize-exact-time local-time :detailed t)))
         (recurring (getf data :recurring))
         (item-url (strcat "/events/" item-id)))

    (activity-item
      :id item-id
      :event-time (or date local-time)
      :url item-url
      :class (strcat* (when featuredp "featured ") "event")
      :edit (or (eql host *userid*)
                group-adminp
                (getf *user* :admin))
      :deactivate (and (getf data :active)
                       (or (eql host *userid*)
                         group-adminp
                         (getf *user* :admin)))
      :reactivate (and (not (getf data :active))
                       (or (eql host *userid*)
                         group-adminp
                         (getf *user* :admin)))
      :loves (loves item-id)
      ;:comments (length (comments item-id))
      :content (html
                 (:h3 (:a :href item-url
                                (str
                                  (if truncate
                                    (ellipsis
                                      (getf data :title)
                                      :length (if sidebar 33 50))
                                    (html-text (getf data :title))))))
                 (unless sidebar
                   (htm
                     (:p
                       (str (if (getf data :editied)
                              "Edited by "
                              "Posted by "))
                       (str (person-link host))
                       "&nbsp;"
                       (str (humanize-universal-time (getf data :created))))))

                 (:table
                   (:tr
                     (:td (:strong "Time: "))
                     (:td (str event-time)))

                   (when recurring
                     (htm
                       (:tr
                         (:td (:strong "Schedule: "))
                         (:td
                           (str
                             (recurring-event-schedule
                               item-id
                               data
                               (string= (script-name*) item-url)))))))
                   (:tr
                     (:td (:strong "Place: "))
                     (:td (str (getf data :address))
                      (when (and show-distance (not sidebar))
                        (with-location
                          (htm (:small
                            " (within "
                            (str
                              (distance-string
                                (air-distance (result-latitude result)
                                              (result-longitude result)
                                              *latitude*
                                              *longitude*)))
                          ")")))))))

                 (:p
                   (str
                     (if truncate
                       (ellipsis (getf data :details)
                                 :length (if sidebar 150 500)
                                 :see-more item-url)
                       (html-text (getf data :details)))))))))

(defun gratitude-activity-item
  (result
    &key truncate
         reciprocity
         (show-on-item t)
         (show-when t)
         (show-actions t)
         (reciprocity-to-show (when reciprocity
                                (display-gratitude-reciprocities result)))
         ; result-people is a list
         ; car of list is person showing gratitude
         ; cdr of list is subjects
    &aux ;item-id is derived from "result" which is passed in.
         (item-id (result-id result))
         ; data is a plist with all relevant info about the gratitude
         ; item.
         (data (db item-id))
         ; tests to see if the user is the author of this item
         (author (getf data :author))
         (self (eql author *userid*))
        ;(gratitude-recipients (remove author (result-people result)))
         (adminp (group-admin-p author))
         (images (getf data :images))
         (item-url (strcat "/gratitude/" item-id)))

  (unless (getf data :pending)
    (activity-item
      :id item-id
      :url item-url
      :class "gratitude"
      :time (when show-when
              (timestamp (result-time result)
                         :type "gratitude"))
      :show-actions show-actions
      :edit (when (or self adminp (getf *user* :admin)) t)
      :image-text (when (and (or self adminp)
                             (< (length images) 5))
                    "Add photos")
      :loves (loves item-id)
      ;:comments (length (comments item-id))
      :content (html
                 (:p (str (person-link author))
                     " expressed "
                     (:a :href item-url "gratitude")
                     " for "
                     (str (name-list (getf data :subjects)
                                     :maximum-links 100)))
                 (:p
                   (str
                     (if truncate
                       (ellipsis (getf data :text)
                                 :length 500
                                 :see-more item-url)
                       (html-text (getf data :text)))))
                 (unless (string= item-url (script-name*))
                   (str (activity-item-images images item-url "gift"))))
      :publish-facebook (and self
                            (getf *user* :fb-id)
                            (not (getf data :fb-publishing-in-process))
                            (not (fb-object-actions-by-user
                                   item-id
                                   :data data)))
      :related-items (when (and (getf data :on) show-on-item)
                       (html
                         (when (and (getf data :on) show-on-item)
                           (str (gratitude-on-item-html
                                  item-id
                                  :gratitude-data data)))
                         (when (and reciprocity
                                    (getf data :on)
                                    reciprocity-to-show
                                    show-on-item)
                           (htm (:hr)))
                         (awhen reciprocity-to-show
                           (str it)))))))


(defun gift-activity-item (result)
  (let* ((user-id (first (result-people result)))
         (item-id (result-id result))
         (data (db item-id)))
    (activity-item :id item-id
                   :url (strcat "/gifts/" item-id)
                   :class "gratitude"
                   :time (timestamp (result-time result) :type "gift")
                   :loves (loves item-id)
                   :delete (when (or (eql user-id *userid*) (getf *user* :admin)) t)
                   :comments (length (comments item-id))
                   :content (html
                              (:p (str (person-link user-id))
                                  " was "
                                  (:a :href (strcat "/gifts/" item-id) "acknowledged") 
                                  " by "
                                  (str (name-list-all (getf data :recipients))))
                              (:p (str (html-text (getf data :text))))))))

(defun joined-activity-item (result)
  (html
    (:div :class "card joined"
      (str (timestamp (result-time result)))
      (:p (str (person-link (first (result-people result)))) " joined Kindista"))))

(defun inventory-activity-item
  (result
   &key truncate
        show-distance
        show-icon
        show-tags
        show-recent-action
   &aux (user-id (first (result-people result)))
        (self (eql user-id *userid*))
        (item-id (result-id result))
        (data (db item-id))
        (by (getf data :by))
        (requestp (eql (getf data :type) :request))
        (type (if requestp "request" "offer"))
        (group-adminp (group-admin-p by))
        (admin-matchmaker (matchmaker-admin-p))
        (images (getf data :images))
        (item-url (strcat "/" type "s/" item-id))
        (active-p (getf data :active))
        (refreshed (getf data :refreshed))
        (edited (getf data :edited))
        (renew-link (s+ (url-compose item-url
                                     "edit" "t"
                                     "focus" "expiration")
                        "#expiration"))
        (time (when active-p
                (if self (getf data :expires) (result-time result))))
        (timestamp (if active-p
                     (timestamp
                       time
                       ;; indicate to admins which items are refreshed
                       ;; to make sure refreshing thread hasn't died
                       :icon (when (and (getf *user* :admin)
                                        refreshed
                                        (or (not edited)
                                            (> refreshed edited)))
                               (icon "home"))
                       :class (when
                                (or (not active-p)
                                    (and self
                                         (< time (+ (get-universal-time)
                                                    (* 5 +day-in-seconds+)))))
                                "red")
                       :type type
                       :verb (if self
                               "expires"
                               (when show-recent-action
                                 (cond
                                   ((and refreshed edited)
                                    (if (> refreshed edited)
                                      "refreshed"
                                      "edited"))
                                   (refreshed "refreshed")
                                   (edited  "edited")
                                   ((string= type "request") "requested")
                                   ((string= type "offer") "offered")))))
                     (html (:h3 :class "timestamp red" "inactive")))))

  (when self
    (asetf timestamp (html (:a :href renew-link (str it)))))

  (activity-item :id item-id
                 :url item-url
                 :time timestamp
                 :edit (and active-p
                            (or self group-adminp (getf *user* :admin)))
                 :deactivate (and active-p
                                  (or self group-adminp (getf *user* :admin)))
                 :reactivate (and (not active-p)
                                  (or self
                                      group-adminp
                                      (getf *user* :admin)))
                 :admin-delete (and (getf *user* :admin)
                                    (not self)
                                    (not group-adminp))
                 :image-text (when (and (or self group-adminp)
                                        (< (length images) 5))
                               "Add photos")
                 :primary-action (unless self
                                   (if (string= type "request")
                                     (list :name "action-type"
                                           :value "offer"
                                           :hidden (list (cons "reply" "t"))
                                           :text "Offer This"
                                           :image (icon "white-offer"))
                                     (list :name "action-type"
                                           :value "request"
                                           :hidden (list (cons "reply" "t"))
                                           :text "Request This"
                                           :image (icon "white-request"))))
                 :class (s+ type " inventory-item")
                 :publish-facebook (and self
                                        (or *enable-facebook*
                                            (getf *user* :test-user))
                                        (getf *user* :fb-id)
                                        (not (getf data :fb-publishing-in-process))
                                        (not (fb-object-actions-by-user
                                               item-id
                                               :data data)))
                 :reply (unless (or self
                                    (not (getf data :active)))
                          t)
                 :loves (loves item-id)
                 :matchmaker (or (getf *user* :matchmaker)
                                 (and (or group-adminp self)
                                    (string= type "request")))

                 :content (inventory-item-content result
                                                  :truncate truncate
                                                  :data data
                                                  :show-distance show-distance
                                                  :show-icon show-icon
                                                  :show-tags show-tags)

                 :related-items (when (and (or self
                                               group-adminp
                                               admin-matchmaker)
                                           (not (string= (script-name*)
                                                         item-url)))
                                  (let ((matches (case (getf data :type)
                                                   (:offer (length (gethash item-id *offers-with-matching-requests-index*)))
                                                   (:request (length (getf data :matching-offers))))))
                                    (when (> matches 0)
                                      (matching-item-count-html
                                        item-id
                                        type
                                        matches
                                        :admin (and admin-matchmaker
                                                    (not self))))))))

(defun inventory-item-content
  (result
   &key show-distance show-icon show-tags truncate data
   &aux (item-id (result-id result))
        (item (or data (db item-id)))
        (by (getf item :by))
        (requestp (eql (getf item :type) :request))
        (type (if requestp "request" "offer"))
        (q (awhen (get-parameter-string "q") (stem-text it)))
        (title (getf item :title))
        (images (getf item :images))
        (item-url (strcat "/" type "s/" item-id))
        (tags (getf item :tags)))
  (html
    (:div
      (when (or title show-icon)
        (htm (:div :class "inventory-title"
               (when (and title show-icon)
                 (htm (str (icon (if requestp "requests" "offers")))))
               (awhen title
                 (htm (:h3 :class "inventory-title"
                        (:a :href item-url
                          (str (if q
                                 (highlight-stems-in-text q it)
                                 it)))))))))
      (:div
        (str (if requestp
               "requested by "
               "offered by "))
        (str (person-link by))
        (when show-distance
          (with-location
            (htm (:small
                   " (within "
                   (str
                     (distance-string
                       (air-distance (result-latitude result)
                                     (result-longitude result)
                                     *latitude*
                                     *longitude*)))
                   ")")))))

      (awhen (getf item :details)
        (htm
          (:p
            (str (cond
                   (q (highlight-stems-in-text q it))
                   (truncate (ellipsis it :length 250 :see-more item-url))
                   (t (html-text it))))))))

    (when (and show-tags tags)
      (htm (:div :class "tags"
             "Tags:  "
             (str (display-tags type tags)))))

    (when (and images
               (not (string= item-url (script-name*))))
      (str (activity-item-images images item-url type)))))

(defun display-tags (type tags)
  (html
    (dolist (tag tags)
      (htm
        (:a :href (url-compose (strcat "/" type "s") "kw" tag) (str tag))
        (unless (eql tag (car (last tags)))
          (htm
            " &middot; "))))))

(defun activity-item-images (images url alt)
  (html
    (:div :class "small-activity-image"
      (dolist (image-id images)
        (htm
          (:div :class "border"
           (:a :href url (:img :src (get-image-thumbnail image-id 70 70)
                               :alt alt))))))))

(defun activity-items (items &key (page 0) (count 30) (url "/home")
                             (paginate t) (location t) reciprocity show-tags)
  (with-location
    (let ((start (* page count)))
      (html
        (iter (for i from 0 to (- (+ start count) 1))
              (cond
                ((< i start)
                 (setf items (cdr items)))

                ((and (>= i start) items)
                 (let* ((item (car items)))
                   (case (result-type item)
                     (:gratitude
                       (awhen (gratitude-activity-item item :truncate t
                                                            :reciprocity
                                                            reciprocity)
                         (str it)))
                     (:gift
                       (str (gift-activity-item item)))
                     (:person
                       (str (joined-activity-item item)))
                     (:offer
                       (str (inventory-activity-item item :show-icon t :show-distance location :show-tags show-tags :truncate t)))
                     (:request
                       (str (inventory-activity-item item :show-icon t :show-distance location :truncate t :show-tags show-tags)))))
                 (setf items (cdr items)))

                (t
                 (when (< (user-distance) 100)
                   (htm
                     (:div :class "item small"
                      (:em "Increasing the ")
                      (:strong "show activity within")(:em " distance may yield more results."))))
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
                         (:a :style "float: right;" :href (strcat url "?p=" (+ page 1)) "next page >"))))))))))))

(defun local-activity-items
  (&key (page 0)
        (count 40)
        (url "/home")
        show-tags
   &aux (distance (user-distance))
        (sitewide (eql distance 0)))

  (let* ((local-items (remove-private-items
                        (if sitewide
                          (copy-list *recent-activity-index*)
                          (geo-index-query *activity-geo-index*
                                         *latitude*
                                         *longitude*
                                         distance))))
         (ranked-items (mapcar #'(lambda (result)
                                   (cons result
                                         (multiple-value-list
                                           (activity-rank
                                             result
                                             :sitewide (eql distance 0)))))
                               local-items))
         (sorted-items (sort ranked-items #'> :key #'cadr))
         (items (remove-duplicates
                  (mapcar #'car sorted-items)
                  :from-end t
                  :key #'result-id)))

   ;for debugging rank
   ;(mapcar #'pprint (subseq sorted-items 0 4))
   ;(terpri)
    (activity-items items :page page
                          :count count
                          :reciprocity t
                          :url url
                          :show-tags show-tags)))

