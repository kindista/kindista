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

(defun timestamp (time &key type verb class)
  (html
    (:h3 :class (strcat* "timestamp " class) :data-time time :data-type type
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


(defun activity-icons (&key url hearts comments)
  (html
    (:a :class "icons" :href url
      (when (and hearts (> hearts 0))
        (htm
          (:img :alt "love:" :src "/media/icons/heart16.png") 
          ;(:span :class "unicon" "♥ ")
          (:span (str hearts)))) 
      (when (and comments (> comments 0))
        (htm
          (:img :alt "comments:" :src "/media/icons/comment16.png") 
          ;(:span :class "unicon" " ✎ ")
          (:span (str comments)))))))

(defun activity-item
  (&key id url share-url content time primary-action hearts comments distance delete deactivate reactivate image-text edit reply class admin-delete related-items matchmaker (show-actions t)
   &aux (here (request-uri*))
        (next (if (find (strcat id) (url-parts here) :test #'string=)
                here
                (strcat here "#" id))))

  (html
    (:div :class (if class (s+ "activity card " class) "card") :id id
      (:div :class "activity item"
       ;(:img :src (strcat "/media/avatar/" user-id ".jpg"))
       (awhen time (str it))
       (when distance
         (htm
           (:p :class "distance"
            "within " (str (distance-string distance)))))
       (:div :class "item-text"(str content))
       (when (and matchmaker related-items)
         (str related-items))
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
             (str (activity-icons :hearts hearts :comments comments :url url))
             (:form :method "post" :action (strcat "/love/" id)
               (:input :type "hidden" :name "next" :value next)
               (if (find id (loves *userid*))
                 (htm (:input :type "submit" :name "unlove" :value "Loved"))
                 (htm (:input :type "submit" :name "love" :value "Love"))))
             (:form :method "post" :action url
               (:input :type "hidden"
                       :name "next"
                       ;; don't use anchor link. otherwise user can't see
                       ;; the success flash after they post a reply
                       :value (request-uri*))
               (awhen share-url
                 (htm
                   " &middot; "
                   (:a :href it "Share on Facebook")))
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
               (when edit
                 (htm
                   " &middot; "
                   (:input :type "submit" :name "edit" :value "Edit")
                   " &middot; "
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
                                   :value "Delete")))))))
               (awhen reactivate
                 (htm
                   " &middot; "
                   (:a :href it "Reactivate")))

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
                   (str (comment-button url))))))))

      (when (and related-items (not matchmaker))
        (htm
          (:div :class "related activity items"
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
      :id (strcat item-id ":" (or date local-time))
      :url item-url
      :class (strcat* (when featuredp "featured ") "event")
      :edit (or (eql host *userid*)
                group-adminp
                (getf *user* :admin))
      :deactivate (or (eql host *userid*)
                      group-adminp
                      (getf *user* :admin))
      :hearts (length (getf data :loved-by))
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
                        (htm (:small
                          " (within "
                          (str
                            (distance-string
                              (air-distance (result-latitude result)
                                            (result-longitude result)
                                            *latitude*
                                            *longitude*)))
                        ")"))))))

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
    &aux (user-id (first (result-people result)))
         ; tests to see if the user is the author of this item
         (self (eql user-id *userid*))
         ;item-id is derived from "result" which is passed in.
         (item-id (result-id result))
         ; data is a plist with all relevant info about the gratitude
         ; item.
         (data (db item-id))
         (author (getf data :author))
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
      :hearts (length (getf data :loved-by))
      ;:comments (length (comments item-id))
      :content (html
                 (:p (str (person-link user-id))
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
      :related-items (when (and reciprocity (getf data :on) show-on-item)
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
                   :hearts (length (getf data :loved-by))
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
        show-what
        show-tags
        (show-when t)
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
        (renew-link (s+ (url-compose item-url
                                     "edit" "t"
                                     "focus" "expiration")
                        "#expiration"))
        (time (when active-p
                (if self (getf data :expires) (result-time result))))
        (timestamp (when (and active-p show-when)
                     (timestamp
                       time
                       :class (when (and self
                                         (< time (+ (get-universal-time)
                                                    (* 5 +day-in-seconds+))))
                                "red")
                       :type type
                       :verb (unless (and show-what (not self))
                               (cond
                                 (self "expires")
                                 ((getf data :refreshed) "refreshed")
                                 ((getf data :edited) "edited")
                                 ((string= type "request") "requested")
                                 ((string= type "offer") "offered")))))))

  (when self
    (asetf timestamp (html (:a :href renew-link
                             (if it
                               (str it)
                               (htm (:h3 :class "timestamp red" "inactive")))))))

  (activity-item :id item-id
                 :url item-url
                 :time timestamp
                 :edit (and active-p
                            (or self group-adminp (getf *user* :admin)))
                 :deactivate (and active-p
                                  (or self group-adminp (getf *user* :admin)))
                 :reactivate (when (and (not active-p)
                                        (or self
                                            group-adminp
                                            (getf *user* :admin)))
                               renew-link)
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
                 :share-url (when (and self (not *productionp*))
                              (url-compose
                                "https://www.facebook.com/dialog/share_open_graph"
                                "app_id" *facebook-app-id*
                                "display" "popup"
                                "action_type" "kindistadotorg:post"
                                "action_properties" (url-encode
                                                      (json:encode-json-to-string
                                                        (list
                                                          (cons
                                                            type
                                                            (s+ "https://kindista.org" item-url)))))
                                "redirect_uri" (s+ +base-url+
                                                   (string-left-trim "/" (request-uri*)))))
                 :reply (unless (or self
                                    (not (getf data :active)))
                          t)
                 :hearts (length (getf data :loved-by))
                 :matchmaker (or (getf *user* :matchmaker)
                                 (and (or group-adminp self)
                                    (string= type "request")))

                 :content (inventory-item-content result
                                                  :truncate truncate
                                                  :data data
                                                  :show-distance show-distance
                                                  :show-what show-what
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
   &key show-distance show-what show-tags truncate data
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
      (when (or title show-what)
        (htm (:div :class "inventory-title"
               (when (and title show-what)
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
                       (str (inventory-activity-item item :show-what t :show-distance location :show-tags show-tags :truncate t)))
                     (:request
                       (str (inventory-activity-item item :show-what t :show-distance location :truncate t :show-tags show-tags)))))
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
                                         (activity-rank
                                           result
                                           :sitewide (eql distance 0))))
                               local-items))
         (items (mapcar #'car (sort ranked-items #'< :key #'cdr))))

    (activity-items items :page page
                          :count count
                          :reciprocity t
                          :url url
                          :show-tags show-tags)))

