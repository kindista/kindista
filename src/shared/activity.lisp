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

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun timestamp (time &key type)
  (html
    (:h3 :class "timestamp" :data-time time :data-type type
       (when type
         (htm (str type) " "))
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

(defun activity-item (&key id url content time hearts comments type distance delete image-text edit reply class admin-delete reciprocity)
  (html
    (:div :class (if class (s+ "card " class) "card") :id id
      ;(:img :src (strcat "/media/avatar/" user-id ".jpg"))
      (when time (str (timestamp time :type type)))
      (when distance
        (htm
          (:p :class "distance"
            "within " (str (distance-string distance)))))
      (str content)
      (when *user*
        (htm
          (:div :class "actions"
            (str (activity-icons :hearts hearts :comments comments :url url))
            (:form :method "post" :action url
              (:input :type "hidden" :name "next" :value (request-uri*))
              (if (member *userid* (gethash id *love-index*))
                (htm (:input :type "submit" :name "unlove" :value "Loved"))
                (htm (:input :type "submit" :name "love" :value "Love")))
              (when reply
                 (htm
                  " &middot; "
                  (:input :type "submit" :name "reply" :value "Reply")))
              (when delete
                (htm
                  " &middot; "
                  (:input :type "submit" :name "delete" :value "Delete")))
              (when edit
                (htm
                  " &middot; "
                  (:input :type "submit" :name "edit" :value "Edit")
                  " &middot; "
                  (if admin-delete
                    (htm (:input :type "submit" :name "inappropriate-item" :value "Inappropriate"))
                    (htm (:input :type "submit" :name "delete" :value "Delete"))))))
            (when (and image-text
                       (not (string= url (script-name*))))
              (htm
                " &middot; "
                (:a :href url (str image-text))))
            (when comments
              (htm
                " &middot; "
                (str (comment-button url)))))))
      (awhen reciprocity (str it)))))

        ;(unless (eql user-id *userid*)
        ;  (htm
        ;    " &middot; "
        ;    (str (flag-button url))))))))

(defun event-activity-item (result &key sidebar truncate (show-distance nil) time)
  (let* ((host (first (result-people result)))
         (item-id (result-id result))
         (data (db item-id))
         (group-adminp (group-admin-p host))
         (event-time (or time (humanize-exact-time (getf data :local-time)
                                                   :detailed t)))
         (recurring (getf data :recurring))
         (item-url (strcat "/events/" item-id)))

    (activity-item
      :id item-id
      :url item-url
      :class "event"
      :edit (when (or (eql host *userid*)
                      group-adminp
                      (getf *user* :admin)) t)
      :hearts (length (loves item-id))
      ;:comments (length (comments item-id))
      :content(html
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
                             (recurring-event-schedule item-id
                                                       data
                                                       (string= (script-name*)
                                                                item-url)))))))
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

(defun gratitude-activity-item (result &key truncate reciprocity)
  ; we should probably get rid of these comments -DJB
         ; result-people is a list
         ; car of list is person showing gratitude
         ; cdr of list is subjects
  (let* ((user-id (first (result-people result)))
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
    (html
    (str (activity-item :id item-id
                   :url item-url
                   :class "gratitude"
                   :time (result-time result)
                   :edit (when (or self adminp (getf *user* :admin)) t)
                   :image-text (when (or self adminp)
                                 (if images
                                   "Add/remove photos"
                                   "Add photos"))
                   :hearts (length (loves item-id))
                   ;:comments (length (comments item-id))
                   :content (html
                              (:p (str (person-link user-id))
                                  (str (if (getf data :edited) " edited " " shared "))
                                  (:a :href item-url "gratitude")
                                  " for "
                                  (str (name-list (getf data :subjects) :maximum-links 100)))
                              (:p
                                (str
                                  (if truncate
                                    (ellipsis (getf data :text)
                                              :length 500
                                              :see-more item-url)
                                    (html-text (getf data :text)))))
                              (unless (string= item-url (script-name*))
                                (str (activity-item-images images item-url "gift"))))
                   :reciprocity (when reciprocity
                                  (display-gratitude-reciprocities result)))))))


(defun gift-activity-item (result)
  (let* ((user-id (first (result-people result)))
         (item-id (result-id result))
         (data (db item-id)))
    (activity-item :id item-id
                   :url (strcat "/gifts/" item-id)
                   :class "gratitude"
                   :time (result-time result)
                   :hearts (length (loves item-id))
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

(defun inventory-activity-item (type result &key truncate show-distance show-what show-tags)
  (let* ((user-id (first (result-people result)))
         (self (eql user-id *userid*))
         (item-id (result-id result))
         (data (db item-id))
         (by (getf data :by))
         (group-adminp (group-admin-p by))
         (images (getf data :images))
         (item-url (strcat "/" type "s/" item-id))
         (tags (getf data :tags))) ;DJB

    (activity-item :id item-id
                   :url item-url
                   :time (result-time result)
                   :edit (or self group-adminp (getf *user* :admin))
                   :admin-delete (and (getf *user* :admin)
                                      (not (eql *userid* user-id)))
                   :image-text (when (and (string= type "offer") self)
                                 (if images
                                   "Add/remove photos"
                                   "Add photos"))
                   :class type
                   :reply (unless self t)
                   :hearts (length (loves item-id))
                   :type (unless show-what (cond ((getf data :edited) "edited")
                                                 ((string= type "request") "requested")
                                                 ((string= type "offer") "offered")))
                   :content (html
                              (:p
                                (str (person-link user-id))
                                (when show-what
                                  (str (if (getf data :edited) " edited " " posted "))
                                  (str (if (eq (getf data :type) :offer) "an " "a "))
                                  (htm (:a :href item-url
                                           (str type))))
                                (when show-distance
                                  (htm (:small
                                    " (within "
                                    (str
                                      (distance-string
                                        (air-distance (result-latitude result)
                                                      (result-longitude result)
                                                      *latitude*
                                                      *longitude*)))
                                    ")"))))
                              (:p
                                (str
                                  (if truncate
                                    (ellipsis (getf data :text)
                                              :length 500
                                              :see-more item-url)
                                    (html-text (getf data :text)))))
                              (when (and show-tags tags) 
                                (htm
                                  (:div :class "tags"
                                   "Tags:  "
                                   (str (display-tags type tags)))))
                              (unless (string= item-url (script-name*));image?
                                (str (activity-item-images images
                                                           item-url
                                                           type)))))))

(defun display-tags (type tags)
  (html
    (dolist (tag tags)
      (htm
        (:a :href (url-compose (strcat "/" type "s") "kw" tag) (str tag))
        (unless (eql tag (car (last tags)))
          (htm
            " &middot "))))))

(defun activity-item-images (images url alt)
  (html
    (:div :class "small-activity-image"
      (dolist (image-id images)
        (htm
          (:div :class "border"
           (:a :href url (:img :src (get-image-thumbnail image-id 70 70)
                               :alt alt))))))))

(defun activity-items (items &key (page 0) (count 20) (url "/home")
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
                       (str (gratitude-activity-item item :truncate t
                                                          :reciprocity
                                                          reciprocity))) 
                     (:gift
                       (str (gift-activity-item item)))
                     (:person
                       (str (joined-activity-item item)))
                     (:offer
                       (str (inventory-activity-item "offer" item :show-what t :show-distance location :show-tags show-tags :truncate t)))
                     (:request
                       (str (inventory-activity-item "request" item :show-what t :show-distance location :truncate t :show-tags show-tags)))))
                 (setf items (cdr items)))

                (t
                 (when (< (user-distance) 100)
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
                         (:a :style "float: right;" :href (strcat url "?p=" (+ page 1)) "next page >"))))))))))))

(defun local-activity-items (&key (page 0) (count 20) (url "/home") show-tags)
  (let ((distance (user-distance)))
    (if (= distance 0)
      (activity-items (sort (remove-private-items
                              (copy-list *recent-activity-index*))
                            #'>
                            :key #'result-time)
                      :page page
                      :count count
                      :reciprocity t
                      :url url
                      :show-tags show-tags)
      (let ((items (sort (geo-index-query *activity-geo-index*
                                          *latitude*
                                          *longitude*
                                          distance)
                       #'< :key #'activity-rank)))
        (activity-items items :page page 
                              :count count
                              :reciprocity t 
                              :url url
                              :show-tags show-tags)))))

