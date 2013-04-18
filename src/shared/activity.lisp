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

(defun love-button (id url &optional next-url)
  (html
    (:form :method "POST" :action url
      (when next-url
        (htm (:input :type "hidden" :name "next" :value next-url)))
      (if (member *userid* (gethash id *love-index*))
        (htm (:input :type "submit" :name "unlove" :value "Loved"))
        (htm (:input :type "submit" :name "love" :value "Love"))))))

(defun comment-button (url)
  (html
    (:form :method "GET" :action url
      (:input :type "submit" :value "Discuss"))))

(defun flag-button (url &optional next-url)
  (html
    (:form :method "GET" :action (s+ url "/flag")
      (:input :type "hidden" :name "next" :value next-url)
      (:input :type "submit" :value "Flag"))))

(defun edit-button (url)
  (html
    (:form :method "GET" :action (s+ url "/edit")
      (:input :type "submit" :value "Edit"))))

(defun delete-button (url)
  (html
    (:form :method "POST" :action (s+ url "/edit")
      (:input :type "submit" :name "delete" :value "Delete"))))


(defun activity-icons (&key url hearts comments)
  (html
    (:a :class "icons" :href url
      (when hearts
        (htm
          (:img :alt "love:" :src "/media/icons/heart16.png") 
          ;(:span :class "unicon" "♥ ")
          (:span (str hearts)))) 
      (when comments
        (htm
          (:img :alt "comments:" :src "/media/icons/comment16.png") 
          ;(:span :class "unicon" " ✎ ")
          (:span (str comments)))))))

(defun activity-item (&key id url content time next-url hearts comments type distance edit user-id)
  (html
    (:div :class "card" :id id
      ;(:img :src (strcat "/media/avatar/" user-id ".jpg"))
      (str (timestamp time :type type))
      (when distance
        (htm
          (:p :class "distance"
            "within " (str (distance-string distance)))))
      (str content)
      (:div :class "actions"
        (str (activity-icons :hearts hearts :comments comments :url url))    
        (str (love-button id url next-url))
        (when edit
          (htm
            " &middot; "  
            (str (edit-button url))  
            " &middot; "  
            (str (delete-button url))))
        (when comments
          (htm
            " &middot; "
            (str (comment-button url))))
        (unless (eql user-id *userid*)
          (htm
            " &middot; "
            (str (flag-button url))))))))

(defun gratitude-activity-item (result &key next-url)
  (let* ((user-id (first (result-people result)))
         (item-id (result-id result))
         (data (db item-id)))
    (activity-item :id item-id
                   :user-id user-id
                   :url (strcat "/gratitude/" item-id)
                   :time (result-time result)
                   :next-url next-url
                   :edit (when (eql user-id *userid*) t)
                   :hearts (length (loves item-id))
                   ;:comments (length (comments item-id))
                   :content (html
                              (:p (str (person-link user-id))
                                  (str (if (getf data :editied) " edited " " shared ")) 
                                  (:a :href (strcat "/gratitude/" item-id) "gratitude") 
                                  " for "
                                  (fmt "~{~A~^, ~}"
                                      (iter (for subject in (getf data :subjects))
                                            (collect (person-link subject)))))
                              (:p (cl-who:esc (getf data :text)))))))

(defun gift-activity-item (result &key next-url)
  (let* ((user-id (first (result-people result)))
         (item-id (result-id result))
         (data (db item-id)))
    (activity-item :id item-id
                   :user-id user-id
                   :url (strcat "/gift/" item-id)
                   :time (result-time result)
                   :next-url next-url
                   :hearts (length (loves item-id))
                   :comments (length (comments item-id))
                   :content (html
                              (:p (str (person-link user-id))
                                  " was "
                                  (:a :href (strcat "/gifts/" item-id) "acknowledged") 
                                  " by "
                                  (str (name-list-all (getf data :recipients))))
                              (:p (cl-who:esc (getf data :text)))))))

(defun joined-activity-item (result)
  (html
    (:div :class "card"
      (str (timestamp (result-time result)))
      (:p (str (person-link (first (result-people result)))) " joined Kindista"))))

(defun inventory-activity-item (type result &key show-distance show-what next-url)
  (let ((user-id (first (result-people result)))
        (data (db (result-id result))))
    (activity-item :id (result-id result)
                   :user-id user-id
                   :url (strcat "/" type "s/" (result-id result))
                   :time (result-time result)
                   :next-url next-url
                   :edit (when (eql user-id *userid*) t)
                   :hearts (length (loves (result-id result)))
                   :type (unless show-what (cond ((getf data :edited) "edited")
                                                 ((string= type "request") "requested")
                                                 ((string= type "offer") "offered")))
                   :content (html
                              (:p
                                (str (person-link user-id))
                                (when show-what
                                  (str (if (getf data :edited) " edited a " " posted a "))
                                  (htm (:a :href (str (format nil (s+ "/" type "s/~d")
                                                              (result-id result)))
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
                              (:p (cl-who:esc (getf data :text)))))))

(defun activity-items (&key (user *user*) (page 0) (count 20) next-url)
  (with-location
    (let ((items (sort (geo-index-query *activity-geo-index*
                                        *latitude*
                                        *longitude*
                                        (or (getf user :distance) 50))
                       #'< :key #'activity-rank))
          (start (* page 20)))
      (html
        (iter (for i from 0 to (+ start count))
              (cond
                ((< i start)
                 (setf items (cdr items)))

                ((and (>= i start) items)
                 (let* ((item (car items)))
                   (case (result-type item)
                     (:gratitude
                       (str (gratitude-activity-item item :next-url next-url)))
                     (:gift
                       (str (gift-activity-item item :next-url next-url)))
                     (:person
                       (str (joined-activity-item item)))
                     (:offer
                       (str (inventory-activity-item "offer" item :show-what t :next-url next-url)))
                     (:request
                       (str (inventory-activity-item "request" item :show-what t :next-url next-url)))))
                 (setf items (cdr items)))

                (t
                 (when (< (user-distance) 100)
                   (htm
                     (:div :class "item small"
                      (:em "Increasing the ")(:strong "show activity within")(:em " distance may yield more results."))))
                 (finish)))

              (finally
                (when (or (> page 0) (cdr items))
                  (htm
                    (:div :class "item"
                     (when (> page 0)
                       (htm
                         (:a :href (strcat "/home?p=" (- page 1)) "< previous page")))
                     "&nbsp;"
                     (when (cdr items)
                       (htm
                         (:a :style "float: right;" :href (strcat "/home?p=" (+ page 1)) "next page >"))))))))))))
