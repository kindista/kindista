(in-package :kindista)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun timestamp (time &key type url)
  (let ((inner (html
                 (when type
                   (htm (str type) " "))
                 (str (humanize-universal-time time)))))
    (html
      (:h3 :class "timestamp" :data-time time :data-type type
        (if url
          (htm (:a :href url (str inner)))
          (str inner))))))

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
          (str hearts))) 
      (when comments
        (htm
          (:img :alt "comments:" :src "/media/icons/comment16.png") 
          ;(:span :class "unicon" " ✎ ")
          (str comments))))))

(defun activity-item (&key id url content time next-url hearts comments type distance edit user-id)
  (html
    (:div :class "item" :id id
      (str (timestamp time :type type :url url))
      (when distance
        (htm
          (:div :class "distance"
            "within " (str (distance-string distance)))))
      (str content)
      (:div :class "actions"
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
            (str (flag-button url))))
        (str (activity-icons :hearts hearts :comments comments :url url))))))

(defun person-link (id)
  (html
    (:a :href (s+ "/people/" (username-or-id id)) (str (getf (db id) :name)))))

(defun gratitude-activity-item (result &key next-url)
  (let* ((user-id (first (result-people result)))
         (item-id (result-id result))
         (data (db item-id)))
    (activity-item :id item-id
                   :user-id user-id
                   :url (strcat "/gratitude/" item-id)
                   :time (result-created result)
                   :next-url next-url
                   :edit (when (eql user-id *userid*) t)
                   :hearts (length (loves item-id))
                   :comments (length (comments item-id))
                   :content (html
                              (str (person-link user-id))
                              (str (if (getf data :editied) "edited" " shared "))
                              (:a :href (strcat "/gratitude/" id) "gratitude")
                              " for "
                              (fmt "~{~A~^, ~}"
                                   (iter (for subject in (getf data :subjects))
                                         (collect (person-link subject))))
                              (:blockquote (cl-who:esc (getf data :text)))))))

(defun joined-activity-item (result)
  (html
    (:div :class "item"
      (str (timestamp (result-created result)))
      (str (person-link (result-people result))) " joined Kindista")))

(defun offer-activity-item (result &key show-distance show-what next-url)
  (let ((user-id (first (result-people result)))
        (data (db (result-id result))))
    (activity-item :id (result-id result)
                   :user-id user-id
                   :url (strcat "/offers/" (result-id result))
                   :time (result-created result)
                   :distance (when show-distance
                               (air-distance (result-latitude result)
                                             (result-longitude result)
                                             (getf *user* :lat)
                                             (getf *user* :long)))
                   :next-url next-url
                   :edit (when (eql user-id *userid*) t)
                   :hearts (length (loves (result-id result)))
                   :type (unless show-what (if (getf data :edited) "edited" "offered"))
                   :content (html
                              (str (person-link user-id))
                              (when show-what
                                (htm
                                  (str (if (getf data :edited) " edited a " " posted a "))
                                  (:a :href (format nil "/offers/~d" (result-id result)) "offer")))
                              (:blockquote (cl-who:esc (getf data :text)))))))

(defun request-activity-item (result &key show-distance show-what next-url)
  (let ((user-id (first (result-people result)))
        (data (db (result-id result))))
    (activity-item :id (result-id result)
                   :user-id user-id
                   :url (strcat "/requests/" (result-id result))
                   :time (result-created result)
                   :distance (when show-distance
                               (air-distance (result-latitude result)
                                             (result-longitude result)
                                             *latitude*
                                             *longitude*))
                   :next-url next-url
                   :edit (when (eql user-id *userid*) t)
                   :hearts (length (loves (result-id result)))
                   :type (unless show-what (if (getf data :edited) "edited" "requested"))
                   :content (html
                              (str (person-link user-id))
                              (when show-what
                                (htm
                                  (str (if (getf data :edited) " edited a " " posted a "))
                                  (:a :href (format nil "/requests/~d" (result-id result)) "request")))
                              (:blockquote (cl-who:esc (getf data :text)))))))


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
                     (:person
                       (str (joined-activity-item item)))
                     (:offer
                       (str (offer-activity-item item :next-url next-url)))
                     (:request
                       (str (request-activity-item item :show-what t :next-url next-url)))))
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
