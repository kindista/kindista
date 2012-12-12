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

(defun activity-item (&key id url content time next-url hearts comments type)
  (html
    (:div :class "item" :id id
      (str (timestamp time :type type :url url))
      (str content)
      (:div :class "actions"
        (str (love-button id url next-url))
        (when comments
          (htm
            " &middot; "
            (str (comment-button url))))
        " &middot; "
        (str (flag-button url))
        (str (activity-icons :hearts hearts :comments comments :url url))))))

(defun person-link (id)
  (html
    (:a :href (strcat "/people/" (username-or-id id)) (str (getf (db id) :name)))))

(defun offer-activity-item (&key time user-name user-id offer-id next-url hearts text)
  (activity-item :id offer-id
                 :url (strcat "/offers/" offer-id)
                 :time time
                 :next-url next-url
                 :hearts hearts
                 :content (html
                            (:a :href (strcat "/people/" user-id) (str user-name))
                            " posted a "
                            (:a :href (strcat "/people/" user-id "/offers#" offer-id) "offer")
                            (:blockquote (str (second (multiple-value-list (markdown text :stream nil))))))))

(defun request-activity-item (&key time user-name user-id request-id next-url hearts text what)
  (activity-item :id request-id
                 :url (strcat "/requests/" request-id)
                 :time time
                 :next-url next-url
                 :hearts hearts
                 :type (unless what "requested")
                 :content (html
                            (:a :href (strcat "/people/" user-id) (str user-name))
                            (when what
                              (htm
                                " posted a "
                                (:a :href (strcat "/requests/" request-id) "request")))
                            (:blockquote (str (second (multiple-value-list (markdown text :stream nil))))))))

(defun gratitude-activity-item (&key time id next-url text)
  (activity-item :id id
                 :url (strcat "/gratitude/" id)
                 :time time
                 :next-url next-url
                 :hearts (length (loves id))
                 :comments (length (comments id))
                 :content (html
                            (str (person-link (getf (db id) :author)))
                            " shared "
                            (:a :href (strcat "/gratitude/" id) "gratitude")
                            " for "
                            (fmt "~{~A~^, ~}"
                                 (iter (for subject in (getf (db id) :subjects))
                                       (collect (person-link subject))))
                            (:blockquote (str (second (multiple-value-list (markdown text :stream nil))))))))

(defun joined-activity-item (&key time user-name user-id)
  (html
    (:div :class "item"
      (str (timestamp time))
      (:a :href (s+ "/people/" user-id) (str user-name)) " joined Kindista")))

(defun activity-items (&key (user *user*) (page 0) (count 20) next-url)
  (let ((items (sort (geo-index-query *activity-geo-index*
                                      (getf user :lat)
                                      (getf user :long)
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
                 (case (getf (db (fourth item)) :type)
                   (:gratitude (str (gratitude-activity-item :time (first item)
                                                             :id (fourth item)
                                                             :next-url next-url
                                                             :text (getf (db (fourth item)) :text))))
                   (:person (str (joined-activity-item :time (first item)
                                                       :user-id (username-or-id (fourth item))
                                                       :user-name (getf (db (fourth item)) :name))))
                   (:offer
                     (let ((userid (getf (db (fourth item)) :by)))
                       (str (offer-activity-item :time (first item)
                                                 :offer-id (fourth item)
                                                 :user-name (getf (db userid) :name)
                                                 :user-id (username-or-id userid)
                                                 :next-url next-url
                                                 :hearts (length (loves (fourth item)))
                                                 ;:comments (length (comments (fourth item)))
                                                 :text (getf (db (fourth item)) :text)))))
                   (:request
                     (let ((userid (getf (db (fourth item)) :by)))
                       (str (request-activity-item :time (first item)
                                                   :request-id (fourth item)
                                                   :user-name (getf (db userid) :name)
                                                   :user-id (username-or-id userid)
                                                   :what t
                                                   :next-url next-url
                                                   :hearts (length (loves (fourth item)))
                                                   ;:comments (length (comments (fourth item)))
                                                   :text (getf (db (fourth item)) :text)))))))
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
                       (:a :style "float: right;" :href (strcat "/home?p=" (+ page 1)) "next page >")))))))))))
