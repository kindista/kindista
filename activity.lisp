(in-package :kindista)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun timestamp (time)
  (html (:h3 :class "timestamp" :data-time time (str (humanize-universal-time time)))))

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

(defun activity-item (&key id url content time next-url hearts comments)
  (html
    (:div :class "item" :id id
      (str (timestamp time))
      (str content)
      (:div :class "actions"
        (str (love-button id url next-url))
        " &middot; "
        (str (comment-button url))
        " &middot; "
        (str (flag-button url))
        (str (activity-icons :hearts hearts :comments comments :url url))))))

(defun person-link (id)
  (html
    (:a :href (format nil "/people/~A" id) (str (getf (db id) :name)))))

(defun offer-activity-item (&key time user-name user-id offer-id next-url hearts comments text)
  (activity-item :id offer-id
                 :url (s+ "/offers/" offer-id)
                 :time time
                 :next-url next-url
                 :hearts hearts
                 :comments comments
                 :content (html
                            (:a :href (s+ "/people/" user-id) (str user-name))
                            " posted a "
                            (:a :href (s+ "/people/" user-id "/offers#" offer-id) "offer")
                            (:blockquote (str (second (multiple-value-list (markdown text :stream nil))))))))

(defun request-activity-item (&key time user-name user-id request-id next-url hearts comments text)
  (activity-item :id request-id
                 :url (s+ "/request/" request-id)
                 :time time
                 :next-url next-url
                 :hearts hearts
                 :comments comments
                 :content (html
                            (:a :href (s+ "/people/" user-id) (str user-name))
                            " posted a "
                            (:a :href (s+ "/people/" user-id "/requests#" request-id) "request")
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

(defun activity-items (&key (user *user*) (offset 0) (count 10) next-url)
  (let ((activity (geo-index-query (getf user :lat)
                                   (getf user :long)
                                   (or (getf user :distance) 50))))
    (html
      (dolist (item activity)
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
                                        :offer-id (write-to-string (fourth item))
                                        :user-name (getf (db userid) :name)
                                        :user-id (username-or-id userid)
                                        :next-url next-url
                                        :hearts (length (loves (fourth item)))
                                        :comments (length (comments (fourth item)))
                                        :text (getf (db (fourth item)) :text)))))
          (:request
            (let ((userid (getf (db (fourth item)) :by)))
              (str (request-activity-item :time (first item)
                                          :request-id (write-to-string (fourth item))
                                          :user-name (getf (db userid) :name)
                                          :user-id (username-or-id userid)
                                          :next-url next-url
                                          :hearts (length (loves (fourth item)))
                                          :comments (length (comments (fourth item)))
                                          :text (getf (db (fourth item)) :text))))))))))
