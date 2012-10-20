(in-package :kindista)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

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

(defun activity-item (&key url content time next-url hearts comments)
  (html
    (:div :class "item"
      (str (timestamp time))
      (str content)
      (:div :class "actions"
        (str (love-button url))
        " &middot; "
        (str (comment-button url))
        " &middot; "
        (str (flag-button url))
        (str (activity-icons :hearts hearts :comments comments :url (s+ url "/comments")))))))

(defun offer-activity-item (&key time user-name user-id offer-id next-url hearts comments text)
  (activity-item :url (s+ "/offers/" offer-id)
                 :time time
                 :next-url next-url
                 :hearts hearts
                 :comments comments
                 :content (html
                            (:a :href (s+ "/people/" user-id) (str user-name))
                            " posted a new "
                            (:a :href (s+ "/people/" user-id "/offers#" offer-id) "offer")
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
          )
        )
      
      )))
