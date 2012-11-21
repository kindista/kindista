(in-package :kindista)

(defun create-request (&key (by ruserid*) text)
  (insert-db (list :type :request
                         :by by
                         :text text
                         :created (get-universal-time))))

(defun index-request (id data)
  (with-locked-hash-table (*request-index*)
    (setf (gethash (getf data :by) *request-index*)
          (union (gethash (getf data :by) *request-index*)
                 (list id))))
  (geo-index-insert (or (getf data :lat)
                        (getf (db (getf data :by)) :lat))
                    (or (getf data :long)
                        (getf (db (getf data :by)) :long))
                    id
                    (getf data :created)))

(defun request-compose (&key text)
  (standard-page
   "Post a request"
   (html
     (:div :class "item"
      (:h2 "Post a request"))
     (:div :class "item"
      (:form :method "post" :action "/requets/post"
        (:textarea :rows "8" :name "text" (str text))
        (:p  (:button :class "no" :type "submit" :class "cancel" :name "cancel" "Cancel")
        (:button :class "yes" :type "submit" :class "submit" :name "create" "Create")))))
   :selected "people"))

(defroute "/requests/compose" ()
  (:get
    (require-user
      (request-compose)))
  (:post
    (require-user
      (cond
        ((post-parameter "cancel")
         (see-other (or (post-parameter "next") "/home")))
        ((post-parameter "create")
         (let ((subjects (parse-subject-list (post-parameter "subject") :remove (write-to-string *userid*))))
           (cond
             ((post-parameter "text")
              (see-other (format nil "/requests/~A"
                                     (create-request :text (post-parameter "text")))))
             (t
              "totally blank"))))
        (t
         (request-compose
           :text (post-parameter "text")
           :subjects (parse-subject-list
                       (post-parameter "subject")
                       :remove (post-parameter "remove"))))))))

(defroute "/requests/<int:id>" (id)
  (:get
    (setf id (parse-integer id))
    (aif (db id)
      (require-user
        (standard-page
          "First few words... | Kindista"
          (html
            (str (request-activity-item :time (getf it :created)
                                        :request-id id
                                        :user-name (getf (db (getf it :by)) :name)
                                        :user-id (username-or-id (getf it :by))
                                        :hearts (length (loves id))
                                        :comments (length (comments id))
                                        :text (getf it :text))))))
      (standard-page "Not found" "not found")))
  (:post
    (require-user
      (setf id (parse-integer id)) 
      (aif (db id)
        (cond
          ((and (post-parameter "love")
                (member (getf it :type) '(:gratitude :offer :request)))
           (love id)
           (see-other (or (post-parameter "next") (referer))))
          ((and (post-parameter "unlove")
                (member (getf it :type) '(:gratitude :offer :request)))
           (unlove id)
           (see-other (or (post-parameter "next") (referer)))))
        (standard-page "Not found" "not found")))))

(defroute "/requests" ()
  (:get
    (require-user
      (standard-page
        "Requests"
        (html
          (:div :class "categories"
            (:h2 "browse categories")
            (:div :class "category"
              (:h3 "activities")
              (:a :href "/categories/classes" "classes") ", "
              (:a :href "/categories/groups" "groups") ", "
              (:a :href "/categories/recreation" "recreation") ", "
              (:a :href "/categories/fitness" "fitness") ", "
              (:a :href "/categories/performances" "performances"))
            (:div :class "category"
              (:h3 "caregiving")
              (:a :href "/" "children") ", "
              (:a :href "/" "elders") ", "
              (:a :href "/" "medical") ", "
              (:a :href "/" "alternative") ", "
              (:a :href "/" "counseling") ", "
              (:a :href "/" "physical therapy"))
            (:div :class "category"
              (:h3 "animals")
              (:a :href "/" "pet sitting") ", "
              (:a :href "/" "health") ", "
              (:a :href "/" "food") ", "
              (:a :href "/" "supplies") ", "
              (:a :href "/" "other"))
            (:div :class "category"
              (:h3 "clothing")
              (:a :href "/" "women's") ", "
              (:a :href "/" "men's") ", "
              (:a :href "/" "children's") ", "
              (:a :href "/" "services"))
            (:div :class "category"
              (:h3 "media services")
              (:a :href "/" "audio") ", "
              (:a :href "/" "video") ", "
              (:a :href "/" "photography") ", "
              (:a :href "/" "print") ", "
              (:a :href "/" "web"))
            (:div :class "category"
              (:h3 "supplies")
              (:a :href "/categories/household" "household") ", "
              (:a :href "/categories/office" "office") ", "
              (:a :href "/categories/beauty" "beauty") ", "
              (:a :href "/categories/crafts" "crafts"))
            (:div :class "category"
              (:h3 "expertise")
              (:a :href "/categories/design" "design") ", "
              (:a :href "/categories/legal" "legal/mediation") ", "
              (:a :href "/categories/financial" "financial") ", "
              (:a :href "/categories/computer" "computer") ", "
              (:a :href "/categories/communication" "communication"))
            (:div :class "category"
              (:h3 "volunteering")
              (:a :href "/categories/classes" "event") ", "
              (:a :href "/categories/recreation" "short-term") ", "
              (:a :href "/categories/fitness" "long-term"))
            (:div :class "category"
              (:h3 "transportation")
              (:a :href "/categories/rides" "rides") ", "
              (:a :href "/categories/bike" "bicycle") ", "
              (:a :href "/categories/cars" "automotive") ", "
              (:a :href "/categories/train" "train") ", "
              (:a :href "/categories/air" "air"))
            (:div :class "category"
              (:h3 "electronics")
              (:a :href "/categories/computer" "computer") ", "
              (:a :href "/categories/mobile" "mobile") ", "
              (:a :href "/categories/entertainment" "entertainment") ", "
              (:a :href "/categories/other" "other"))
            (:div :class "category"
              (:h3 "tools/equipment")
              (:a :href "/categories/kitchen" "kitchen") ", "
              (:a :href "/categories/games" "games") ", "
              (:a :href "/categories/garden" "farm/garden") ", "
              (:a :href "/categories/marine" "marine"))
            (:div :class "category"
              (:h3 "food") 
              (:a :href "/" "meals") ", "
              (:a :href "/" "produce") ", "
              (:a :href "/" "staples") ", "
              (:a :href "/" "preparation") ", "
              (:a :href "/" "misc")) 
            )
          (:ul
            ))
        :selected "requests"))))
