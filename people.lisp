(in-package :kindista)

(defun create-person (&key name email password)
  (insert-db `(:type :person
               :name ,name
               :email ,email
               :help t
               :pass ,(new-password password)
               :created ,(get-universal-time))))

(defun index-person (id data)
  (setf (gethash (getf data :email) *email-index*) id)
  (setf (gethash (getf data :username) *username-index*) id)

  (metaphone-index-insert *metaphone-index* (getf data :name) id)

  (awhen (getf data :following)
    (with-locked-hash-table (*followers-index*)
      (dolist (person it)
        (push id (gethash person *followers-index*)))))

  (when (getf data :loves)
    (with-locked-hash-table (*love-index*)
      (dolist (item (getf data :loves))
        (unless (member id (gethash item *love-index*))
          (push id (gethash item *love-index*))))))

  (with-locked-hash-table (*activity-person-index*)
    (push (cons (getf data :created) id) (gethash id *activity-person-index*)))

  (when (and (getf data :lat) (getf data :long) (getf data :created))
    (people-geo-index-insert (getf data :lat) (getf data :long) id (getf data :created))
    (activity-geo-index-insert (getf data :lat)
                               (getf data :long)
                               id
                               (getf data :created)
                               (list id)))

  (timeline-insert id (getf data :created) id))

(defun username-or-id (&optional (id *userid*))
  (or (getf (db id) :username)
      (write-to-string id)))

(defun profile-activity-items (&key (userid *userid*) (page 0) (count 20) next-url type)
  (let ((items (sort (gethash userid *activity-person-index*) #'> :key #'car))
        (start (* page 20)))
    (html
      (iter (for i from 0 to (+ start count))
            (if (and (>= i start) items)
              (let* ((item (car items))
                     (obj (db (cdr item)))
                     (userid (getf obj :by)))
                (when (or (not type) (eql type (getf obj :type)))
                  (case (getf obj :type)
                    (:gratitude (str (gratitude-activity-item :time (first item)
                                                              :id (cdr item)
                                                              :next-url next-url
                                                              :text (getf (db (cdr item)) :text))))
                    (:person (str (joined-activity-item :time (first item)
                                                        :user-id (username-or-id (cdr item))
                                                        :user-name (getf (db (cdr item)) :name))))
                    (:offer
                      (str (offer-activity-item :time (first item)
                                                :offer-id (cdr item)
                                                :user-name (getf (db userid) :name)
                                                :user-id (username-or-id userid)
                                                :next-url next-url
                                                :hearts (length (loves (cdr item)))
                                                ;:comments (length (comments (cdr item)))
                                                :text (getf (db (cdr item)) :text))))
                    (:request
                      (str (request-activity-item :time (first item)
                                                  :request-id (cdr item)
                                                  :user-name (getf (db userid) :name)
                                                  :user-id (username-or-id userid)
                                                  :what t
                                                  :next-url next-url
                                                  :hearts (length (loves (cdr item)))
                                                  ;:comments (length (comments (cdr item)))
                                                  :text (getf (db (cdr item)) :text)))))))
              (finish))

            (setf items (cdr items))

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

(defun profile-activity-html (userid &key type)
  (let ((user (db userid))
        (strid (username-or-id userid)))
    (require-user
      (standard-page
        "Home"
        (html
          (:menu :class "bar"
            (:h3 :class "label" "Profile Menu")
            (if (not type)
              (htm (:li :class "selected" "Activity"))
              (htm (:li (:a :href (strcat "/people/" strid) "Activity"))))
            (if (eql type :offer)
              (htm (:li :class "selected" "Offers"))
              (htm (:li (:a :href (strcat "/people/" strid "/offers") "Offers"))))
            (if (eql type :request)
              (htm (:li :class "selected" "Requests"))
              (htm (:li (:a :href (strcat "/people/" strid "/requests") "Requests"))))
            (if (eql type :gratitude)
              (htm (:li :class "selected" "Reputation"))
              (htm (:li (:a :href (strcat "/people/" strid "/reputation") "Reputation")))))  

          (:div :class "activity"
            (str (profile-activity-items :userid userid :type type))))
        :top (html
               (:img :class "bigavatar" :src (strcat "/media/avatar/" userid ".jpg"))
               (:div :class "basics"
                 (:h1 (str (getf user :name)))
                 (:p :class "city" "Eugene, OR")
               (:form :method "GET" :action (strcat "/people/" strid "/message")
                 (:input :type "submit" :value "Send a message")) 
               (:form :method "POST" :action "/friends"
                 (:input :type "hidden" :name "add" :value strid)
                 (:input :type "hidden" :name "next" :value (strcat "/people/" strid))
                 (:input :type "submit" :value "Add as friend")))

               (:p :class "bio" (str (getf user :bio)))

               )
        :right (let ((mutuals (mutual-connections userid)))
                 (when (and mutuals (not (eql userid *userid*)))
                   (html
                     (:div :class "item people"
                      (:h3 "Mutual Friends")
                      (:ul
                        (dolist (id (mutual-connections userid))
                          (htm (:li (:a :href (strcat "/people/" (username-or-id id))
                                        (str (getf (db id) :name)))))))))))

        :selected "people"))))

(defroute "/people/<id>" (id)
  (:get
    (require-user
      (let ((is-number (scan +number-scanner+ id)))
        (if is-number
          (aif (db (parse-integer id))
            (aif (getf it :username)
              (see-other (strcat "/people/" it))
              (profile-activity-html (parse-integer id)))
            (not-found))
          (aif (gethash id *username-index*)
            (profile-activity-html it)
            (not-found)))))))

(defroute "/people/<id>/requests" (id)
  (:get
    (require-user
      (let ((is-number (scan +number-scanner+ id)))
        (if is-number
          (aif (db (parse-integer id))
            (aif (getf it :username)
              (see-other (strcat "/people/" it))
              (profile-activity-html (parse-integer id) :type :request))
            (not-found))
          (aif (gethash id *username-index*)
            (profile-activity-html it :type :request)
            (not-found)))))))


(defun nearby-people (&optional (userid *userid*))
  (let ((user (db userid)))
    (sublist (remove userid
                     (sort (geo-index-query *people-geo-index*
                                            (getf user :lat)
                                            (getf user :long)
                                            25
                                            :with-distance t)
                           #'< :key #'first)
                     :key #'fifth)
             0 10)))

(defun mutual-connections (one &optional (two *userid*))
  (intersection (gethash one *followers-index*)
                (getf (db two) :following)))

(defun suggested-people (&optional (userid *userid*))
  ; get nearby people
  ; get friends of friends
  ; get distance and number of mutuals for each
  (sort
    (let* ((user (db userid))
           (following (getf user :following)))
      (iter (for person in (remove userid
                                   (iter (for person in following)
                                         (reducing (getf (db person) :following)
                                                   by #'union))))
            (unless (member person following)
              (collect (cons (length (mutual-connections person userid)) person)))))
    #'> :key #'first))

(defun person-tile (id &key distance)
  (html
    (:div :class "person-tile"
      (:img :src (strcat "/media/avatars/" id ".jpg"))
      (:p (:a :href (strcat "/people/" (username-or-id id))
           (str (getf (db id) :name))))   
      ; distance
      (:p (acase (length (mutual-connections id))
            (0 (str "no mutual connections"))
            (1 (str "1 mutual connection"))
            (t (str (strcat it " mutual connections"))))))))

(defroute "/people" ()
  (:get
    (require-user
      (standard-page
        "People"
        (html
          ; favorites / connections?
          (:h2 "Nearby")
          (:div :class "person-row"
            (dolist (data (nearby-people))
              (str (person-tile (fifth data)))))
          (:h2 "People you may know")
          (:div :class "person-row"
            (dolist (data (suggested-people))
              (str (person-tile (cdr data))))))
        :selected "people"))))
