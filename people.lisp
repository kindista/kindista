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

  (with-locked-hash-table (*followers-index*)
    (dolist (following (getf data :following))
      (setf (gethash following *followers-index*)
            (union (gethash following *followers-index*)
                   (list id)))))

  (when (getf data :loves)
    (with-locked-hash-table (*love-index*)
      (dolist (item (getf data :loves))
        (setf (gethash item *love-index*)
              (cons id (gethash item *love-index*))))))

  (when (and (getf data :lat) (getf data :long) (getf data :created))
    (geo-index-insert (getf data :lat) (getf data :long) id (getf data :created)))

  (timeline-insert id (getf data :created) id))

(defun username-or-id (&optional (id *userid*))
  (or (getf (db id) :username)
      (write-to-string id)))

(defun loves (id)
  (gethash id *love-index*))

(defun follow (id &key (userid *userid*))
  (with-locked-hash-table (*db*)
    (let* ((user (db userid))
           (following (getf user :following)))
      (unless (member id following)
        (setf (getf user :following) (cons id following))
        (update-db userid user))))

  (with-locked-hash-table (*followers-index*)
    (let* ((followers (gethash id *followers-index*)))
      (unless (member userid followers)
        (setf (gethash id *followers-index*) (cons userid followers))))))

(defun unfollow (id &key (userid *userid*))
  (with-locked-hash-table (*db*)
    (let* ((user (db userid))
           (following (getf user :following)))
      (when (member id following)
        (setf (getf user :following) (remove id following))
        (update-db userid user))))

  (with-locked-hash-table (*love-index*)
    (let* ((followers (gethash id *love-index*)))
      (when (member userid followers)
        (setf (gethash id *love-index*) (remove userid followers))))))

(defun profile-page (id content &key right (data (db id)) (selected :activity))
  (standard-page
    (strcat (getf data :name) " | Kindista")
    content
    :top (let ((username (username-or-id id)))
           (html
             (:div :class "profile"
               (when (eql id *userid*)
                 (htm (:div :class "welcome item"
                        "This is your profile.")))
               (:img :src (strcat "/media/avatar/" id ".jpg"))
               (:div :class "basics"
                 (:h1 (str (getf data :name)))
                 (:p :class "city" "Eugene, OR")
               (:form :method "GET" :action (strcat "/people/" username "/message")
                 (:input :type "submit" :value "Send a message")) 
               (:form :method "POST" :action "/friends"
                 (:input :type "hidden" :name "add" :value username)
                 (:input :type "hidden" :name "next" :value (strcat "/people/" username))
                 (:input :type "submit" :value "Add as friend")))

               (awhen (getf data :bio-html)
                 (htm (:p :class "bio" (str it))))

               (:menu :class "bar"
                 (:h3 :class "label" "Profile Menu")
                 (:li :class (when (eql selected :activity) "selected")
                   (:a :href (strcat "/people/" username) "Activity"))
                 (:li :class (when (eql selected :requests) "selected")
                   (:a :href (strcat "/people/" username "/requests") "Requests"))
                 (:li :class (when (eql selected :testimonials) "selected")
                   (:a :href (strcat "/people/" username "/testimonials") "Testimonials"))
                 (:li :class (when (eql selected :posts) "selected")
                   (:a :href (strcat "/people/" username "/posts") "Posts"))
                 (:li :class (if (eql selected :connections) "selected notonly" "notonly")
                   (:a :href (strcat "/people/" username "/connections") "Connections"))))))

      :right (let ((mutuals (intersection (getf *user* :following)
                                          (gethash id *followers-index*))))
               (html
                 (:div :class "item people"
                   (:h3 "Mutual Connections")
                   (:ul
                     (unless mutuals
                       (htm (:li "No mutual connections")))
                     (dolist (person mutuals)
                       (htm (:li (:a :href (strcat "/people/" (username-or-id person))
                                     (getf (db person) :name)))))))))

    :selected "people"))

(defroute "/people/<id>" (id)
  (:get
    (if (parse-integer id :junk-allowed t)
      (setf id (parse-integer id))
      (setf id (gethash id *username-index*)))
    (aif (db id)
      (require-user
        (profile-page
          id
          (html
            (dolist (request (gethash id *request-index*))
              (let ((item (db request)))
                (str (request-activity-item :time (getf item :created)
                                            :request-id (write-to-string request)
                                            :user-name (getf it :name)
                                            :user-id (write-to-string id)
                                            :hearts (length (loves request))
                                            :comments (length (comments request))
                                            :text (getf item :text))))))
          :selected :activity
          :data it))

        (not-found))))

(defroute "/people/<id>/requests" (id)
  (:get
    (if (parse-integer id :junk-allowed t)
      (setf id (parse-integer id))
      (setf id (gethash id *username-index*)))
    (aif (db id)
      (require-user
        (profile-page
          id
          (html
            (dolist (request (gethash id *request-index*))
              (let ((item (db request)))
                (str (request-activity-item :time (getf item :created)
                                            :request-id (write-to-string request)
                                            :user-name (getf it :name)
                                            :user-id (write-to-string id)
                                            :hearts (length (loves request))
                                            :comments (length (comments request))
                                            :text (getf item :text))))))
          :selected :requests
          :data it))

        (not-found))))
