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

(defun profile-tabs-html (userid &key type)
  (let ((strid (username-or-id userid))
        (bio (getf (db userid) :bio)))
    (html
      (:menu :class "bar"
        (:h3 :class "label" "Profile Menu")
        (when bio
          (if (not type)
            (htm (:li :class "selected" "About"))
            (htm (:li (:a :href *base-url* "About")))))
        (if (eql type :activity)
          (htm (:li :class "selected" "Activity"))
          (htm (:li (:a :href (if bio (strcat *base-url* "/activity") *base-url*) "Activity"))))
        (if (eql type :gratitude)
          (htm (:li :class "selected" "Reputation"))
          (htm (:li (:a :href (strcat *base-url* "/reputation") "Reputation"))))  
        (if (eql type :offer)
          (htm (:li :class "selected" "Offers"))
          (htm (:li (:a :href (strcat *base-url* "/offers") "Offers"))))
        (if (eql type :request)
          (htm (:li :class "selected" "Requests"))
          (htm (:li (:a :href (strcat *base-url* "/requests") "Requests"))))))))

(defun profile-bio-section-html (title content &key editing editable section-name)
  (when (string= content "")
    (setf content nil))

  (when (or content editing editable)
    (html
      (:div :class "bio-section"
        (:h2 (str title)
             (when (and editable (not editing))
               (htm (:a :href (strcat *base-url* "?edit=" section-name)
                        (:img :class "icon" :src "/media/icons/pencil.png")))))
        (cond
          (editing
            (htm
              (:form :method "post" :action "/settings"
                (:input :type "hidden" :name "next" :value *base-url*)
                (:textarea :name (strcat "bio-" section-name) (str content))
                (:button :class "yes" :type "submit" :name "save" "Save")
                (:a :class "cancel" :href *base-url* "Cancel")
                ))) 
          ((not content)
            (htm
              (:p :class "empty" "I'm empty... fill me out!")))

          (t
            (htm
              (:p (str content)))))))))

(defun profile-bio-html (userid &key editing)
  ; is the user editing one of the sections?
  ; should we show edit links for the sections?
  ;  if the user is looking at their own bio
  ;   and they are not currently editing another section
  ;
  (require-user
    (let* ((strid (username-or-id userid))
           (editable (when (not editing) (eql userid *userid*)))
           (user (db userid))
           (*base-url* (strcat "/people/" strid)))
      (standard-page
        (getf (db userid) :name)
        (html
          (str (profile-tabs-html userid))
          (if (or (eql userid *userid*) (getf user :bio))
            (htm
              (:div :class "bio"
                (str (profile-bio-section-html
                       "My self-summary"
                       (getf user :bio-summary)
                       :section-name "summary"
                       :editing (eql editing 'summary)
                       :editable editable))  
                (str (profile-bio-section-html
                       "What I'm doing with my life"
                       (getf user :bio-doing)
                       :section-name "doing"
                       :editing (eql editing 'doing)
                       :editable editable))  
                (str (profile-bio-section-html
                       "What I'm really good at"
                       (getf user :bio-skills)
                       :section-name "skills"
                       :editing (eql editing 'skills)
                       :editable editable))
                (str (profile-bio-section-html
                       "I'm also into"
                       (getf user :bio-into)
                       :section-name "into"
                       :editing (eql editing 'into)
                       :editable editable))
                (str (profile-bio-section-html
                       "You should contact me if"
                       (getf user :bio-contact)
                       :section-name "contact"
                       :editing (eql editing 'contact)
                       :editable editable))))
            (htm (:h3 "This person hasn't written anything here."))))
        :right (when editing (html (:h2 "Style guide")))
        :top (profile-top-html userid)))))

(defun profile-top-html (userid)
  (let ((user (db userid))
        (is-friend (member userid (getf *user* :following))))
    (html
     (:img :class "bigavatar" :src (strcat "/media/avatar/" userid ".jpg"))
     (:div :class "basics"
       (:h1 (str (getf user :name)))
       (:p :class "city" "Eugene, OR")
     (:form :method "GET" :action (strcat *base-url* "/message")
       (:input :type "submit" :value "Send a message")) 
     (:form :method "POST" :action "/friends"
       (:input :type "hidden" :name (if is-friend "remove" "add") :value userid)
       (:input :type "hidden" :name "next" :value *base-url*)
       (:input :class (when is-friend "cancel") :type "submit" :value (if is-friend "Remove friend" "Add as friend")))))))

(defun profile-activity-html (userid &key type)
  (let* ((user (db userid))
         (strid (username-or-id userid))
         (*base-url* (strcat "/people/" strid)))
    (require-user
      (standard-page
        "Home"

        (html
          (str (profile-tabs-html userid :type (or type :activity)))
          (:div :class "activity"
            (str (profile-activity-items :userid userid :type type))))

        :top (profile-top-html userid)

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

(defmacro ensuring-userid ((user-id base-url) &body body)
  (let ((is-number (gensym))
        (user-name (gensym)) 
        (user-data (gensym)))
    `(let ((,is-number (scan +number-scanner+ ,user-id)))
       (if ,is-number
         (let* ((,user-id (parse-integer ,user-id))
                (,user-data (db ,user-id))
                (,user-name (getf ,user-data :username)))
           (if ,user-data
             (if ,user-name
               (see-other (format nil ,base-url ,user-name))
               (progn ,@body))
             (not-found)))
         (let ((,user-id (gethash ,user-id *username-index*)))
           (if ,user-id
             (progn ,@body)
             (not-found)))))))

(defroute "/people/<id>" (id)
  (:get
    (require-user
      (ensuring-userid (id "/people/~a")
        (let ((editing (get-parameter "edit")))
          (cond
            ((or (not editing)
                 (not (eql id *userid*)))
             (if (getf (db id) :bio)
               (profile-bio-html id)
               (profile-activity-html id)))

            ((string= editing "doing")
             (profile-bio-html id :editing 'doing))

            ((string= editing "contact")
             (profile-bio-html id :editing 'contact))

            ((string= editing "into")
             (profile-bio-html id :editing 'into))

            ((string= editing "summary")
             (profile-bio-html id :editing 'summary))

            ((string= editing "skills")
             (profile-bio-html id :editing 'skills))
            
            (t (not-found))))))))

(defroute "/people/<id>/activity" (id)
  (:get
    (require-user
      (ensuring-userid (id "/people/~a/activity")
        (profile-activity-html id)))))

(defroute "/people/<id>/requests" (id)
  (:get
    (require-user
      (ensuring-userid (id "/people/~a/requests")
        (profile-activity-html id :type :request)))))


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
