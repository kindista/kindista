(in-package :kindista)

(defun create-person (&key name email password)
  (insert-db `(:type :person
               :name ,name
               :email ,email
               :help t
               :pass ,(new-password password)
               :created ,(get-universal-time))))

(defun index-person (id data)
  (let ((result (make-result :id id
                             :latitude (getf data :lat)
                             :longitude (getf data :long)
                             :type :person
                             :people (list id)
                             :created (getf data :created))))

    (setf (gethash (getf data :email) *email-index*) id)
    (setf (gethash (getf data :username) *username-index*) id)

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
      (asetf (gethash id *activity-person-index*)
             (sort (push result it) #'> :key #'result-created)))

    (when (and (getf data :lat) (getf data :long) (getf data :created))
      (metaphone-index-insert *metaphone-index* (getf data :name) result)
      (geo-index-insert *people-geo-index* result)
      (geo-index-insert *activity-geo-index* result))))

(defun username-or-id (&optional (id *userid*))
  (or (getf (db id) :username)
      (write-to-string id)))

(defun profile-activity-items (&key (userid *userid*) (page 0) (count 20) next-url type)
  (let ((items (gethash userid *activity-person-index*))
        (start (* page 20)))
    (html
      (iter 
        (for i from 0 to (+ start count))
          (if (and (>= i start) items)
            (let ((item (car items)))
              (when (or (not type) (eql type (result-type item)))
                (case (result-type item)
                  (:gratitude 
                    (str (unless (and (eql type :gratitude) 
                                      (eql userid (first (result-people item))))
                           (gratitude-activity-item item
                                                    :next-url next-url))))
                  (:person 
                    (str (joined-activity-item item)))
                  (:offer
                    (str (offer-activity-item item
                                              :show-what (unless (eql type :offer) t)
                                              :next-url next-url)))   
                  (:request
                    (str (request-activity-item item
                                                :show-what (unless (eql type :request) t)
                                                :next-url next-url))))))
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

(defun profile-tabs-html (userid &key tab)
  (let* ((bio (getf (db userid) :bio))
         (self (eql userid *userid*))
         (show-bio-tab (or bio self)))
    (html
      (:menu :class "bar"
        (:h3 :class "label" "Profile Menu")
        (when show-bio-tab
          (if (and self (eql tab :about))
            (htm (:li :class "selected" "About"))
            (htm (:li (:a :href (strcat *base-url* "/about") "About")))))

        (if (eql tab :activity)
          (htm (:li :class "selected" "Activity"))
          (htm (:li (:a :href (strcat *base-url* "/activity") "Activity"))))
        (if (eql tab :gratitude)
          (htm (:li :class "selected" "Reputation"))
          (htm (:li (:a :href (strcat *base-url* "/reputation") "Reputation"))))  
        (if (eql tab :offer)
          (htm (:li :class "selected" "Offers"))
          (htm (:li (:a :href (strcat *base-url* "/offers") "Offers"))))
        (if (eql tab :request)
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
          (str (profile-tabs-html userid :tab :about))
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
        :top (profile-top-html userid)
        :selected "people"))))

(defun profile-top-html (userid)
  (let ((user (db userid))
        (is-friend (member userid (getf *user* :following))))
    (html
     (:img :class "bigavatar" :src (strcat "/media/avatar/" userid ".jpg"))
     (:div :class "basics"
       (:h1 (str (getf user :name)))
       (:p :class "city" "Eugene, OR")
     (unless (eql userid *userid*)
       (htm 
         (:form :method "GET" :action (strcat *base-url* "/message")
           (:input :type "submit" :value "Send a message")) 
         (:form :method "POST" :action "/friends"
           (:input :type "hidden" :name (if is-friend "remove" "add") :value userid)
           (:input :type "hidden" :name "next" :value *base-url*)
           (:input :class (when is-friend "cancel") :type "submit" :value (if is-friend "Remove friend" "Add as friend")))))))))

(defun profile-activity-html (userid &key type)
  (let* ((user (db userid))
         (strid (username-or-id userid))
         (*base-url* (strcat "/people/" strid)))
    (require-user
      (standard-page
        "Home"
        (html
          (str (profile-tabs-html userid :tab (or type :activity)))
          (when (and (eql type :request) (eql userid *userid*))
            (htm
              (:div :class "item"
                (:h4 "post a request") 
                (:form :method "post" :action "/requests/new"
                  (:table :class "post"
                    (:tr
                      (:td (:textarea :cols "1000" :rows "4" :name "text"))
                      (:td
                        (:button :class "yes" :type "submit" :class "submit" :name "next" "Post"))))))))
          (when (and (eql type :gratitude) (not (eql userid *userid*))
            (htm
              (:div :class "item"
               (:h4 "Do you have gratitude to share about " (str (getf user :name)) "?")
               (:form :method "post" :action "/gratitude/new"
                 (:input :type "hidden" :name "subject" :value userid)
                 (:input :type "hidden" :name "next" :value (strcat *base-url* "/reputation"))
                 (:table :class "post"
                  (:tr
                    (:td (:textarea :cols "1000" :rows "4" :name "text"))
                    (:td
                      (:button :class "yes" :type "submit" :class "submit" :name "create" "Post")))))))))
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
    (with-user
      (ensuring-userid (id "/people/~a")
        (let ((editing (get-parameter "edit"))
              (bio (getf (db id) :bio)))
          (cond
           ;((or (not editing)
           ;     (not (eql id *userid*)))
           ; (if (getf (db id) :bio)
           ;   (profile-bio-html id)
           ;   (profile-activity-html id)))

            ((not (eql id *userid*))
             (profile-activity-html id))

            ((not editing)
             (profile-bio-html id))

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

(defroute "/people/<id>/about" (id)
  (:get
    (require-user
      (ensuring-userid (id "/people/~a/about")
        (profile-bio-html id)))))

(defroute "/people/<id>/activity" (id)
  (:get
    (with-user
      (ensuring-userid (id "/people/~a/activity")
        (profile-activity-html id)))))

(defroute "/people/<id>/reputation" (id)
  (:get
    (require-user
      (ensuring-userid (id "/people/~a/reputation")
        (profile-activity-html id :type :gratitude)))))

(defroute "/people/<id>/offers" (id)
  (:get
    (require-user
      (ensuring-userid (id "/people/~a/offers")
        (profile-activity-html id :type :offer)))))

(defroute "/people/<id>/requests" (id)
  (:get
    (require-user
      (ensuring-userid (id "/people/~a/requests")
        (profile-activity-html id :type :request)))))


(defun nearby-people (&optional (userid *userid*))
  (let* ((user (db userid)))
    (labels ((distance (result)
               (air-distance *latitude* *longitude* (result-latitude result) (result-longitude result))))
      (sublist (remove userid
                 (sort (geo-index-query *people-geo-index* *latitude* *longitude* 25)
                       #'< :key #'distance)
                 :key #'result-id)
             0 10))))

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

(defun person-tile (id &key distance show-city)
  (let ((data (db id)))
    (html
      (:div :class "person-tile"
        (:img :src (strcat "/media/avatar/" id ".jpg"))
        (:p (:a :href (strcat "/people/" (username-or-id id))
             (str (getf data :name))))   
        ; distance
        (when show-city
          (htm
            (:p :class "city" (str (getf data :city)))))
        (acase (length (mutual-connections id))
          (0)
          (1 (htm (:p "1 mutual connection")))
          (t (htm (:p (str (strcat it " mutual connections"))))))))))

(defroute "/people" ()
  (:get
    (with-user
      (standard-page
        "People"
        (html
          ; favorites / connections?
          (:h2 "People near you")
          (:div :class "person-row"
            (with-location
              (dolist (data (nearby-people))
                (str (person-tile (result-id data))))))
          (when *user* 
            (htm
              (:h2 "People you may know") 
              (:div :class "person-row"
                (dolist (data (suggested-people))
                  (str (person-tile (cdr data))))))))
          :selected "people"))))
