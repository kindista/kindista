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

  (when (getf data :loves)
    (with-locked-hash-table (*love-index*)
      (dolist (item (getf data :loves))
        (unless (member id (gethash item *love-index*))
          (push id (gethash item *love-index*))))))

  (when (and (getf data :lat) (getf data :long) (getf data :created))
    (activity-geo-index-insert (getf data :lat)
                               (getf data :long)
                               id
                               (getf data :created)
                               (list id)))

  (timeline-insert id (getf data :created) id))

(defun username-or-id (&optional (id *userid*))
  (or (getf (db id) :username)
      (write-to-string id)))

(defroute "/people/<name>" (name)
  (:get
    (require-user
      (standard-page
        "Home"
        (html
          (:div :class "profile"
            (:div :class "activity"
              (str (offer-activity-item
                :time (get-universal-time)
                :user-name "Benjamin Crandall"
                :user-id "ben"
                :offer-id "12345"
                :next-url "/home"
                :hearts 3
                :text "[google](http://google.com) Saxophone lessons. I am **conservatory trained** (Bachelor of Music in Jazz Saxophone Performance from the CCM at University of Cincinnati). I have been playing for over 20 years, performing professionally in a reggae band (JohnStone Reggae in Washington DC and multiple jazz ensembles (currently StoneCold Jazz in Eugene.)"))
              (str (offer-activity-item
                :time (get-universal-time)
                :user-name "Benjamin Crandall"
                :user-id "ben"
                :offer-id "12345"
                :next-url "/home"
                :hearts 3
                :text "[google](http://google.com) Saxophone lessons. I am **conservatory trained** (Bachelor of Music in Jazz Saxophone Performance from the CCM at University of Cincinnati). I have been playing for over 20 years, performing professionally in a reggae band (JohnStone Reggae in Washington DC and multiple jazz ensembles (currently StoneCold Jazz in Eugene.)")))
            ))
        :top (html
               (:div :class "profile"
               (:img :src "/media/oldeamon.jpg") 
               (:div :class "basics"
                 (:h1 "Eamon Walker")
                 (:p :class "city" "Eugene, OR")
               (:form :method "GET" :action "/people/eamon/message"
                 (:input :type "submit" :value "Send a message")) 
               (:form :method "POST" :action "/friends"
                 (:input :type "hidden" :name "add" :value "eamon")
                 (:input :type "hidden" :name "next" :value "/people/eamon")
                 (:input :type "submit" :value "Add as friend")))
               (:p :class "bio" "Kindista co-creator. I am committed to living fully in gift, which means that I don't charge for anything. If you appreciate what I do, please support me! xxxxxx")

               (:menu :class "bar"
                 (:h3 :class "label" "Profile Menu")
                 (:li :class "selected" "Activity")
                 (:li (:a :href "/people/eamon/resources" "Resources"))
                 (:li (:a :href "/people/eamon/testimonials" "Testimonials"))
                 (:li (:a :href "/people/eamon/blog" "Blog"))
                 (:li :class "notonly" (:a :href "/people/eamon/friends" "Mutual Friends")))))
        :right (html
                 (:div :class "item people"
                  (:h3 "Mutual Friends")
                  (:ul
                    (:li (:a :href "/people/eamon" "Eamon Walker"))
                    (:li (:a :href "/people/eamon" "Eamon Walker"))
                    (:li (:a :href "/people/eamon" "Eamon Walker"))
                    (:li (:a :href "/people/eamon" "Eamon Walker"))
                    (:li (:a :href "/people/eamon/friends" "and xx more")))))

        :selected "people"))))


(defroute "/people" ()
  (:get
    (require-user
      (standard-page
        "People"
        (html
          (:h2 "Favorites") ; or "Connections" if only a few
          (:div :class "person-row"
            (:div :class "person-tile"
              (:img :src "/media/oldeamon.jpg")
              (:p (:a :href "/people/root" "Nicholas E. Walker")) 
              (:p (:a :href "/people/root/mutual" "39 mutual connections")))
            (:div :class "person-tile"
              (:img :src "/media/oldeamon.jpg")
              (:p (:a :href "/people/root" "Nicholas E. Walker")) 
              (:p (:a :href "/people/root/mutual" "39 mutual connections"))) 
            (:div :class "person-tile"
              (:img :src "/media/oldeamon.jpg")
              (:p (:a :href "/people/root" "Nicholas E. Walker")) 
              (:p (:a :href "/people/root/mutual" "39 mutual connections"))) 
            (:div :class "person-tile"
              (:img :src "/media/oldeamon.jpg")
              (:p :class "name" (:a :href "/people/root" "Nicholas E. Walker")) 
              (:p :class "mutual" (:a :href "/people/root/mutual" "39 mutual connections"))) 
            (:div :class "person-tile"
              (:img :src "/media/oldeamon.jpg")
              (:p (:a :href "/people/root" "Nicholas E. Walker")) 
              (:p (:a :href "/people/root/mutual" "39 mutual connections"))) 
            (:div :class "person-tile"
              (:img :src "/media/oldeamon.jpg")
              (:p (:a :href "/people/root" "Nicholas E. Walker")) 
              (:p (:a :href "/people/root/mutual" "39 mutual connections")))) 
            (:h2 "Nearby")
            (:div :class "person-tile"
              (:img :src "/media/oldeamon.jpg")
              (:p (:a :href "/people/root" "Nicholas E. Walker")))
            (:div :class "person-tile"
              (:img :src "/media/oldeamon.jpg")
              (:p (:a :href "/people/root" "Nicholas E. Walker")))
            (:div :class "person-tile"
              (:img :src "/media/oldeamon.jpg")
              (:p (:a :href "/people/root" "Nicholas E. Walker")))
            (:div :class "person-tile"
              (:img :src "/media/oldeamon.jpg")
              (:p (:a :href "/people/root" "Nicholas E. Walker")))
          (:h2 "People you may know")
          (:div :class "person-tile"
            (:img :src "/media/oldeamon.jpg")
            (:p (:a :href "/people/root" "Nicholas E. Walker")))
          (:div :class "person-tile"
            (:img :src "/media/oldeamon.jpg")
            (:p (:a :href "/people/root" "Nicholas E. Walker")))
          (:div :class "person-tile"
            (:img :src "/media/oldeamon.jpg")
            (:p (:a :href "/people/root" "Nicholas E. Walker")))
          (:div :class "person-tile"
            (:img :src "/media/oldeamon.jpg")
            (:p (:a :href "/people/root" "Nicholas E. Walker")))
          )
        :selected "people"))))
