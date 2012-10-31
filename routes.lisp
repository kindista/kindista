(in-package :kindista)

(defroute "/" ()
  (:get
    (with-user
      (if *user*
        (see-other "/home")
        (base-page nil
                 (html
                   (:img :id "biglogo" :src "/media/biglogo.png")
                   (:form :method "POST" :action "/login" :id "login"
                     (awhen (get-parameter "retry")
                       (htm (:p :class "error" "The username or password was incorrect.")))
                     (awhen (get-parameter "next")
                       (htm (:input :type "hidden" :name "next" :value it)))
                     (:label :for "username" "Username or email")
                     (:input :type "text" :name "username")
                     (:label :for "password" "Password")
                     (:input :type "password" :name "password")
                     (:input :type "submit" :value "Log in")
                     (:p (:a :href "/reset" "Forgot your password?"))
                     (:p "New to Kindista?"
                      (:br)
                      (:a :href "/signup" "Create an account")))
                   (:div :id "about"
                    (:h2 "Co-creating a more beautiful world.")
                    (:p :class "big"
                      "Kindista is a new social network for seeing and appreciating the
                       creative potential in all people and supporting each other
                       in building the more beautiful world our hearts know is possible."))
                  (:p "Kindista &copy; 2012 &middot; "
                      (:a :href "/help" "Help") " &middot; "
                      (:a :href "/about" "About") " &middot; "
                      (:a :href "/blog" "Blog")
                      " &middot; Programmed in Common Lisp"))
                 :class "landing")))))

(defun signup-page (&key error name email password)
  (header-page
    "Sign up"
    nil
    (html
      (:h1 "Create an account")
      (when error
        (htm (:p :class "error" (str error))))
      (:form :method "POST" :action "/signup"
        (:div :id "login"
          (:label :for "name" "Full name")
          (:input :type "text" :name "name" :value name)
          (:label :for "email" "Email")
          (:input :type "email" :name "email" :value email)
          (:label :for "name" "Password")
          (:input :type "password" :name "password" :value password))
        (:input :type "submit" :value "Sign up"))

      (:p "Have an account? " (:a :href "/" "Sign in"))

      (:p :class "fineprint" "By creating an account, you are agreeing to our "
        (:a :href "/terms" "Terms of Service") " and " (:a :href "/privacy" "Privacy Policy"))

      (:menu :id "footer"
        (:li (:a :href "/help" "Help"))
        (:li (:a :href "/about" "About"))
        (:li (:a :href "/blog" "Blog"))
       )
      (:br)
      (:br)
      " ")
    :class "landing"))

(defroute "/signup" ()
  (:get
    (with-user
      (if *user*
        (see-other "/home")
        (signup-page))))
  (:post
    (with-user
      (if *user*
        (see-other "/home")
        (let ((userid (gethash (post-parameter "email") *email-index*)))
          (cond
            ((password-match-p userid (post-parameter "password"))

             (set-cookie "token" :value (make-token username)
                         :http-only t
                         :expires (+ (get-universal-time) 2592000)
                         :secure nil)
             (see-other "/home"))

            (userid
             (see-other (s+ "/forgot?email=" (post-parameter "email"))))

            ((not (and (< 0 (length (post-parameter "name")))
                       (< 0 (length (post-parameter "email")))
                       (< 0 (length (post-parameter "password")))))

             (signup-page :error "All fields are required"
                          :name (post-parameter "name")
                          :email (post-parameter "email")
                          :password (post-parameter "password")))

            ((> 7 (length (post-parameter "password")))

             (signup-page :error "Please use a strong password"
                          :name (post-parameter "name")
                          :email (post-parameter "email")))

            ((not (find #\Space (post-parameter "name")))

             (signup-page :error "Please use your full name"
                          :name (post-parameter "name")
                          :email (post-parameter "email")
                          :password (post-parameter "password")))

            (t
             (let ((id (create-person :name (post-parameter "name")
                                      :email (post-parameter "email")
                                      :password (post-parameter "password"))))
               (set-cookie "token"
                           :value (make-token id)
                           :http-only t
                           :expires (+ (get-universal-time) 2592000)
                           :secure nil))
             (see-other "/home"))))))))

(defroute "/logout" ()
  (:get
    (delete-token-cookie)
    (see-other "/")))

(defroute "/login" ()
  (:get
    (see-other "/"))
  (:post
    (let ((user (post-parameter "username"))
          (next (post-parameter "next")))
      (if (find #\@ user :test #'equal)
        (setf user (gethash user *email-index*))
        (setf user (gethash user *username-index*)))
      (cond
        ((password-match-p user (post-parameter "password"))
         (set-cookie "token"
                     :value (make-token user)
                     :http-only t
                     :expires (+ (get-universal-time) 2592000)
                     :secure nil)
         (setf (return-code*) +http-see-other+)
         (setf (header-out :location) (if (and (< 0 (length next))
                                               (equal #\/ (elt next 0)))
                                        next
                                        "/home"))
         "")
        (t
         (setf (return-code*) +http-see-other+)
         (setf (header-out :location) (if (and (< 0 (length next))
                                               (equal #\/ (elt next 0)))
                                        (s+ "/?retry&next=" (url-encode next))
                                        "/?retry"))
         "")))))

(defroute "/home" ()
  (:get
    (require-user
      (standard-page
        "Home"
        (html
          (:div :class "activity"
            (:div :class "item"
              (:menu :class "horiz"
                (:strong "create")
                (:li (:a :href "/testimonials/compose" "testimonial"))
                (:li (:a :href "/offers/compose" "offer"))
                (:li (:a :href "/requests/compose" "request"))
                (:li (:a :href "/events/compose" "event"))
                (:li (:a :href "/announcements/compose" "announcement"))
                )
              )
            (str (activity-items))))
        :selected "news"
        :right (html
                 (:div :class "item"
                   (:h2 "Upcoming Events")
                   (:menu
                     (:li "10/20 7:00PM " (:a :href "x" "East Eugene Gift Circle"))
                     (:li "10/24 7:00PM " (:a :href "x" "West Eugene Gift Circle")))))))))

(defroute "/messages" ()
  (:get
    (require-user
      (standard-page
        "Messages"
        (html
          (:h1 "Messages")
          (:h2 "Ideas")
          (:ul
            (:li "separate pages for message-only inbox and notification-only inbox?")
            ))
        :selected "messages"))))

(defroute "/people" ()
  (:get
    (require-user
      (standard-page
        "People"
        (html
          (:h1 "People")
          (:h2 "Ideas")
          (:ul
            (:li "suggested friends")
            (:li "nearby people, weight given to people w/ mutual friends")
            ))
        :selected "people"))))

(defroute "/offers" ()
  (:get
    (require-user
      (standard-page
        "Offers"
        (html
          (:h1 "Offers")
          (:h2 "Ideas")
          (:ul
            ))
        :selected "offers"))))

(defroute "/requests" ()
  (:get
    (require-user
      (standard-page
        "Requests"
        (html
          (:h1 "Requests")
          (:h2 "Ideas")
          (:ul
            ))
        :selected "requests"))))

(defroute "/events" ()
  (:get
    (require-user
      (standard-page
        "Events"
        (html
          (:h1 "Events")
          (:h2 "Ideas")
          (:ul
            (:li "create a new event")
            (:li "upcoming events")
            (:li "events friends are going to")
            ))
        :selected "events"))))

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
                :comments 7
                :text "[google](http://google.com) Saxophone lessons. I am **conservatory trained** (Bachelor of Music in Jazz Saxophone Performance from the CCM at University of Cincinnati). I have been playing for over 20 years, performing professionally in a reggae band (JohnStone Reggae in Washington DC and multiple jazz ensembles (currently StoneCold Jazz in Eugene.)"))
              (str (offer-activity-item
                :time (get-universal-time)
                :user-name "Benjamin Crandall"
                :user-id "ben"
                :offer-id "12345"
                :next-url "/home"
                :hearts 3
                :comments 7
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
