(in-package :kindista)

(defroute "/" ()
  (:get
    (with-user
      (if *user*
        (see-other "/home")
        (base-page "Welcome"
                 (html
                   (:img :id "biglogo" :src "/media/biglogo.png")
                   (:form :method "POST" :action "/login" :id "login"
                     (awhen (get-parameter "retry")
                       (htm (:p :class "error" "The email/username or password was incorrect.")
                            (unless (string= it "")
                                (htm (:p "Would you like to "
                                         (:a :href (s+ "/signup?email=" it) "create an account") "?")))))
                     (awhen (get-parameter "next")
                       (htm (:input :type "hidden" :name "next" :value it)))
                     (:label :for "username" "Username or email")
                     (:input :type "text" :name "username" :value (get-parameter "retry"))
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
                 :class "landing login")))))

(defun signup-page (&key error name email password)
  (header-page
    "Sign up"
    nil
    (html
      (:form :method "POST" :action "/signup" :id "signup"
        (:h2 "Create an account")
        (when error
          (htm (:p :class "error" (str error))))
        (:label :for "name" "Full name")
        (:input :type "text" :name "name" :value name)
        (:label :for "email" "Email")
        (:input :type "email" :name "email" :value (or email (get-parameter "email")))
        (:label :for "name" "Password")
        (:input :type "password" :name "password" :value password)
        (:input :type "submit" :value "Sign up")

      (:p "Have an account? " (:a :href "/" "Sign in"))

      (:p :class "fineprint" "By creating an account, you are agreeing to our "
        (:a :href "/terms" "Terms of Service") " and " (:a :href "/privacy" "Privacy Policy"))) 

      (:p "Kindista &copy; 2012 &middot; "
          (:a :href "/help" "Help") " &middot; "
          (:a :href "/about" "About") " &middot; "
          (:a :href "/blog" "Blog")
          " &middot; Programmed in Common Lisp")
      (:br)
      (:br)
      " ")
    :class "landing signup"))

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
        ((scan +email-scanner+ (post-parameter "username"))
         (setf (return-code*) +http-see-other+)
         (setf (header-out :location) (if (and (< 0 (length next))
                                               (equal #\/ (elt next 0)))
                                        (s+ "/?retry=" (post-parameter "username") "&next=" (url-encode next))
                                        (s+ "/?retry=" (post-parameter "username"))))
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
      (cond
        ((getf *user* :location)
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
           :selected "home"
           :top (when (getf *user* :help)
                  (welcome-bar
                    (html
                      (:h2 "Getting started")
                      (:p "We're so happy to have you join us! Here are some things you can do to get started:")
                      (:ul
                        (unless (getf *user* :avatar)
                          (htm (:li (:a :href "/avatar" "Upload a picture") " so that other people can recognize you.")))
                        (:li (:a :href "/testimonials/compose" "Write a testimonial") " about someone who has affected your life.")
                        (:li (:a :href "/connections/new" "Make a connection") " to say that you know someone.")
                        (:li (:a :href "/requests/compose" "Post a request") " to the community for something you need.")
                        )
                      (:p "On this page you can see what's going on around you and with people you have made
                           a connection with. Use the menu "
                          (:span :class "menu-button" " (click the button on the header) ")
                          (:span :class "menu-showing" " on the left ")
                          " to explore the site."))))
           :right (html
                    (:div :class "item"
                      (:h2 "Upcoming Events")
                      (:menu
                        (:li (:strong "Thursday, November 23"))
                        (:li "7:00PM " (:a :href "x" "East Eugene Gift Circle"))
                        (:li (:strong "Thursday, November 30"))
                        (:li "7:00PM " (:a :href "x" "West Eugene Gift Circle")))))))
        ((and (getf *user* :lat)
              (getf *user* :long))

         (standard-page
           "Welcome"
           (html
             (:div :class "item"
               (:div :class "setup"
                 (:h2 "Verify your location")
                 (:p "We will never share your exact location with anyone else.
                      If you would like to know more about how we use the information you share with us,
                      please read our " (:a :href "/privacy" "privacy policy") ".")
                 (str (static-google-map :size "280x150" :zoom 12 :lat (getf *user* :lat) :long (getf *user* :long)))

                 (:form :method "post" :action "/settings"
                   (:h3 "Is this location correct?")
                   (:button :class "yes" :type "submit" :name "confirm-location" :value "1" "Yes, this is correct")
                   (:button :class "no" :type "submit" :name "reset-location" :value "1" "No, go back")))))
           :selected "home"))
        (t
         (standard-page
           "Welcome"
           (html
             (:div :class "item"
               (:div :class "setup"
                 (:h2 "Welcome to Kindista!")
                 (:p "Kindista is a social network for " (:strong "building and supporting real community") ".
                      We use your location to help you find " (:strong "local people, resources, and events") ".
                      To get started, we need to know where you call home.")
                 (:p "We will never share your exact location with anyone else.
                      If you would like to know more about how we use the information you share with us,
                      please read our " (:a :href "/privacy" "privacy policy") ".")
                 (:h2 "Where do you call home?")
                 (:p 
                   (:small
                     "Enter a street address and click \"Next\". We'll show you a map to confirm the location."))
                 (:form :method "post" :action "/settings"
                   (:input :type "hidden" :name "next" :value "/home")
                   (:input :type "text" :name "address" :placeholder "1600 Pennsylvania Avenue NW, Washington, DC")
                   (:input :type "submit" :value "Next")))))
           :selected "home"))))))

(defroute "/settings" ()
  (:post
    (require-user
      (cond
        ((post-parameter "address")
         (multiple-value-bind (lat long address city state country)
             (geocode-address (post-parameter "address"))
           (declare (ignore city state country))
           (modify-db *userid* :lat lat :long long :address address)
           (see-other (or (post-parameter "next") "/home"))))
        ((post-parameter "reset-location")
         (modify-db *userid* :lat nil :long nil :address nil :location nil)
         (see-other (or (post-parameter "next") "/home")))
        ((equalp (post-parameter "help") "0")
         (modify-db *userid* :help nil)
         (see-other (or (referer) "/home")))
        ((equalp (post-parameter "help") "1")
         (modify-db *userid* :help t)
         (see-other (or (referer) "/home")))
        ((and (post-parameter "confirm-location")
              (getf *user* :lat)
              (getf *user* :long))
         (modify-db *userid* :location t)
         (see-other (or (post-parameter "next") "/home")))
        (t
         (flash "Sorry, couldn't make sense of that request to update your settings.")
         (see-other "/home"))))))


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

