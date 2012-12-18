(in-package :kindista)

(defroute "/" ()
  (:get
    (with-user
      (if *user*
        (see-other "/home")
        (standard-page "Welcome"
                 (html
                   (:img :id "logo" :src "/media/logo.png")
                   (:form :method "POST" :action "/login" :id "login"
                     (awhen (get-parameter "retry")
                       (htm (:p :class "error" "The email/username or password was incorrect.")
                            (unless (string= it "")
                                (htm (:p (:a :href (s+ "/signup?email=" it)
                                             "Would you like to create an account?"))))))
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
                    (:h2 "Uncovering a wealth of human connection.")
                    (:p :class "big"
                      "Kindista is a new social network for seeing and appreciating the
                       creative potential in all people and supporting each other
                       in building the more beautiful world our hearts know is possible."))
                  (:p "Kindista &copy; 2012 &middot; "
                      (:a :href "/help" "Help") " &middot; "
                      (:a :href "/about" "About") " &middot; "
                      (:a :href "/blog" "Blog")
                      " &middot; Programmed in Common Lisp"))
                      )))))

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
               (:menu :class "horiz"
                 (:strong "actions")
                 (:li (:a :href "/gratitude/new" "express gratitude"))
                 (:li (:a :href "/offers/new" "make offer")
                      " / "
                      (:a :href "/requests/new" "request"))
                 ;(:li (:a :href "/events/new" "event"))
                 (:li (:a :href "/announcements/new" "post announcement"))
                 )
                 
             (:form :class "item" :method "post" :action "/settings"
               (:strong :style "font-size: 1.2em;" "showing activity within ")
               (:input :type "hidden" :name "next" :value "/home")
               (let ((distance (user-distance)))
                 (htm
                   (:select :name "distance" :onchange "this.form.submit()"
                     (:option :value "2" :selected (when (eql distance 2) "") "2 miles")
                     (:option :value "5" :selected (when (eql distance 5) "") "5 miles")
                     (:option :value "10" :selected (when (eql distance 10) "") "10 miles")
                     (:option :value "25" :selected (when (eql distance 25) "") "25 miles")
                     (:option :value "100" :selected (when (eql distance 100) "") "100 miles"))))
               " "
               (:input :type "submit" :class "no-js" :value "apply"))
               (let ((page (if (scan +number-scanner+ (get-parameter "p"))
                            (parse-integer (get-parameter "p"))
                            0)))
                (str (activity-items :page page)))))
           :selected "home"
           :top (when (getf *user* :help)
                  (welcome-bar
                    (html
                      (:h2 "Getting started")
                      (:p "We're so happy to have you join us! Here are some things you can do to get started:")
                      (:ul
                        (unless (getf *user* :avatar)
                          (htm (:li (:a :href "/avatar" "Upload a picture") " so that other people can recognize you.")))
                        (:li (:a :href "/gratitude/new" "Express gratitude") " for someone who has affected your life.")
                        (:li (:a :href "/people/new" "Make a connection") " to say that you know someone.")
                        (:li (:a :href "/requests/new" "Post a request") " to the community for something you need.")
                        )
                      (:p "On this page you can see what's going on around you and with people you have made
                           a connection with. Use the menu "
                          (:span :class "menu-button" " (click the button on the header) ")
                          (:span :class "menu-showing" " on the left ")
                          " to explore the site."))))
           :right (html
;                    (:div :class "item"
;                      (:h2 "Upcoming Events")
;                      (:menu
;                        (:li (:strong "Thursday, November 23"))
;                        (:li "7:00PM " (:a :href "x" "East Eugene Gift Circle"))
;                        (:li (:strong "Thursday, November 30"))
;                        (:li "7:00PM " (:a :href "x" "West Eugene Gift Circle"))))   
                    #|
                    (:div :style "padding-bottom: 1em;
                                  border-bottom: 1px solid #ddd;
                                  margin-bottom: 1em;"
                      (:div :style "font-size: 2.5em; font-weight: bold" "2,000")
                      (:div :style "font-size: 1em; font-weight: bold" "monthly donations")
                      (:div :style "font-size: 2.5em; font-weight: bold" "$999,999")
                      (:div :style "font-size: 1em; font-weight: bold" "per month of $99,999,999 " (:a :href "/donate" "goal"))
                      (:button :style "font-size: 1.5em;
                                       font-weight: bold;
                                       background: #375497;
                                       color: #fff;
                                       border: 0;
                                       border-radius: 4px;
                                       margin-top: 0.5em;
                                       padding: 0.25em 0.6em" "Donate to Kindista")
                      (:div :style "font-size: 0.9em; margin-top: 1em;" (:a :href "#" "How does Kindista use the money?"))
                      )
                      |#
                      
                    (:h2 "People you may know")
                    (:menu
                      (dolist (data (suggested-people))
                        (htm
                          (:li (:a :href (strcat "/people/" (username-or-id (cdr data)))
                                   (str (getf (db (cdr data)) :name))))))))))
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

(defroute "/friends" ()
  (:get
    ":-)")
  (:post
    (require-user
      (let ((friends (getf *user* :following)))
        (cond
          ((scan +number-scanner+ (post-parameter "add"))
           (let ((id (parse-integer (post-parameter "add"))))
             (unless (member id friends)
               (modify-db *userid* :following (cons id friends))))
           (see-other (or (post-parameter "next") "/home")))

          ((scan +number-scanner+ (post-parameter "remove"))
           (let ((id (parse-integer (post-parameter "remove"))))
             (when (member id friends)
               (modify-db *userid* :following (remove id friends))))
           (see-other (or (post-parameter "next") "/home")))

          (t
           (flash "Sorry, couldn't make sense of that request.")
           (see-other "/home")))))))

(defroute "/settings" ()
  (:get
    ":-)")
  (:post
    (require-user
      (cond
        ((post-parameter "bio-doing")
         (unless (getf *user* :bio)
           (modify-db *userid* :bio t))
         (modify-db *userid* :bio-doing (post-parameter "bio-doing"))
         (see-other (or (post-parameter "next") "/home")))
        ((post-parameter "bio-summary")
         (unless (getf *user* :bio)
           (modify-db *userid* :bio t))
         (modify-db *userid* :bio-summary (post-parameter "bio-summary"))
         (see-other (or (post-parameter "next") "/home")))
        ((post-parameter "bio-into")
         (unless (getf *user* :bio)
           (modify-db *userid* :bio t))
         (modify-db *userid* :bio-into (post-parameter "bio-into"))
         (see-other (or (post-parameter "next") "/home")))
        ((post-parameter "bio-contact")
         (unless (getf *user* :bio)
           (modify-db *userid* :bio t))
         (modify-db *userid* :bio-contact (post-parameter "bio-contact"))
         (see-other (or (post-parameter "next") "/home")))
        ((post-parameter "bio-skills")
         (unless (getf *user* :bio)
           (modify-db *userid* :bio t))
         (modify-db *userid* :bio-skills (post-parameter "bio-skills"))
         (see-other (or (post-parameter "next") "/home")))

        ((post-parameter "address")
         (multiple-value-bind (lat long address city state)
             (geocode-address (post-parameter "address"))
           (modify-db *userid* :lat lat :long long :address address :city (format nil "~a, ~a" city state))
           (see-other (or (post-parameter "next") "/home"))))

        ((post-parameter "reset-location")
         (modify-db *userid* :lat nil :long nil :address nil :location nil)
         (see-other (or (post-parameter "next") "/home")))

        ((scan +number-scanner+ (post-parameter "rdist"))
         (modify-db *userid* :rdist (parse-integer (post-parameter "rdist")))
         (flash "Your search distance for offers and requests has been changed!")
         (see-other (or (post-parameter "next") "/requests")))

        ((scan +number-scanner+ (post-parameter "sdist"))
         (modify-db *userid* :rdist (parse-integer (post-parameter "sdist")))
         (flash "Your default search distance has been changed!")
         (see-other (or (post-parameter "next") "/requests")))

        ((scan +number-scanner+ (post-parameter "distance"))
         (modify-db *userid* :distance (parse-integer (post-parameter "distance")))
         (flash (format nil "Now showing activity within ~a miles." (post-parameter "distance")))
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


(defun percent ()
  95/100)

(defroute "/fundbar.png" ()
  (:get
    (setf (content-type*) "image/png")
    (let ((out (send-headers)))
      (vecto:with-canvas (:width 320 :height 34)
        (vecto:set-font (vecto:get-font "/usr/share/fonts/TTF/Ubuntu-B.ttf")
                        12)
        (vecto:set-rgb-fill 255/255 255/255 255/255)
        (vecto:draw-string 0 20 "$99,999/$999,999 monthly goal")
        (vecto:draw-string 245 20 "Donate Now")
        (vecto:set-rgb-fill 55/255 84/255 151/255)
        (vecto:set-rgb-fill 90/255 90/255 90/255)
        (vecto:set-rgb-stroke 130/255 130/255 130/255)
        (vecto:set-line-width 2)
        (vecto:rounded-rectangle 3 3 316 12 6 6)
        (vecto:fill-and-stroke)
        (vecto:rounded-rectangle 2 2 (* 320 (max 4/100 (percent))) 14 7 7)
        (vecto:set-rgb-fill 55/255 84/255 151/255)
        (vecto:set-rgb-stroke 96/255 119/255 171/255)
        (vecto:fill-and-stroke)
        (vecto:save-png-stream out)))))
