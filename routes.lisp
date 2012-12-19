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
         (multiple-value-bind (lat long address city state country street zip)
             (geocode-address (post-parameter "address"))
           (declare (ignore country))
           (modify-db *userid* :lat lat :long long :address address :city city :state state :street street :zip zip)
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

(defun donate-monthly-1 ()
  (html
    (:form :id "donate" :method "post" :action "/donate"
      (:input :type "hidden" :name "type" :value "monthly")
      (:h2 "Sign up to make a monthly contribution")
      (:button :type "submit" :name "amount" :value "5" "$5")
      (:button :type "submit" :name "amount" :value "10" "$10")
      (:button :type "submit" :name "amount" :value "20" "$20")
      (:button :type "submit" :name "amount" :value "35" "$35")
      (:button :type "submit" :name "amount" :value "50" "$50")
      (:button :type "submit" :name "amount" :value "100" "$100")
      (:button :type "submit" :name "amount" :value "250" "$250")
      (:button :type "submit" :name "amount" :value "other" "Other")

      (:h3 (:a :href "/donate/once" "Or, make a one-time donation"))

      (:p "We do not store your credit card information, and we have a really good " (:a :href "/privacy" "privacy policy") ".")
      (:p "For information on other ways to donate, " (:a :href "/donate/more" "click here") "."))))

(defun donate-once-1 ()
  (html
    (:form :id "donate" :method "post" :action "/donate"
      (:input :type "hidden" :name "type" :value "once")
      (:h2 "Make a one-time donation supporting Kindista")
      (:button :type "submit" :name "amount" :value "10" "$10")
      (:button :type "submit" :name "amount" :value "20" "$20")
      (:button :type "submit" :name "amount" :value "25" "$25")
      (:button :type "submit" :name "amount" :value "35" "$35")
      (:button :type "submit" :name "amount" :value "50" "$50")
      (:button :type "submit" :name "amount" :value "100" "$100")
      (:button :type "submit" :name "amount" :value "250" "$250")
      (:button :type "submit" :name "amount" :value "other" "Other")

      (:h3 (:a :href "/donate/once" "Or, make a one-time donation"))

      (:p "We do not store your credit card information, and we have a really good " (:a :href "/privacy" "privacy policy") ".")
      (:p "For information on other ways to donate, " (:a :href "/donate/more" "click here") "."))))

(defun donate-dialog-2 ()
  (with-user
    (let ((name (split " " (getf *user* :name))))
      (html
        (:form :id "donate" :method "post" :action "/donate"
          (:input :type "hidden" :name "amount" :value (post-parameter "amount"))
          (:input :type "hidden" :name "type" :value (post-parameter "type"))
          (:h2 "Step 2/4")
          (:h3 "Billing address")
          (:ul
            (:li :class "half"
              (:label :for "first" "*First name")
              (:input :name "first" :type "text" :value (first name)))
            (:li :class "half"
              (:label :for "last" "*Last name")
              (:input :name "last" :type "text" :value (first (last name))))
            (:li :class "full"
              (:label :for "address" "*Address")
              (:input :name "address" :type "text" :value (getf *user* :street)))
            (:li :class "half"
              (:label :for "city" "*City")
              (:input :name "city" :type "text" :value (getf *user* :city)))
            (:li :class "quarter"
              (:label :for "state" "*State")
              (:select :name "state" (str (state-options (getf *user* :state)))))
            (:li :class "quarter"
              (:label :for "zip" "*Zip")
              (:input :name "zip" :type "text" :value (getf *user* :zip)))
            (:li :class "half"
              (:label :for "email" "*Email")
              (:input :name "email" :type "text" :value (getf *user* :email)))
            (:li :class "half"
              (:label :for "phone" "*Phone number")
              (:input :name "phone" :type "text")))
          (:button :class "nav" :type "submit" "Next >")

          (:p "We do not store your credit card information, and we have a really good " (:a :href "/privacy" "privacy policy") ".")
          (:p "For information on other ways to donate, " (:a :href "/donate/more" "click here") "."))))))
    
(defun donate-page (dialog)
  (base-page
    "Donate"
    (html
      (:img :src "/media/biglogo.png")
      (str dialog)
      (:div :id "letter"
        (:h2 "From Kindista co-founder Benjamin Crandall")
        (:p "Google might have close to a million servers. Yahoo has something like 12,000 staff. We have about 800 servers and 150 employees.")
        (:p "Wikipedia is the #5 site on the web and serves 482 million different people every month – with billions of page views.")
        (:p "Commerce is fine. Advertising is not evil. But it doesn't belong here. Not in Wikipedia.")
        (:p "Wikipedia is something special. It is like a library or a public park. It is like a temple for the mind. It is a place we can all go to think, to learn, to share our knowledge with others.")
        (:p "When I founded Wikipedia, I could have made it into a for-profit company with advertising banners, but I decided to do something different. We’ve worked hard over the years to keep it lean and tight. We fulfill our mission, and leave waste to others.")
        (:p "If everyone reading this donated $5, we would only have to fundraise for one day a year. But not everyone can or will donate. And that's fine. Each year just enough people decide to give.")
        (:p "This year, please consider making a donation of $5, $20, $50 or whatever you can to protect and sustain Wikipedia.")
        (:p "Thanks,")
        (:p (:strong "Jimmy Wales"))
        (:p "Wikipedia Founder")))
    :class "donate"))


(defroute "/donate" ()
  (:get
    (base-page
      "Donate"
      (donate-page (donate-dialog-1))))
  (:post
    (base-page
      "Donate"
      (donate-page (donate-dialog-2)))))

(defroute "/donate/once" ()
  (:get
    (base-page
      "Donate"
      (donate-page (donate-dialog-1))))
  (:post
    (base-page
      "Donate"
      (donate-page (donate-dialog-2)))))
