(in-package :kindista)

(defroute "/" () 
  (:get
    (see-other "/home"))) 
;    (with-user
;      (if *user*
;        (base-page "Welcome"
;                 (html
;                   (:img :id "logo" :src "/media/logo.png")
;                   (:form :method "POST" :action "/login" :id "login"
;                     (awhen (get-parameter "retry")
;                       (htm (:p :class "error" "The email/username or password was incorrect.")
;                            (unless (string= it "")
;                                (htm (:p (:a :href (s+ "/signup?email=" it)
;                                             "Would you like to create an account?"))))))
;                     (awhen (get-parameter "next")
;                       (htm (:input :type "hidden" :name "next" :value it)))
;                     (:label :for "username" "Username or email")
;                     (:input :type "text" :name "username" :value (get-parameter "retry"))
;                     (:label :for "password" "Password")
;                     (:input :type "password" :name "password")
;                     (:input :type "submit" :value "Log in")
;                     (:p (:a :href "/reset" "Forgot your password?"))
;                     (:p "New to Kindista?"
;                      (:br)
;                      (:a :href "/signup" "Create an account")))
;                   (:div :id "about"
;                    (:h2 "Uncovering a wealth of human connection.")
;                    (:p :class "big"
;                      "Kindista is a new social network for seeing and appreciating the
;                       creative potential in all people and supporting each other
;                       in building the more beautiful world our hearts know is possible."))
;                  (:p "Kindista &copy; 2012 &middot; "
;                      (:a :href "/help" "Help") " &middot; "
;                      (:a :href "/about" "About") " &middot; "
;                      (:a :href "/blog" "Blog")
;                      " &middot; Programmed in Common Lisp"))
;                      ))))

(defun signup-page (&key error name email password)
  (standard-page
    "Sign up"
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

        (:p "Have an account? " (:a :href "/login" "Log in")) 

        (:p :class "fineprint" "By creating an account, you are agreeing to our "
          (:a :href "/terms" "Terms of Service") " and " (:a :href "/privacy" "Privacy Policy"))))
    :top (when (get-parameter "action")
           (welcome-bar
             (html
               "If you want to do more than browse and search Kindista, you'll need to "
               (:a :href "/signup" "create an account") ". Or, " (:a :href "/login" "log in")
               " if you've already got one!")
             nil))))

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

             (setf (token-userid *token*) username)

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
             (setf (token-userid *token*) (create-person :name (post-parameter "name")
                                                         :email (post-parameter "email")
                                                         :password (post-parameter "password")))
             (see-other "/home"))))))))

(defroute "/logout" ()
  (:get
    (delete-token-cookie)
    (see-other "/")))

(defroute "/login" ()
  (:get
    (with-user
      (if *user*
        (see-other "/home")
        (standard-page
          "Login"
          (html
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
               (:a :href "/signup" "Create an account"))))))))

  (:post
    (with-token
      (let ((user (post-parameter "username"))
            (next (post-parameter "next")))
        (if (find #\@ user :test #'equal)
          (setf user (gethash user *email-index*))
          (setf user (gethash user *username-index*)))
        (cond
          ((password-match-p user (post-parameter "password"))
           (setf (token-userid *token*) user)
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
           ""))))))


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

      (:h3 (:a :href "/donate" "Or, make a monthly donation"))

      (:p "We do not store your credit card information, and we have a really good " (:a :href "/privacy" "privacy policy") ".")
      (:p "For information on other ways to donate, " (:a :href "/donate/more" "click here") "."))))

(defun donate-dialog-2 (&optional show-error)
  (with-donate-info
    (html
      (:form :id "donate" :method "post" :action "/donate"
        (:h2 "Step 2/4")
        (:h3 "Billing address")
        (:ul
          (:li :class "full"
            (:label :class (when (and show-error
                                      (empty-string-p (donate-info-name*)))
                             "error")
                    :for "name" "*Name on card")
            (:input :name "name" :type "text" :value (donate-info-name*)))
          (:li :class "full"
            (:label :class (when (and show-error
                                      (empty-string-p (donate-info-address*)))
                             "error")
                    :for "address" "*Address")
            (:input :name "address" :type "text" :value (donate-info-address*)))
          (:li :class "half"
            (:label :class (when (and show-error
                                      (empty-string-p (donate-info-city*)))
                             "error")
                    :for "city" "*City")
            (:input :name "city" :type "text" :value (donate-info-city*)))
          (:li :class "quarter"
            (:label :class (when (and show-error
                                      (empty-string-p (donate-info-state*)))
                             "error")
                    :for "state" "*State")
            (:select :name "state" (str (state-options (donate-info-state*)))))
          (:li :class "quarter"
            (:label :class (when (and show-error
                                      (empty-string-p (donate-info-zip*)))
                             "error")
                    :for "zip" "*Zip")
            (:input :name "zip" :type "text" :value (donate-info-zip*)))
          (:li :class "half"
            (:label :class (when (and show-error
                                      (empty-string-p (donate-info-email*)))
                             "error")
                    :for "email" "*Email")
            (:input :name "email" :type "text" :value (donate-info-email*)))
          (:li :class "half"
            (:label :class (when (and show-error
                                      (empty-string-p (donate-info-phone*)))
                             "error")
                    :for "phone" "*Phone")
            (:input :name "phone" :type "text" :value (donate-info-phone*))))
        (:button :class "nav" :type "submit" "Next >")

        (:p "We do not store your credit card information, and we have a really good " (:a :href "/privacy" "privacy policy") ".")
        (:p "For information on other ways to donate, " (:a :href "/donate/more" "click here") ".")))))

(defun donate-dialog-3 (&key show-error show-amount)
  (with-donate-info
    (html
      (:script :type "text/javascript" :src "https://js.stripe.com/v1/")
      (:script :type "text/javascript"
        (str (ps
               (defvar *processing* nil)
               (defun tokenize (form)
                 (unless *processing*
                   (setf *processing* t)
                   (dolist (element ((@ document get-elements-by-tag-name) "label"))
                     ((@ element set-attribute) "class" ""))
                   ((@ *stripe set-publishable-key) "pk_test_ffSdkWiT4zyspRyxsM8tb0tI")
                   ((@ *stripe create-token)
                    (create
                      :number (@ ((@ document get-element-by-id) "ccn") value)
                      :cvc (@ ((@ document get-element-by-id) "cvc") value)
                      :exp_month (@ ((@ document get-element-by-id) "ccm") value)
                      :exp_year (@ ((@ document get-element-by-id) "ccy") value)
                      :name (ps:lisp (donate-info-name*))
                      :address_city (ps:lisp (donate-info-city*))
                      :address_line1 (ps:lisp (donate-info-address*))
                      :address_state (ps:lisp (donate-info-state*))
                      :address_zip (ps:lisp (donate-info-zip*)))
                    (lambda (status response)
                      ((@ console log) response)
                      (cond
                        ((@ response :error)
                         (setf *processing* nil)
                         (cond
                           ((or (eq (@ response :error :code) "invalid_number")
                                (eq (@ response :error :code) "incorrect_number"))
                            ((@ ((@ document get-element-by-id) "lccn") set-attribute) "class" "error"))
                           ((eq (@ response :error :code) "invalid_cvc")
                            ((@ ((@ document get-element-by-id) "lcvc") set-attribute) "class" "error"))
                           ((eq (@ response :error :code) "invalid_expiry_month")
                            ((@ ((@ document get-element-by-id) "lccm") set-attribute) "class" "error") 
                            ((@ ((@ document get-element-by-id) "lccy") set-attribute) "class" "error"))
                           ((eq (@ response :error :code) "invalid_expiry_year")
                            ((@ ((@ document get-element-by-id) "lccm") set-attribute) "class" "error")
                            ((@ ((@ document get-element-by-id) "lccy") set-attribute) "class" "error"))
                           (t (alert "unknown error!"))))
                        (t (setf (@ ((@ document get-element-by-id) "cctoken") value)
                                 (@ response :id))
                           ((@ form submit)))))))
                 f))))
      (:form :id "donate" :method "post" :action "/donate" :onsubmit "return tokenize(this);"
        (:h2 "Step 3/4")
        (:h3 "Credit card info")
        (:input :id "cctoken" :name "token" :type "hidden")
        (:ul
          (when (or (not (donate-info-amount*)) show-amount)
            (htm
              (:li :class "full"
                (:label :class (when (and show-error
                                          (not (donate-info-amount*)))
                                 "error")
                        :for "amount" "*Donation amount")
                (:input :name "amount" :type "text" :value (donate-info-amount*))))) 
          (:li :class "full"
            (:label :class (when (and show-error
                                      (empty-string-p (donate-info-token*)))
                             "error")
                    :id "lccn"
                    "*Card number")
            (:input :id "ccn" :type "text"))
          (:li :class "quarter"
            (:label :class (when (and show-error
                                      (empty-string-p (donate-info-token*)))
                             "error")
                    :id "lcvc"
                    "*CVC " (:a :href "http://en.wikipedia.org/wiki/Card_security_code" :target "_blank" "(?)"))
            (:input :id "cvc" :type "text")
           
           )
          (:li :class "half"
            (:label :class (when (and show-error
                                      (empty-string-p (donate-info-token*)))
                             "error")
                    :id "lccm"
                    "*Exp month")
            (:select :id "ccm"
              (:option :value "01" "01")
              (:option :value "02" "02")
              (:option :value "03" "03")
              (:option :value "04" "04")
              (:option :value "05" "05")
              (:option :value "06" "06")
              (:option :value "07" "07")
              (:option :value "08" "08")
              (:option :value "09" "09")
              (:option :value "10" "10")
              (:option :value "11" "11")
              (:option :value "12" "12")))
          (:li :class "quarter"
            (:label :class (when (and show-error
                                      (empty-string-p (donate-info-token*)))
                             "error")
                    :id "lccy"
                    "*Exp year")
            (:select :id "ccy"
              (:option :value "2012" "2012")
              (:option :value "2013" "2013")
              (:option :value "2014" "2014")
              (:option :value "2015" "2015")
              (:option :value "2016" "2016")
              (:option :value "2017" "2017")
              (:option :value "2018" "2018")
              (:option :value "2019" "2019")
              (:option :value "2020" "2020")
              (:option :value "2021" "2021")
              (:option :value "2022" "2022"))))

        (:button :id "ccnext" :class "nav" :type "submit" "Next >")

        (:p "We do not store your credit card information, and we have a really good " (:a :href "/privacy" "privacy policy") ".")
        (:p "For information on other ways to donate, " (:a :href "/donate/more" "click here") ".")))))

(defun donate-dialog-4 ()
  (html
    (:form :id "donate" :method "post" :action "/donate"
      (:h2 "Step 4/4")
      (:h3 "Confirm donation")

      (:p (:strong "Donation amount:") " $" (str (donate-info-amount*)))
      (:p (:strong "Donation type:") " " (str (donate-info-type*)))

      (:button :name "confirm" :class "nav" :type "submit" "Donate >")

      (:p "We do not store your credit card information, and we have a really good " (:a :href "/privacy" "privacy policy") ".")
      (:p "For information on other ways to donate, " (:a :href "/donate/more" "click here") "."))))
    
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
      (donate-page (donate-monthly-1))))
  (:post
    (let ((amount (post-parameter "amount"))
          (type (post-parameter "type"))
          (name (post-parameter "name"))
          (address (post-parameter "address"))
          (city (post-parameter "city"))
          (state (post-parameter "state"))
          (zip (post-parameter "zip"))
          (email (post-parameter "email"))
          (phone (post-parameter "phone"))
          (token (post-parameter "token")) 
          (confirm (post-parameter "confirm")))

      (with-donate-info
          (cond
            ((and type
                  (or (string= type "monthly")
                      (string= type "once")))

             (setf (donate-info-amount*)
                   (if (and amount (scan +number-scanner+ amount))
                     (parse-integer amount)
                     nil))

             (setf (donate-info-type*) type)
             (see-other "/donate/2"))

            ((and (or (string= (donate-info-type*) "monthly")
                      (string= (donate-info-type*) "once"))
                  name
                  address
                  city
                  state
                  zip
                  email
                  phone)

             (setf (donate-info-name*) name)
             (setf (donate-info-address*) address)
             (setf (donate-info-city*) city)
             (setf (donate-info-state*) state)
             (setf (donate-info-zip*) zip)
             (setf (donate-info-email*) email)
             (setf (donate-info-phone*) phone)

             (if (and (not (string= name ""))
                      (not (string= address ""))
                      (not (string= city ""))
                      (not (string= state ""))
                      (scan +email-scanner+ email)
                      (scan +phone-scanner+ phone))
               (see-other "/donate/3")
               (donate-page (donate-dialog-2 t))))

            ((not (empty-string-p token))
              (unless (donate-info-amount*)
                (if (and amount (scan +number-scanner+ amount))
                  (setf (donate-info-amount*) (parse-integer amount)) 
                  (donate-page (donate-dialog-3 :show-error t :show-amount t))))
              (setf (donate-info-token*) token)
              (see-other "/donate/4"))

            ((and confirm
                  (donate-info-type*)
                  (donate-info-name*)
                  (donate-info-address*)
                  (donate-info-city*)
                  (donate-info-state*)
                  (donate-info-zip*)
                  (donate-info-email*)
                  (donate-info-phone*) 
                  (donate-info-amount*) 
                  (not (empty-string-p (donate-info-token*))))

             (handler-case
               (cond
                 ((string= (donate-info-type*) "once")

                  (stripe:create-charge :card (donate-info-token*)
                                        :amount (* 100 (donate-info-amount*))
                                        :currency "USD"
                                        :description (donate-info-email*))

                  "yay!")
                 
                 ((string= (donate-info-type*) "monthly")

                  (aif (getf *user* :custid)
                    (progn
                      (stripe:update-subscription it
                        :plan (make-donation-plan (* 100 (donate-info-amount*)))
                        :prorate nil
                        :card (donate-info-token*))
                      "yay updated!")
                    (let ((customer (stripe:create-customer
                                      :card (donate-info-token*)
                                      :email (donate-info-email*)
                                      :plan (make-donation-plan (* 100 (donate-info-amount*))))))

                      (acond
                        ((stripe:sstruct-get customer :id)
                         (modify-db *userid* :custid it)
                         "yay new monthly!")

                        (t "oh no something bad :-("))))))
               (stripe::stripe-error (err)
                 (let ((code (stripe:sstruct-get (stripe::stripe-error-reply err) :error :code)))
                   (cond
                     ((string= code "card_declined")
                      "Your card was declined")

                     ((string= code "processing_error")
                      "Our payment processor encountered an error while processing your card.")

                     ((string= code "invalid_cvc")
                      "The CVC provided was incorrect.")

                     (t "An error occurred while processing your card."))))))

            (t (see-other "/donate")))))))

(defroute "/donate/2" ()
  (:get
    (with-donate-info
      (if (and (donate-info-type*))
        (base-page
          "Donate"
          (donate-page (donate-dialog-2)))
        (see-other "/donate")))))

(defroute "/donate/3" ()
  (:get
    (with-donate-info
      (if (and (donate-info-type*)
               (donate-info-name*)
               (donate-info-address*)
               (donate-info-city*)
               (donate-info-state*)
               (donate-info-zip*)
               (donate-info-email*)
               (donate-info-phone*))
        (base-page
          "Donate"
          (donate-page (donate-dialog-3)))
        (see-other "/donate")))))

(defroute "/donate/4" ()
  (:get
    (with-donate-info
      (if (and (donate-info-type*)
               (donate-info-name*)
               (donate-info-address*)
               (donate-info-city*)
               (donate-info-state*)
               (donate-info-zip*)
               (donate-info-email*)
               (donate-info-phone*) 
               (donate-info-amount*) 
               (not (empty-string-p (donate-info-token*))))
        (base-page
          "Donate"
          (donate-page (donate-dialog-4)))
        (see-other "/donate")))))

(defroute "/donate/once" ()
  (:get
    (base-page
      "Donate"
      (donate-page (donate-once-1)))))
