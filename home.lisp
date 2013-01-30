(in-package :kindista)

(defroute "/home" ()
  (:get
    (with-user
      (cond
        ((or (getf *user* :location) (not *user*))
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
               (:strong "show activity within ")
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
                (with-location
                  (str (activity-items :page page))))))
           :selected "home"
           :top (cond
                  ((not *user*)
                   (welcome-bar
                     (html
                       (:h2 "Welcome to Kindista!")
                       (:p "On this page you can see what's going on around you and with people you have made
                            a connection with. Use the menu "
                           (:span :class "menu-button" " (click the button on the header) ")
                           (:span :class "menu-showing" " on the left ")
                           " to explore the site."))
                     nil))
                  ((getf *user* :help)
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
                           " to explore the site.")))))
           :right (html
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

                    (unless *user*
                      (htm
                        (:div :class "item"
                          (:h3 "Log in")
                          (:form :method "POST" :action "/login" :id "login"
                            (:label :for "username" "Username or email")
                            (:input :type "text" :name "username" :value (get-parameter "retry"))
                            (:label :for "password" "Password")
                            (:input :type "password" :name "password")
                            (:input :type "submit" :value "Log in")
                            (:a :href "/reset" "Forgot your password?")))))

                    (:div :class "item"
                      (:h3 (:a :href "/donate" "Help us grow!"))
                      (:p "We are a tiny non-profit, and we rely on your contributions to pay our bills and fund improvements. Will you " (:a :href "/donate" "make a tax-deductable donation") " and help us grow? Learn more about " (:a :href "/plan" "what we are doing") ".")) 
                    
                    (when *user*
                      (htm
                        (:div :class "item right only"
                          (:h3 (:a :href "/invite" "Invite your friends"))
                          (:p "Kindista is invitation-only. As a Kindista member, you can invite people you know to join. " (:a :href "/faq/" "How does this work?")))))

                    (:div :class "item right only"
                      (:h3 "Upcoming " (:a :href "/events" "Events"))
                      (:menu
                        (:li (:strong "Thursday, November 23"))
                        (:li "7:00PM " (:a :href "x" "East Eugene Gift Circle"))
                        (:li (:strong "Thursday, November 30"))
                        (:li "7:00PM " (:a :href "x" "West Eugene Gift Circle"))))   

                    (when *user*
                      (htm
                        (:div :class "item right only"
                          (:h3 "People you may know")
                          (:menu
                            (dolist (data (suggested-people))
                              (htm
                                (:li (:a :href (strcat "/people/" (username-or-id (cdr data)))
                                         (str (getf (db (cdr data)) :name)))))))))))))
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
