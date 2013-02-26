;;; Copyright 2012-2013 CommonGoods Network, Inc.
;;;
;;; This file is part of Kindista.
;;;
;;; Kindista is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Affero General Public License as published
;;; by the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Kindista is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public
;;; License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with Kindista.  If not, see <http://www.gnu.org/licenses/>.

(in-package :kindista)

(defroute "/home" ()
  (:get
    (with-user
      (cond
        ((or (getf *user* :location) (not *user*))
         (notice :home "")
         (standard-page
           "Home"
           (html
             (:div :class "activity"
               (:menu :class "horiz"
                 (:strong "actions")
                 (:li (:a :href "/gratitude/new" "express gratitude"))
                 (:li (:a :href "/resource/new" "offer a resource"))
                 (:li (:a :href "/requests/new" "make a request"))
                 ;(:li (:a :href "/events/new" "event"))
                 ;(:li (:a :href "/announcements/new" "post announcement"))
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
                       (:p "On this page you can see what's going on around you and with the people 
                            you connect with. Use the menu "
                           (:span :class "menu-button" " (click the button on the header) ")
                           (:span :class "menu-showing" " on the left ")
                           " to explore the site.")
                       (:p "We've guessed your location as "
                        (:strong "Eugene, OR")
                           " based on your IP address. When you log in you can change your location.")
                       (:p "Many activities on Kindista require the creation of an account. An account represents your true identity in the world. To create a Kindista account, you will need an invitation from an existing Kindista member. By browsing the site you may find people near you to ask for an invitation.")
                       (:p "You can find us on " (:a :href "http://freenode.net/" "Freenode") " " (:a :href "http://en.wikipedia.org/wiki/Internet_Relay_Chat" "IRC") " in #kindista. Source code is available on " (:a :href "http://github.com/kindista/kindista" "GitHub") "."))
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

                    (str (donate-sidebar))
                    
                    (unless *user*
                      (htm
                        (:div :class "login item"
                          (:h3 "Log in")
                          (:form :method "POST" :action "/login" :id "login"
                            (:label :for "username" "Email")
                            (:input :type "text" :name "username" :value (get-parameter "retry"))
                            (:label :for "password" "Password")
                            (:input :type "password" :name "password")
                            (:input :type "submit" :value "Log in")
                            (:a :href "/reset" "Forgot your password?")))))

                    (str (invite-sidebar))

                    #|
                    (:div :class "item right only"
                      (:h3 (:a :href "/events" "Events") " happening nearby")
                      ;; TODO lookup the user's timezone by lat/long
                      ;; 
                      (:menu
                        (let ((lastday nil))
                          (dolist (event (upcoming-events :count 6))
                            (let ((item (db (result-id event))))
                              (htm
                                (:li (:strong (getf item :title)))
                                (:li
                                 "7:00PM "
                                 (:a :href (strcat "/events/" (result-id event))
                                     "East Eugene Gift Circle"))))))))
                    |#

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

         (notice :home-verify-location "")
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
         (notice :home-setup "")
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
