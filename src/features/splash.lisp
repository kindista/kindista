;;; Copyright 2012-2016 CommonGoods Network, Inc.
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

(defun get-splash ()
  (if *user*
    (see-other "/home")
    (base-page
      "Welcome to Kindista"
      (html
        (:div :id "header-container"
         (:div :id "header"
          (:a :id "logo" :href "/"
           (:img :id "symbol" :src "/media/logo.png"))
          (:div :id "splashlogin"
            (:a :href "/login" :class "yes" "Sign In")
            (:form :action "/login" :method "post"
             (:div :id "email-signin"
              (:label :for "username" "Email")
              (:input :id "username"
               :type "text"
               :name "username" )
              (:label :for "password" "Password")
              (:input :id "password"
               :type "password"
               :name "password" )
              (:a :href "/reset" "Forgot your password?")
              (:button :class "yes" :type "submit" "Sign In")
              (:br :class "clear"))
              (unless *productionp*
                (htm
                  (:div :class "social-signin"
                    (str *or-divider*)
                    (str
                      (facebook-sign-in-button :redirect-uri "login")))))))))

        (:h1 "Share resources and gratitude with friends and neighbors.")
        (:div :id "splashbox"
          (:div :class "half float-left"
           "Make new connections and lead a more abundant life through sharing. "
           (:br)
           (:strong "Because sharing is good."))
         (:div :class "half float-left"
           (:h2 :class "center" "New to Kindista?")
           (:div :class "center"
             (:a :class "yes"
                 :id "big-ass-signup-button" 
                 :href "/signup" "Create an account."))
             (:p :class "center"
              (:a :href "/home" "...or try Kindista as a guest")))

           (:p :class "clear"))

       (:div :class "details"
          (:h2 "How does Kindista work?")
          (:ol :class "numeric"
            (:li (:strong "Everything shared is given freely. ")
             "That means no bartering, renting, or selling.")
            (:li (:strong "You choose who you want to share with. ")
             "When considering sharing with someone you don't already know, you can check their reputation and get references from mutual connections.")
            (:li (:strong "Be kind and respectful."))))

       (:div :id "word-association"
         (:h2 "What does it mean to be a Kindista?")
         (:p (str (icon "offers")) " offering free " (str (icon "gift")) " resources ")
         (:p "fulfilling " (str (icon "map-marker")) " local requests " (str (icon "requests")))
         (:p "growing community generosity")
         (:p "sharing prosperity " (str (icon "gratitude-love")) " gratitude ")
         (:p "freedom " (str (icon "groups")) " connection ease ")
         (:p (str (icon "share")) " honoring reputation " (str (icon "verified-identity")))
         (:p " giving abundance trusting love ")
         (:p (str (icon "home")) " receiving plenty"))

       ;(:div :id "preview"
       ;  (:h2 "What's happening on Kindista?")
       ;  (:p "People are sharing offers, requests, and gratitude with friends and neighbors. Here's what's happening now:")
       ;  (:div :id "preview-cards"
       ;    (with-location
       ;      (str (activity-items
       ;             (remove-private-items
       ;               (safe-sort *recent-activity-index*
       ;                          #'> :key
       ;                          #'result-time))
       ;             :url "/"
       ;             :page 0
       ;             :count 10
       ;             :location nil
       ;             :paginate nil))))
       ;  (:div :class "splactions"
       ;    (:a :class "yes" :href "/home" "Try Kindista")
       ;    " "
       ;    (:a :class "yes" :href "/signup"  "Create an account")
       ;    " "
       ;    (:a :class "yes" :href "/donate" "Donate")))

       (:div :class "about splactions" "Kindista is a project of CommonGoods Network, Inc., an awesome Oregon 501(c)(3) non-profit organization.")
       (:div :class "splactions footer"
         (if *userid*
           (htm (:a :href "/contact-kindista" "contact"))
           (htm (:a :href "mailto:info@kindista.org" "contact")))
         " &middot; "
         (:a :href "/about" "about")
         " &middot; "
         (:a :href "/terms" "terms")
         " &middot; "
         (:a :href "/privacy" "privacy")))

      :class "splash")))
