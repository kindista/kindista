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
              (:label :for "username" "Email")
              (:input :id "username"
                      :type "text"
                      :name "username" )
              (:label :for "password" "Password")
              (:input :id "password"
                      :type "password"
                      :name "password" )
              (:br)
              (:a :href "/reset" "Forgot your password?")
              (:button :class "yes" :type "submit" "Sign In")
              (unless *productionp*
                (htm
                  (:div :class "social-signin"
                    (:div :class "center" "or")
                    (str
                      (facebook-sign-in-button :redirect-uri "login")))))))))

        (:div :id "splashbox"
          (:div :class "half float-left"
           (:h2 "Kindista helps you share offers, requests, and gratitude with friends and neighbors.")
           (:p "Make new connections and lead a more abundant life through sharing. "
            (:strong "Because sharing is good.")))
         (:div :class "half float-left"
           (:h2 :class "center" "New to Kindista?")
           (:div :class "center"
             (:a :class "yes"
                 :id "big-ass-signup-button" 
                 :href "/signup" "Create an account."))
             (:p :class "center"
              (:a :href "/home" "...or try Kindista as a guest")))
           (:p :class "clear"))

        (:div :id "preview"
          (:h2 "What's happening on Kindista?")
          (:p "People are sharing offers, requests, and gratitude with friends and neighbors. Here's what's happening now:")
          (:div :id "preview-cards"
            (with-location
              (str (activity-items
                     (remove-private-items
                       (safe-sort *recent-activity-index*
                                  #'> :key
                                  #'result-time))
                     :url "/"
                     :page 0
                     :count 10
                     :location nil
                     :paginate nil))))
          (:div :class "splactions"
            (:a :class "yes" :href "/home" "Try Kindista")
            " "
            (:a :class "yes" :href "/signup"  "Create an account")
            " "
            (:a :class "yes" :href "/donate" "Donate")))


        (:div :id "screenshots"
          (:h2 "What does Kindista look like?")
          (:p "It works great on computers, phones, and tablets.")
          (:img :src "/media/screenshot2.png")
          (:div :class "splactions"
            (:a :class "yes" :href "/home" "Try Kindista")
            " "
            (:a :class "yes" :href "/signup" "Create an account")
            " "
            (:a :class "yes" :href "/donate" "Donate"))
          (:br)
          (:p "Kindista is a project of CommonGoods Network, Inc., an awesome Oregon 501(c)(3) non-profit organization.")
        (:div :class "splactions footer"
          (if *userid*
            (htm (:a :href "/contact-kindista" "contact"))
            (htm (:a :href "mailto:info@kindista.org" "contact")))
          " &middot; "
          (:a :href "/about" "about")
          " &middot; "
          (:a :href "/terms" "terms")
          " &middot; "
          (:a :href "/privacy" "privacy")  
         )))

      :class "splash")))
