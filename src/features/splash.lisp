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
              (:div
                (:a :href "/reset" "Forgot your password?")
                (:div :id "signin-button"
                  (:button :class "yes" :type "submit" "Sign In"))))
              (unless *productionp*
                (htm
                  (:div :class "social-signin"
                    (str *or-divider*)
                    (str
                      (facebook-sign-in-button :redirect-uri "login")))))))))

        (:div :class "full-width lt-fuchsia-bg"
         (:div
           (:h1 "Share resources and gratitude with friends and neighbors"))
         (:div :id "splashbox-container"
          (:div :id "splashbox"
           (:div
             (:p "Make new connections and lead a more abundant life through sharing. ")
             (:p (:strong "Because sharing is good.")))
           (:div
             (:h3 :class "center" "New to Kindista?")
             (:div :class "center"
              (:a :class "yes"
               :id "big-ass-signup-button"
               :href "/signup" "Create an account"))
             (:p :class "center"
              (:a :href "/home" "...or try Kindista as a guest"))))))

       (:div :class "full-width lt-blue-bg"
        (:div :class "details"
          (:h2 "How does Kindista work?")
          (:ol :class "numeric"
            (:li (:strong "Everything shared is given freely. ")
             "That means no bartering, renting, or selling.")
            (:li (:strong "Post gratitude to celebrate generosity in your community. ") "Statements of gratitude build the reputation of givers and function as a currency to promote more sharing.")
            (:li (:strong "You choose who you want to share with. ")
             "When considering sharing with someone you don't already know, you can check their reputation and get references from mutual connections.")
            (:li (:strong "Be kind and respectful.")))))

       (:div :class "full-width green-bg"
        (:div :class "details"
         (:h2 "What does it mean to be a Kindista?")
         (:div :id "word-association"
           (:p (str (icon "offers")) " offering free " (str (icon "gift")) " resources ")
           (:p "fulfilling " (str (icon "map-marker")) " local requests " (str (icon "requests")))
           (:p "growing community generosity")
           (:p "sharing prosperity " (str (icon "gratitude-love")) " gratitude ")
           (:p "freedom " (str (icon "groups")) " connection ease ")
           (:p (str (icon "share")) " honoring reputation " (str (icon "verified-identity")))
           (:p " giving abundance trusting love ")
           (:p (str (icon "home")) " receiving plenty"))))

       (:div :class "full-width details dk-brown-bg"
         (:div :class "footer"
           (:div :class "about" "Kindista is a project of CommonGoods Network, Inc., an awesome Oregon 501(c)(3) non-profit organization.")
           (:div
            (if *userid*
              (htm (:a :href "/contact-kindista" "contact"))
              (htm (:a :href "mailto:info@kindista.org" "contact")))
            " &middot; "
            (:a :href "/about" "about")
            " &middot; "
            (:a :href "/terms" "terms")
            " &middot; "
            (:a :href "/privacy" "privacy")))))

      :extra-head (facebook-item-meta-content
                    nil
                    nil
                    "Join the Revolution of Kindness"
                    :url +base-url+
                    :description "Kindista is a revolutionary economic network where everything is free and the currency is gratitude."
                    )
      :class "splash")))
