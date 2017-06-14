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
        (:div :class "container"
         (:div :class "header-background")
         (:div :id "splash-top"
          (:div :id "header-container" :class "full-width"
           (:div :id "header"
            (:a :id "logo" :href "/"
             (:img :id "symbol" :src "/media/logo.png"))
            (:div :id "splashlogin"
             (:a :href "/login" :class "yes" "LOGIN")
             ; (:form :action "/login" :method "post"
             ;  (:div :id "email-signin"
             ;   (:label :for "username" "Email")
             ;   (:input :id "username"
             ;    :type "text"
             ;    :name "username" )
             ;   (:label :for "password" "Password")
             ;   (:input :id "password"
             ;    :type "password"
             ;    :name "password" )
             ;   (:div
             ;     (:a :href "/reset" "Forgot your password?")
             ;     (:div :id "signin-button"
             ;      (:button :class "yes" :type "submit" "Sign In"))))
             ;  (unless *productionp*
             ;    (htm
             ;      (:div :class "social-signin"
             ;       (str *or-divider*)
             ;       (str
             ;         (facebook-sign-in-button :redirect-uri "login"))))))
             )))

          (:div :id "signup"
           (:div
             (:div :class "center"
              (:h1 (:em "Join a Revolution of Kindness"))
              (:a :class "purple"
               :id "big-ass-signup-button"
               :href "/signup" "SIGN UP NOW"))
             (:div :id "demo" :class "center"
              (:a :href "/home" "...or try Kindista as a guest"))
             )))

         ; (:ol :class "numeric"
         ;   (:li (:strong "Everything shared is given freely. ")
         ;    "That means no bartering, renting, or selling.")
         ;   (:li (:strong "Post gratitude to celebrate generosity in your community. ") "Statements of gratitude build the reputation of givers and function as a currency to promote more sharing.")
         ;   (:li (:strong "You choose who you want to share with. ")
         ;    "When considering sharing with someone you don't already know, you can check their reputation and get references from mutual connections.")
         ;   (:li (:strong "Be kind and respectful.")))

         (:div :class "full-width green-bg"
          (:div :class "details"
           (:h2 "Share freely.")
           (:div :class "float-right"
            (str (icon "offers")))
           (:p "After decades of consumerism, we live in a world of abundance.  Over the same time we've lost our culture of sharing, and with it the true sense of community.")
           (:p "Kindista is a tool that helps people share goods and services for free, the way neighbors use to.")))

         (:div :class "full-width white-bg"
          (:div :class "details"
           (:h2 :class "text-right" "Find local goods and services.")
           (:div :class "float-left"
            (str (icon "requests")))
           (:div :class "text-right"
            (:p "No selling. No trading. No renting. This is not a classified service; just free stuff in your area.")
            (:p "There's no catch. We're a non-profit dedicated to growing gift economy.")
            )))

         (:div :class "full-width purple-bg"
          (:div :class "details semi-transparent-lighter"
           (:h2 "Gratitude is the currency.")
           (:div :class "float-right"
            (str (icon "gratitude-love-light")))
           (:div :class "text-left"
            (:p "When someone shares with you, or you see them do something awesome for your community, be sure to thank them.")
            (:p "Considering sharing with someone you don't already know? Check their reputation on their profile to see what they have been sharing with others. Keep the generosity flowing!")))))


       (:div :class "full-width details dk-grey-bg"
         (:div :class "footer"
           (:div
            (:a :href "/signup" "SIGNUP")
            (:a :href "/login" "LOGIN")
            (:a :href "/donate" "DONATE"))
           (:div :class "small"
            (if *userid*
              (htm (:a :href "/contact-kindista" "CONTACT"))
              (htm (:a :href "mailto:info@kindista.org" "CONTACT")))
           ;" &middot; "
            (:a :href "/about" "ABOUT")
           ;" &middot; "
            (:a :href "/terms" "TERMS")
           ;" &middot; "
            (:a :href "/privacy" "PRIVACY"))
           (:div :class "about text-right"
             "Kindista is a project of CommonGoods Network, Inc., an awesome Oregon 501(c)(3) non-profit organization.")
           (:div :class "center" (:h2 "Grow Prosperity through Community"))
           )))

      :extra-head (facebook-item-meta-content
                    nil
                    nil
                    "Join the Revolution of Kindness"
                    :url +base-url+
                    :description "Kindista is a revolutionary economic network where everything is free and the currency is gratitude."
                    )
      :class "splash")))
