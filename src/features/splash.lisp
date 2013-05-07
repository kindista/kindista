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

(defun get-splash ()
  (if *user*
    (see-other "/home")
    (base-page
      "Welcome to Kindista"
      (html
        (:div :id "header"
          (:a :id "logo" :href "/"
           (:img :id "symbol" :src "/media/logo.png")))

        (:div :id "splashbox"
          (:form :id "splashlogin" :action "/login" :method "post"
            (:label :for "username" "Username or email")
            (:input :id "username" :type "text" :name "username" )
            (:label :for "password" "Password")
            (:input :id "password" :type "password" :name "password" )
            (:button :class "yes" :type "submit" "Log in")
            (:p (:a :href "/reset" "Forgot your password?"))
            (:h3 "New to Kindista?")
            (:p (:a :href "/request-invitation" "Request an invitation.")))  
          (:h2 "Kindista helps you share offers, requests, and gratitude with people who live nearby.")
          (:p "Make connections with people who can help you lead a more abundant life through the culture of sharing. "
              (:strong "Because sharing is good."))
          (:p :class "clear"))

        (:div :id "preview"
          (:h2 "What's happening on Kindista?")
          (:p "People are sharing offers, requests, and gratitude with others who live near them. Here's a sampling of what's happening right now.")
          (:div :id "preview-cards"
            (with-location
              (str (activity-items (sort (copy-list *recent-activity-index*) #'> :key #'result-time)
                                   :url "/"
                                   :page 0
                                   :count 10
                                   :location nil
                                   :paginate nil))))
          (:div :class "splactions"
            (:a :class "yes" :href "/home" "Try out Kindista")
            " "
            (:a :class "yes" :href "/request-invitation" "Request an invitation")
            " "
            (:a :class "yes" :href "/donate" "Make a donation")))


        (:div :id "screenshots"
          (:h2 "What does Kindista look like?")
          (:p "It works great on computers, phones, and tablets.")
          (:img :src "/media/screenshot2.png")
          (:div :class "splactions"
            (:a :class "yes" :href "/home" "Try out Kindista")
            " "
            (:a :class "yes" :href "/request-invitation" "Request an invitation")
            " "
            (:a :class "yes" :href "/donate" "Make a donation"))
          (:br)
          (:p "Kindista is a project of CommonGoods Network, Inc., an awesome Oregon 501(c)(3) non-profit organization.")))

      :class "splash")))
