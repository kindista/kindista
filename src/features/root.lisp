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

(defun root-page ()
  (base-page
    nil
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
         " &middot; Programmed in Common Lisp"))))    
