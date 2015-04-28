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

(defun get-login ()
  (with-user
    (if *user*
      (see-other "/home")
      (header-page
        "Login"
        nil
        (html
          (dolist (flash (flashes))
            (str flash))
          (:div :id "body"
            (:form :method "POST" :action "/login" :id "login"
             (awhen (get-parameter "retry")
               (htm (:p :class "error" "The email/username or password was incorrect.")
                    (unless (string= it "")
                        (htm (:p (:a :href (s+ "/signup?email=" it)
                                     "Would you like to create an account?"))))))
             (awhen (get-parameter "next")
               (htm (:input :type "hidden" :name "next" :value it)))
             (:label :for "username" "Username or email")
             (:input :type "text"
                     :id "username"
                     :name "username"
                     :value (get-parameter "retry"))
             (:label :for "password" "Password")
             (:input :type "password"
                     :id "password"
                     :name "password")
             (:button :type "submit" :class "yes" "Log in")
             (:span (:a :href "/reset" "Forgot your password?")))))
        :hide-menu t))))

(defun post-login ()
  (with-token
    (let ((username (post-parameter "username"))
          (next (post-parameter-string "next"))
          (user nil))
      (if (find #\@ username :test #'equal)
        (setf user (gethash username *email-index*))
        (setf user (gethash username *username-index*)))
      (cond
        ((gethash username *banned-emails-index*)
         (flash (s+ "The email you have entered, "
                    username
                    ", is associated with an account "
                    "that has been suspended for posting inappropriate "
                    "content or otherwise violating Kindista's "
                    "Terms of Use. "
                    "If you believe this to be an error, please email us "
                    "so we can resolve this issue.")
                :error t)
         (see-other "/home"))
        ((password-match-p user (post-parameter "password"))
         (setf (token-userid *token*) user)
         (notice :login)
         (see-other (or next "/home")))
        (t
         (see-other (if next (url-compose "/login" "next" next) "/login"))
         (flash "The email or password you entered was not recognized.  Please try again." :error t)
         (notice :auth-failure :username user)
         "")))))

(defun get-logout ()
  (notice :logout)
  (delete-token-cookie)
  (see-other "/"))
