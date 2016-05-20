;;; Copyright 2012-2015 CommonGoods Network, Inc.
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

(defun get-reset-password ()
  (header-page
    "Reset your password"
    nil
    (html
      (dolist (flash (flashes))
        (str flash))
      (:div :id "body"
        (:h2 "Reset your password")
        (acond
         ((get-parameter "email")
          (let ((email it)
                (token (get-parameter "token")))
            (htm
              (:form :method "post"
                     :class "confirm password"
                     :autocomplete "off"
                     :action "/reset"
                 (:input :type "hidden" :name "email" :value email)
                 (if (and token (not (string= token "")))
                   (htm (:input :type "hidden" :name "token" :value token))
                   (htm
                     (:div
                       (:label "Security code:"
                         (:input :type "text"
                                 :name "token"
                                 :placeholder "enter security code here")))))
                     (:div
                       (:label "New password:"
                         (:input :type "password"
                                 :name "new-password-1"
                                 :placeholder "at least 8 characters")))
                     (:div
                       (:label "Confirm new password:"
                         (:input :type "password"
                                 :name "new-password-2"
                                 :placeholder "please retype your new password")))
                     (:button :class "yes small" :type "submit" :class "submit" "Change password"))
               (:p :class "help-text"
                "Minimum of 8 characters. "
                "We strongly recommend using either a mix of upper- and "
                "lower-case letters, numbers, and symbols; or a phrase "
                "of at least 5 words."))))
         (t
          (htm
            (:form :method "post" :class "reset password" :autocomplete "off" :action "/reset"
             (:div
               (:label :for "email" "Please confirm your email address: ")
               (:input :type "text"
                :name "email"
                :id "email"
                :placeholder "enter your email address")
               (:button :class "yes"
                :type "submit"
                "Reset password"))))))))
    :hide-menu t))

(defun post-reset-password ()
  (let* ((email (post-parameter "email"))
         (emailp (scan +email-scanner+ (post-parameter "email")))
         (userid (gethash email *email-index*))
         (user (db userid))
         (token (post-parameter "token")))
   (cond 
     (token
       (cond
         ((not (string= token (car (getf user :password-reset-token))))
          (flash "The security code you entered is incorrect. Please check your email and try again." :error t)
          (see-other (url-compose "/reset" "email" email)))
         ((< (length (post-parameter "new-password-1")) 8)
          (flash "Your new password is too short. Please use at least 8 characters." :error t)
          (see-other (url-compose "/reset" "email" email "token" token)))
         ((not (string= (post-parameter "new-password-1")
                        (post-parameter "new-password-2")))
          (flash "The confirmation text you entered does not match the new password you entered. Please try again." :error t)
          (see-other (url-compose "/reset" "email" email "token" token)))
         (t
          (modify-db userid :pass (new-password (post-parameter "new-password-1"))
                            :password-reset-token nil)
          (setf (token-userid *token*) userid)
          (notice :login :reset t)
          (flash "You have successfully changed your password.")
          (see-other "/home"))))

     ((and email (not emailp))
      (flash "The email address you have entered is incorrect. Please enter a valid email address." :error t)
      (see-other "/reset"))

     ((not userid)
      (flash "The email address you have entered does not belong to any current Kindista members. Please try again." :error t)
      (see-other "/reset"))

     ;when the user already has a valid reset-token
     ((and userid 
           (awhen (getf user :password-reset-token) 
             (> (cdr it) 
                (+ 3600 (get-universal-time)))))
      (send-password-reset userid email)
      (flash (s+ "Your password reset token has been resent to " email ". "
                 "Please click on the link provided in that email to reset "
                 "your password."))
      (see-other (url-compose "/reset" "email" email)))

     ;when the user needs a new password-reset-token
     (userid
      (modify-db userid :password-reset-token (cons
                                                (random-password 12)
                                                (+ (get-universal-time) 604800)))
      (send-password-reset userid email)
      (flash (s+ "A password reset token has been sent to " email ". "
                 "Please click on the link provided in that email to reset "
                 "your password."))
      (see-other (url-compose "/reset" "email" email))))))
