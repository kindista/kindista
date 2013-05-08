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

(defun signup-page (&key error 
                         name 
                         email 
                         token 
                         invitation-email)
  (let ((code (or token (get-parameter "token")))
        (email (or email (get-parameter "email"))))
    (standard-page
      "Sign up"
      (html
        (when code 
          (htm 
            (:h2 "Please RSVP to your invitation. "
             (:br) "Your activation code is:  " (str code))))
        (:form :method "POST" :action "/signup" :id "signup"
          (:input :type "hidden" :name "token" :value code)
          (:input :type "hidden" :name "invitation-email" :value (or invitation-email 
                                                                     (get-parameter "email")))
          (:h2 "Create an account") 
          (when error
            (flash error :error t))
          (:span
          (:label :for "name" "Full Name") 
          (:input :type "text" :name "name" :value name)) 
          (:br)
          (:label :for "email" "Email") 
          (:input :type "email" :name "email" :value (or email 
                                                         invitation-email))
          (:br)
          (:label :for "name" "Password") 
          (:input :type "password" :name "password") 
          (:br)
          (:label :for "name" "Confirm Password") 
          (:input :type "password" :name "password-2") 
          (:p :class "fineprint" "By creating an account, you are agreeing to our "
            (:a :href "/terms" "Terms of Service") " and " (:a :href "/privacy" "Privacy Policy"))

          (:br)
          (:button :class "yes" :type "submit" "Sign up") 

          (:span "Have an account? " (:a :href "/login" "Log in"))))

      :top (when (get-parameter "action")
             (welcome-bar
               (html
                 "If you want to do more than browse and search Kindista, you'll need to "
                 (:a :href "/signup" "create an account") ". Or, " (:a :href "/login" "log in")
                 " if you've already got one!")
               nil)))))

(defun get-signup ()
  (with-user
    (cond 
      (*user* 
       (see-other "/home")) 
      ((or (not (get-parameter "token"))
           (not (get-parameter "email")))
       (flash "Kindista is by invitation only. You can only RSVP from a valid invitation email." :error t) 
       (see-other "/home"))
      (t 
       (signup-page)))))

(defun post-signup ()
  (with-user
    (let* ((token (post-parameter "token"))
           (id (car (gethash token *invitation-index*)))
           (invitation (db id))
           (host (getf invitation :host))
           (new-id nil))
      (when *user* (reset-token-cookie))
      (cond
        ((not invitation)
         (flash "Kindista is by invitation only. You can only RSVP from a valid invitation email." :error t) 
         (see-other "/home"))

        ((not (or (equal (getf invitation :recipient-email)
                         (post-parameter "invitation-email"))
                  (equal (getf invitation :recipient-email)
                         (post-parameter "email"))))
         (signup-page :error "The invitation you are using belongs to a different email address. 
Please use the correct email address or find someone you know on Kindista and request an invitation."
                      :name (post-parameter "name")
                      :email (post-parameter "email")
                      :token token
                      :invitation-email (post-parameter "invitation-email")))

        ((gethash (post-parameter "email") *email-index*)
         (signup-page :error "The email address you have entered already belongs to another Kindista member. Please try again, or contact us if this really is your email address."
                      :name (post-parameter "name")
                      :email (post-parameter "email")
                      :token token
                      :invitation-email (post-parameter "invitation-email")))

        ((< (getf invitation :valid-until) (get-universal-time))
         (signup-page :error "Your invitation has expired. Please contact the person who invited you to join Kindista and request another invitation."
                      :name (post-parameter "name")
                      :email (post-parameter "email")
                      :token token
                      :invitation-email (post-parameter "invitation-email")))

        ((not (and (< 0 (length (post-parameter "name")))
                   (< 0 (length (post-parameter "email")))
                   (< 0 (length (post-parameter "password")))
                   (< 0 (length (post-parameter "password-2")))))
         (signup-page :error "All fields are required"
                      :name (post-parameter "name")
                      :email (post-parameter "email")
                      :token token
                      :invitation-email (post-parameter "invitation-email")))

        ((> 8 (length (post-parameter "password")))
         (signup-page :error "Please use a strong password of at least 8 characters."
                      :name (post-parameter "name")
                      :email (post-parameter "email")
                      :invitation-email (post-parameter "invitation-email")
                      :token token))

        ((not (validate-name (post-parameter "name")))
         (signup-page :error "Please use your full name"
                      :name (post-parameter "name")
                      :email (post-parameter "email")
                      :token token
                      :invitation-email (post-parameter "invitation-email")))

        ((not (string= (post-parameter "password") (post-parameter "password-2")))
         (signup-page :error "Your password confirmation did not match the password you entered."
                      :name (post-parameter "name")
                      :email (post-parameter "email")
                      :token token
                      :invitation-email (post-parameter "invitation-email")))
        (t
           (pprint host)(terpri)
           (setf new-id (create-person :name (post-parameter "name")
                                       :host host
                                       :email (post-parameter "email")
                                       :password (post-parameter "password")))
           (setf (token-userid *token*) new-id)
           (unless (eql host +kindista-id+)
             (add-contact new-id host))
           (add-contact host new-id)
           (delete-invitation id)
           (see-other "/home"))))))
