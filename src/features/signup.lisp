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

(defun signup-page (&key error name email token)
  (standard-page
    "Sign up"
    (html
      (:h2 "Please RSVP to your invitation. ")
      (:p "Your activation code is:  " (:strong (str token))
        (:br) "Your email address is: " (:strong (str email)))
      (:p "You will be able to change your email address on the Settings page after you sign up.")
      (:form :method "POST" :action "/signup" :id "signup"
        (:input :type "hidden" :name "token" :value token)
        (:input :type "hidden" :name "email" :value email)
        (:h2 "Create an account")
        (when error
          (flash error :error t))
        (:span
        (:label :for "name" "Full Name") 
        (:input :type "text" :name "name" :value name)) 
        (:br)
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
             nil))))

(defun get-signup ()
  (with-user
    (let* ((get-email (get-parameter "email"))
           (get-token (get-parameter "token"))
           (invitation-id (car (gethash get-token *invitation-index*)))
           (invitation (db invitation-id))
           (valid-email (getf invitation :recipient-email))
           (name (getf invitation :name)))
      (cond 
        (*user* 
         (see-other "/home")) 
        ((or (not get-token)
             (not (string= valid-email get-email)))
         (flash "Kindista is by invitation only. You can only RSVP from a valid invitation email." :error t) 
         (see-other "/home"))
        (t 
         (signup-page :email get-email
                      :token get-token
                      :name name))))))

(defun post-signup ()
  (with-user
    (let* ((token (post-parameter "token"))
           (name (post-parameter "name"))
           (email (post-parameter "email"))
           (id (car (gethash token *invitation-index*)))
           (invitation (db id))
           (host (getf invitation :host))
           (invite-request-id (getf invitation :invite-request-id))
           (new-id nil))
      (when *user* (reset-token-cookie))
      (cond
        ((not invitation)
         (flash "Kindista is by invitation only. You can only RSVP from a valid invitation email." :error t) 
         (see-other "/home"))

        ((not (string= (getf invitation :recipient-email) email))
         (signup-page :error "The invitation you are using belongs to a different email address. 
Please use the correct email address or find someone you know on Kindista and request an invitation."
                      :name name
                      :email email
                      :token token))

        ((gethash email *email-index*)
         (signup-page :error "The email address you have entered already belongs to another Kindista member. Please try again, or contact us if this really is your email address."
                      :name name
                      :email email
                      :token token))

        ((< (getf invitation :valid-until) (get-universal-time))
         (signup-page :error "Your invitation has expired. Please contact the person who invited you to join Kindista and request another invitation."
                      :name name
                      :email email
                      :token token))

        ((not (and (< 0 (length name))
                   (< 0 (length (post-parameter "password")))
                   (< 0 (length (post-parameter "password-2")))))
         (signup-page :error "All fields are required"
                      :name name
                      :email email
                      :token token))

        ((> 8 (length (post-parameter "password")))
         (signup-page :error "Please use a strong password of at least 8 characters."
                      :name name
                      :email email
                      :token token))

        ((not (validate-name name))
         (signup-page :error "Please use your full name"
                      :name name
                      :email email
                      :token token))

        ((not (string= (post-parameter "password") (post-parameter "password-2")))
         (signup-page :error "Your password confirmation did not match the password you entered."
                      :name name
                      :email email
                      :token token))
        (t
           (pprint host)(terpri)
           (setf new-id
                 (aif invite-request-id
                   (progn
                     (modify-db it :type :person
                                   :name name
                                   :emails (list email)
                                   :host +kindista-id+
                                   :active t
                                   :help t
                                   :pass (new-password (post-parameter "password"))
                                   :created (get-universal-time)
                                   :notify-gratitude t
                                   :notify-message t
                                   :notify-kindista t
                                   :email nil
                                   :requested nil)
                     (with-locked-hash-table (*db-results*)
                       (remhash it *db-results*))
                     (when (member it *invite-request-index* :key #'result-id)
                       (with-mutex (*invite-request-mutex*)
                        (setf *invite-request-index* (remove it *invite-request-index* :key #'result-id))))
                     (index-person it (db it))
                     it)
                   (create-person :name (post-parameter "name")
                                  :host host
                                  :email (post-parameter "email")
                                  :password (post-parameter "password"))))
           (setf (token-userid *token*) new-id)
           (unless (eql host +kindista-id+)
             (add-contact new-id host))
           (add-contact host new-id)
           (delete-invitation id)
           (see-other "/home"))))))
