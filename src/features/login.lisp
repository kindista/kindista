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

(defun get-login (&aux (next (get-parameter-string "next")))
  (when next
    (setf (getf (token-session-data *token*) :login-redirect)
          next))
  (cond
   ((get-parameter-string "code") (post-login))
   (*user* (see-other "/home"))
   (t
    (header-page
      "Login"
      nil
      (html
        (dolist (flash (flashes))
          (str flash))
          (:div :id "login"
            (:h1 "Sign in")
            (:h3 "Welcome back!")
            (:div :id "login-form"
              (:form :method "POST" :action "/login" :class "login"
                (awhen (get-parameter "retry")
                  (htm (:p :class "error" "The email/username or password was incorrect.")
                       (unless (string= it "")
                           (htm (:p (:a :href (s+ "/signup?email=" it)
                                        "Would you like to create an account?"))))))
                (when next
                  (htm (:input :type "hidden" :name "next" :value next)))
                (:label "Username or email"
                  (:input :type "text"
                   :class "username"
                   :name "username"
                   :value (get-parameter "retry")))
                (:label "Password"
                  (:input :type "password"
                   :class "password"
                   :name "password"))
                (:button :type "submit" :class "yes" "Sign in")
                (:span :class "forgot"
                 (:a :href "/reset" :class "reset"  "Forgot your password?"))))
            (htm
              (str *or-divider*)
              (when (or *enable-facebook* (getf *user* :test-user))
                (htm
                  (:div :class "social-signin"
                     (str (if (and (get-parameter "facebook-signup")
                                   (not *productionp*))
                            (facebook-sign-in-button
                              :redirect-uri "signup"
                              :button-text "Use Facebook to sign up for Kindista")
                            (facebook-sign-in-button :redirect-uri "login"))
                          )))))
            (:div :id "join"
              (:span "Not on Kindista yet? ")
              (:a :href "/signup"
               "Join the kindness revolution!"))))
      :hide-menu t))))

(defun register-login (userid &key next fb-token fb-expires fb-scope &aux (user (db userid)))
  (setf (token-userid *token*) userid)
  (when (and fb-token
             fb-expires
             (or (not (string= (getf user :fb-token)
                               fb-token))
                 (not (eql fb-expires (getf user :fb-expires)))))
    (apply #'modify-db
           userid
           (remove-nil-plist-pairs
             (list :fb-token fb-token
                   :fb-expires fb-expires
                   :fb-scope fb-scope
                   :fb-link-active t))))
  (asetf (token-session-data *token*)
         (remove-from-plist it :login-redirect))
  (let ((*user* (db userid)))
    (when (and (getf *user* :fb-token)
               (getf *user* :fb-link-active)
               (not (getf *user* :avatar))))
    (modify-db userid :avatar (save-facebook-profile-picture-to-avatar userid)))
  (when (getf (token-session-data *token*) :publish-to-fb)
    (let* ((id (getf (token-session-data *token*) :publish-to-fb))
           (item (db id))
           (type (getf item :type)))
      (notice :new-facebook-action :item-id id)
      (modify-db id :fb-publishing-in-process (get-universal-time))
      (flash (s+ "Your "
                 (string-downcase (symbol-name type))
                 " has been published on Facebook"))
      (asetf (token-session-data *token*)
             (remove-from-plist it :publish-to-fb))))
  (with-locked-hash-table (*user-tokens-index*)
    (asetf (gethash userid *user-tokens-index*)
           (push (cons (cookie-in "token") *token*) it)))
  (notice :login)
  (see-other (if (not (db userid :active))
               "/settings#reactivate"
               (or next "/home"))))

(defun post-login
  (&key (fb-token-data (when (get-parameter-string "code")
                               (register-facebook-user "login")))
        (fb-scope (when fb-token-data
                    (awhen (get-parameter-string "granted_scopes")
                      (mapcar #'string-to-keyword
                              (words-from-string it)))))
        (fb-token (cdr (assoc "access_token"
                              fb-token-data
                              :test #'string=)))
        (fb-expires (when fb-token-data
                      (+ (get-universal-time)
                         (safe-parse-integer (cdr (assoc "expires"
                                                         fb-token-data
                                                         :test #'string=))))))
        (fb-data (when fb-token (get-facebook-user-data fb-token)))
   &aux (username (remove-whitespace-around-string (post-parameter "username")))
        (password (post-parameter-string "password"))
        (login-token-redirect (getf (token-session-data *token*) :login-redirect))
        (next (or login-token-redirect
                  (awhen (post-parameter-string "next") (url-decode it))))
        (fb-id (safe-parse-integer (getf fb-data :id)))
        (existing-k-id (gethash fb-id *facebook-id-index*))
        (userid nil))

  (setf userid
        (cond
         (existing-k-id existing-k-id)
         ((find #\@ username :test #'equal)
          (gethash username *email-index*))
         (t (gethash username *username-index*))))

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

    ((or existing-k-id
         (and password (password-match-p userid password)))
     (apply #'register-login userid
            (remove-nil-plist-pairs
              (list :next next
                    :fb-token fb-token
                    :fb-expires fb-expires
                    :fb-scope fb-scope))))

    (fb-token-data
     (flash "There is no Kindista account associated with the Facebook account currently active on this browser. Please confirm that you are logged into Facebook, or Sign Up for Kindista below."
            :error t)
     (notice :auth-failure :fb-id fb-id)
     (see-other (if next
                  (url-compose "/login"
                               "next" (url-encode next)
                               "facebook-signup" "t")
                  (url-compose "/login" "facebook-signup" "t"))))
    (t
     (flash "The email or password you entered was not recognized.  Please try again." :error t)
     (notice :auth-failure :username userid)
     (see-other (if next
                  (url-compose "/login" "next" (url-encode next))
                  "/login"))
     )))

(defun get-logout ()
  (notice :logout)
  (delete-token-cookie)
  (see-other "/"))
