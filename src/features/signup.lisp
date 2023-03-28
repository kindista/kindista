;;; Copyright 2012-2023 CommonGoods Network, Inc.
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

(defun add-kindista-to-all-user-contact-lists ()
  (dolist (person *active-people-index*)
    (add-contact +kindista-id+ person)))

(defun signup-identity-selection ()
  (html
    (:div :class "identity-selection"
      (:p (:strong "Important: ") "This form is for creating accounts for individuals only.  After you sign up, you will be able to create an account for your organization or group.")
      (:fieldset
        (:legend "What kind of an account is this?")
        (:label
          (:input :type "radio"
                  :class "first-option"
                  :name "account-type"
                  :value "person") 
          "An individual person")
        (:label
          (:input :type "radio"
                  :name "account-type"
                  :value "group")
          "A group (business, non-profit, school, community, church, etc.)")))))

(defun signup-base (body &key error)
  (header-page
    "Sign up"
    nil
    (html
      (dolist (flash (flashes))
        (str flash))
      (when error (htm (:div :class "signup flash err" (str error))))
      (:div :id "signup" (str body)))
    :hide-menu t
    ))

(defun signup-page (&key error name email email2)
  (signup-base
    (html
      (:h1 "Sign up for Kindista ")
      (when (or *enable-facebook-login*
                (string= (get-parameter "sekrut-fb-access")
                         "not-ready-for-prime-time"))
        (htm
          (str (facebook-sign-in-button
                 :redirect-uri "signup"
                 :re-request (get-parameter-string "rerequest-email-permission")
                 :button-text "Use Facebook to sign up for Kindista"))
          (str *or-divider*)))
      (:form :method "POST" :action "/signup" :id "signup-form"
       (:label :for "name" "Full Name")
       (:input :type "text" :id "name" :name "name" :value name)
       (:br)
       (:label :for "email" "Email")
       (:input :type "text" :id "email" :name "email" :value email)
       (:br)
       (:label :for "email-2" "Confirm Email")
       (:input :type "text" :id "email-2" :name "email-2" :value email2)
       (:br)
       (:div
         (str (signup-identity-selection)))
       (:button :class "yes" :type "submit" "Sign up")
       (:p :class "fineprint" "By creating an account, you are agreeing to our "
        (:a :href "/terms" "Terms of Service") " and " (:a :href "/privacy" "Privacy Policy")))


       (:div :id "login"
        (:span "Already have an account? " (:a :href "/login" "Sign in."))))
    :error error))

(defun email-verification-page (&key error name email token host)
  (signup-base
    (html
      (if (eq host +kindista-id+)
        (htm (:h2 "Create a Kindista account")
             (:h3 "Step 2 of 3: Set your password"))
        (htm (:h2 "Please RSVP to your invitation. ")))
      (:p "Your email address is: " (:strong (str email)))
      (:p "You will be able to change it or add additional email addresses on the Settings page after you sign up.")
      (:form :method "POST" :action "/signup" :id "signup-form"
       (:input :type "hidden" :name "token" :value token)
       (:input :type "hidden" :name "email" :value email)
       (unless (eq host +kindista-id+)
         (htm (:h2 "Create an account")))
       (:label :for "name" "Full Name")
       (:input :type "text" :id "name" :name "name" :value name)
       (:br)
       (:label :for "password" "Password")
       (:input :type "password" :id "password" :name "password" :value "")
       (:br)
       (:label :for "password-2" "Confirm Password")
       (:input :type "password" :id "password" :name "password-2" :value "")
       (:div
         (str (signup-identity-selection)))
       (:button :class "yes" :type "submit" "Sign up") 
       (:p :class "fineprint" "By creating an account, you are agreeing to our "
        (:a :href "/terms" "Terms of Service") " and " (:a :href "/privacy" "Privacy Policy"))

       (:br)
       (:div :id "login"
         (:span "Already have an account? " (:a :href "/login" "Sign in.")))))
    :error error))

(defun get-signup
  (&aux (get-email (get-parameter "email"))
        (get-token (get-parameter "token"))
        (facebook-code (get-parameter "code"))
        (valid-email (gethash get-email *invitation-index*))
        (valid-token (rassoc get-token valid-email :test #'equal))
        (invitation-id (car valid-token))
        (invitation (db invitation-id))
        (host (getf invitation :host))
        (name (getf invitation :name)))
  (cond
    (facebook-code (post-signup))
    (*user*
     (see-other "/home"))
    ((and get-token (not valid-token))
     (flash "Your invitation code is incorrect or is no longer valid. No worries, you can still sign up for Kindista below.")
     (signup-page))
    ((not valid-token)
     (signup-page))
    (t
     (email-verification-page :email get-email
                              :token get-token
                              :host host
                              :name name))))

(defun signup-confirmation-sent (invite-id email &key resent)
  (header-page
    "Confirm your email address"
    nil
    (html
      (:div :id "signup-resend-confirmation"
       (:h2
         "We have "
         (str (if resent "resent" "sent"))
         " a confirmation email to " (str email))
       (:p
         "You should be receiving this email very soon. "
         "Please check your email and follow the instructions we sent you to complete the sign-up process. "
         "If you don't see the confirmation email in your inbox, please check your \"junk\" "
         "or \"spam\" folder.")
       (if resent
         (htm
           (:p "Still haven't received your confirmation email?")
           (:p (htm (:a :class "yes" :href "mailto:info@kindista.org"
                      "Contact us."))))
         (htm
           (:div
             (:form :method "post" :action "/signup"
              (:input :type "hidden" :name "invite-id" :value invite-id)
              (:input :type "hidden" :name "email" :value email)
              (:button :type "submit"
               :name "resend-confirmation"
               :class "yes"
               "Resend invitation")))))))
    :hide-menu t))

(defun post-signup
  (&aux (token (post-parameter "token"))
        (person-p (string= (post-parameter "account-type") "person"))
        (group-p (string= (post-parameter "account-type") "group"))
        (email (post-parameter "email"))
        (email2 (post-parameter "email-2"))
        (valid-email-invites (gethash email *invitation-index*))
        (valid-token (rassoc token valid-email-invites :test #'equal))
        (name (unless (and group-p (not valid-token))
                (post-parameter "name")))
        (id (or (car valid-token) (post-parameter-integer "invite-id")))
        (invitation (db id))
        (host (getf invitation :host))
        (invite-request-id (getf invitation :invite-request-id))
        (fb-token-data (when (get-parameter-string "code")
                               (register-facebook-user "signup")))
        (fb-token (getf fb-token-data :access--token))
        (fb-expires (when fb-token-data
                      (+ (get-universal-time)
                         (safe-parse-integer (getf fb-token-data :expires--in)))))
        (fb-id (getf invitation :fb-id)))

  (when *user* (reset-token-cookie))
  (cond
    ;; Facebook signup
    ((and fb-token-data (not invitation))
     (facebook-signup fb-token-data))

    ;; Invitations
    (invitation
      (labels ((try-again (e)
                 (email-verification-page :error e
                                          :name name
                                          :email email
                                          :token token
                                          :host host)))
        (cond
          ((post-parameter "resend-confirmation")
            (resend-invitation id)
            (signup-confirmation-sent id email :resent t))

          (group-p
            (try-again "This form is for creating personal accounts only. Once you create your personal account you can create group accounts from the \"Groups\" section of Kindista. If you ignore this warning you will create mass confusion for our community and will not be able to invite people to join your group. (Also we will probably end up deleting group accounts created with this form.)"))
          ((not valid-token)
            (try-again "The invitation code you have entered is not valid. Please sign up using te link provided in a valid Kindista invitation."))
          ((gethash email *banned-emails-index*)
            (flash (s+ "The email you have entered, "
                       email
                       ", is associated with an account "
                       "that has been banned for posting inappropriate "
                       "content or otherwise violating Kindista's "
                       "Terms of Use. "
                       "If you believe this to be an error, please email us "
                       "so we can resolve this issue.")
                   :error t)
            (see-other "/home"))

          ((gethash email *email-index*)
            (try-again "The email address you have entered already belongs to another Kindista member. Please try again, or contact us if this really is your email address."))

          ((and (numberp (getf invitation :valid-until))
             (< (getf invitation :valid-until) (get-universal-time)))
            (try-again "Your invitation has expired. Please contact the person who invited you to join Kindista and request another invitation."))

          ((not (and (< 0 (length name))
                     (< 0 (length (post-parameter "password")))
                     (< 0 (length (post-parameter "password-2")))))
           (try-again "All fields are required"))
          ((> 8 (length (post-parameter "password")))
            (try-again "Please use a strong password of at least 8 characters."))
          ((not (validate-name name))
            (try-again "Please use your full name"))
          ((not (string= (post-parameter "password") (post-parameter "password-2")))
            (try-again "Your password confirmation did not match the password you entered."))

          ((not person-p)
            (try-again "Please select an account type"))

          (t
           (create-new-person-account name
                                      email
                                      :invitation invitation
                                      :host host
                                      :fb-id fb-id
                                      :fb-token fb-token
                                      :fb-expires fb-expires
                                      :invite-request-id invite-request-id)
           (see-other "/home")))))

      (t
        (labels ((try-again (e)
                  (signup-page :error e
                               :name name
                               :email email
                               :email2 (when (equalp email email2) email2))))
          (cond
            ((or (not id) (not valid-email-invites))
                        (try-again "In response to malicous attacks on our system, and our limited volunteer development resources, Kindista is currently Invite Only. Please use the email address in your invitation to sign up. If you have not yet been invited, you must first receive an invitation from a current Kindista member."))
            (group-p
              (try-again "This form is for creating personal accounts only. Once you create your personal account you can create group accounts from the \"Groups\" section of Kindista. If you ignore this warning you will create mass confusion for our community and will not be able to invite people to join your group. (Also we will probably end up deleting group accounts created with this form.)"))

            ((not (validate-name name))
              (try-again "Please use your full name"))

            ((not (scan +email-scanner+ email))
              (try-again "There was a problem with the email address you entered. Please use a valid email address."))

            ((not (string= email (post-parameter "email-2")))
              (try-again "Your email confirmation did not match the email you entered"))

            ((gethash email *email-index*)
              (flash
                (html "There is already a Kindista account for the email address: "
                      (str email)
                      ". If this is your email address and you forgot your password, you can "
                      (:a :href (s+ +base-url+ "reset") "reset it")
                      ". If you know your password, you can sign in below.")
                :error t)
              (see-other "/login"))

            ((not person-p)
              (try-again "Please select an account type"))

            (t
             (let ((id (aif (find email (unconfirmed-invites +kindista-id+)
                                  :key #'(lambda (item) (getf item :email))
                                  :test #'string=)
                         (resend-invitation (getf it :id))
                         (create-invitation email
                                            :host +kindista-id+
                                            :name name
                                            :expires (* 90 +day-in-seconds+)))))

               (signup-confirmation-sent id email))))))))

(defun facebook-signup (fb-token-data)
  (let* ((fb-token (getf fb-token-data :access--token))
         (fb-expires (when fb-token-data
                           (+ (get-universal-time)
                              (safe-parse-integer (getf fb-token-data :expires--in)))))
         (fb-data (get-facebook-user-data fb-token))
         (fb-id (safe-parse-integer (getf fb-data :id)))
         (raw-fb-email (getf fb-data :email))
         (fb-email (when (scan +email-scanner+ raw-fb-email)
                     raw-fb-email))
         (fb-name (getf fb-data :name))
         (existing-k-id (gethash fb-id *facebook-id-index*))
         (existing-k-email-id (gethash fb-email *email-index*))
         (host (db (caar (gethash fb-email *invitation-index*)) :host)))
    (cond
      (existing-k-id
        (flash (strcat* "There is already an existing Kindista acccount for "
                        (db existing-k-id :name)
                        ". You are now signed in."))
        (post-login :fb-token-data fb-token-data
                    :fb-data fb-data))

      (existing-k-email-id
        (let* ((userid existing-k-email-id)
               (user (db userid))
               (new-name)
               (new-data))
          (unless (and (eql fb-id (getf user :fb-id))
                       (string= fb-token (getf user :fb-token))
                       (eql fb-expires (getf user :fb-expires)))
            (modify-db userid :fb-id fb-id
                              :fb-token fb-token
                              :fb-expires fb-expires)
            (with-locked-hash-table (*facebook-id-index*)
              (setf (gethash fb-id *facebook-id-index*) userid)))

          (unless (find fb-name (getf user :aliases) :test #'equalp)
            (setf new-name fb-name)
            (asetf new-data
                   (append
                     (list :aliases
                           (append (getf user :aliases)
                                   (list fb-name)))
                     it)))
          (unless (getf user :avatar)
            (asetf new-data
                   (append
                     (list :avatar (save-facebook-profile-picture-to-avatar userid))
                     it)))
          (apply #'modify-db userid new-data)
          (when new-name (reindex-person-names userid))

          (register-login userid)))

      ((not fb-email)
       (flash "Kindista needs a valid email address so we can let you know when someone is replying to an offer or request that you post. After you sign up, you can edit which types of notifications you want to receive from Kindista on your settings page." :error t)
       (see-other "/signup?rerequest-email-permission=t"))
      (t
        (create-new-person-account fb-name
                                   fb-email
                                   :host (or host +kindista-id+)
                                   :fb-id fb-id
                                   :fb-token fb-token
                                   :fb-expires fb-expires)
        (see-other "/home")))))

(defun create-new-person-account
  (name
   email
   &key invitation
        (host +kindista-id+)
        invite-request-id
        fb-id
        fb-token
        fb-expires
   &aux new-id
        (aliases (unless (search (getf invitation :name) name :test #'equalp)
                   (list (getf invitation :name)))))

  (setf new-id
        (if (integerp invite-request-id)
          ;for legacy invitation requests
          ;this can be removed when there are no 
          ;invite requests in *invite-request-index*
          (let ((it invite-request-id))
            (progn
              (modify-db it :type :person
                            :name name
                            :aliases aliases
                            :emails (list email)
                            :host +kindista-id+
                            :active t
                            :help t
                            :pass (new-password (post-parameter "password"))
                            :created (get-universal-time)
                            :notify-gratitude t
                            :notify-message t
                            :notify-kindista t
                            :notify-reminders t
                            :email nil
                            :requested nil)
              (with-locked-hash-table (*db-results*)
                (remhash it *db-results*))
              (when (member it *invite-request-index* :key #'result-id)
                (with-mutex (*invite-request-mutex*)
                 (setf *invite-request-index* (remove it *invite-request-index* :key #'result-id))))
              (index-person it (db it))
              it))
          (create-person :name name
                         :aliases aliases
                         :pending (when (eq host +kindista-id+) t)
                         :host host
                         :email email
                         :fb-id fb-id
                         :fb-token fb-token
                         :fb-expires (awhen (safe-parse-integer fb-expires)
                                       (+ (get-universal-time) it))
                         :password (post-parameter-string "password"))))
  (setf (token-userid *token*) new-id)
  (when fb-id
    (modify-db new-id :avatar (save-facebook-profile-picture-to-avatar new-id)))

  (dolist (group (getf invitation :groups))
    (add-group-member new-id group))
  (add-contact host new-id)
  (unless (eql host +kindista-id+)
    (add-contact +kindista-id+ new-id))
  (with-locked-hash-table (*invited-index*)
    (pushnew new-id (gethash host *invited-index*)))
  ;; see if anyone has invited this email to a group
  ;; posted gratitude
  ;; or added to contacts
  (pending-email-actions email :userid new-id))

