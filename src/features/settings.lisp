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

(defun settings-tabs-html (tab)
  (html
    (:menu :class "bar"
      (if (equal tab "personal")
        (htm (:li :class "selected" "Personal Settings"))
        (htm (:li (:a :href "/settings/personal" "Personal Settings"))))
      (if (equal tab "communication")
        (htm (:li :class "selected" "Communication Settings"))
        (htm (:li (:a :href "/settings/communication" "Communication Settings")))))))

(defun settings-item-html (base item title body &key help-text editable edit-text)
  (html
    (:div :class "settings-item"
      (:div :class "settings-item title" (str title))
      (:div :class "settings-item content"
        (unless editable
          (htm
            (:div :class "button"
              (:a :class "yes" :href (s+ base "?edit=" item)
                                     (or (str edit-text) (htm "Edit"))))))
        (str body)
        (:p :class "help-text" (:em (str help-text)))))))

(defun settings-avatar (base editable)
  (settings-item-html
    base "avatar" "Avatar"
    (cond
      (editable
        (html
          (:form :method "post" :action "/settings" :enctype "multipart/form-data"
            (:input :type "hidden" :name "next" :value "/settings/personal")
            (:div :class "submit-settings"
              (:button :class "cancel" :type "submit" :class "submit" :name "cancel" "Cancel")
              (:button :class "yes" :type "submit" :class "submit" :name "submit" "Submit"))
            (:input :type "file" :name "avatar"))))
      (t
        (html
          (:img :src (strcat +avatar-base+ *userid* ".jpg?" (get-universal-time))))))

  :editable editable))

(defun settings-name (base editable)
  (let ((aliases (getf *user* :aliases)))
    (settings-item-html
      base "name" "Name"
      (cond
        (editable
          (html
            (:form :method "post" :action "/settings"
              (:input :type "hidden" :name "next" :value "/settings/personal")
              (:div :class "submit-settings"
                (:button :class "cancel" :type "submit" :class "submit" :name "cancel" "Cancel")
                (:button :class "yes" :type "submit" :class "submit" :name "submit" "Submit"))
              (:ul
                (:li (:span (:input :type "text"
                                    :name "name"
                                    :value (str (getf *user* :name))))
                     (:span (:strong "display name")))
                (loop for i to 3
                      do (htm (:li
                                (:span (:input :type "text"
                                                :name "aliases"
                                                :value (awhen (nth i aliases)
                                                      (str it))))
                                (:span "nickname"))))))))
        (t
          (html
            (:ul
              (:li (:span (:strong (str (getf *user* :name))))
                   (when aliases (htm (:span (str "(displayed)")))))
              (dolist (alias aliases)
                (htm (:li (:span (str alias))
                          (:span :class "help-text" "(nickname)"))))))))
    :editable editable
    :help-text (s+ "If you are known by multiple names or nicknames, "
                   "enter up to 5 to help people find you. "))))

(defun settings-address (base editable)
  (let ((address (getf *user* :address)))
    (settings-item-html base "address" "Address"
    (cond
      (editable
        (html
          (:form :method "post" :class "address" :action "/settings"
           (:input :type "hidden" :name "next" :value "/settings/personal")
           (:div :class "submit-settings"
             (:button :class "cancel" :type "submit" :class "submit" :name "cancel" "Cancel")
             (:button :class "yes" :type "submit" :class "submit" :name "confirm-address" "Submit"))
           (:input :type "text" :name "address" :value (str address)))))
      (t
        (aif address
          (html (:p (str it)))
          (html (:p (:strong "You have not set your address yet."))))
            ))
    :editable editable
    :edit-text (unless address "Add address")
    :help-text (s+ "Addresses help people find nearby offers and requests. "
                   "Your address will never be displayed or shared; "
                   "it is used only to calculate distance. "))))

(defun get-verify-address (&key next-url)
  (let ((next (or next-url (get-parameter "next"))))
    (standard-page
      "Please verify your location."
      (html
        (:div :class "item"
          (:div :class "setup"
            (:h2 "Verify your location")
            (:p "We will never share your exact location with anyone else.
                 If you would like to know more about how we use the information you share with us,
                 please read our " (:a :href "/privacy" "privacy policy") ".")
            (str (static-google-map :size "280x150" :zoom 12 :lat (getf *user* :lat) :long (getf *user* :long)))

            (:form :method "post" :action "/settings"
              (:h3 "Is this location correct?")
              (:input :type "hidden" :name "next" :value (str next))
              (:button :class "cancel"
                       :type "submit"
                       :name "reset-location"
                       :value "1"
                       "No, go back")
              (:button :class "yes"
                       :type "submit"
                       :name "confirm-location"
                       :value "1"
                       "Yes, this is correct"))))))))

(defun settings-password (base)
  (settings-item-html base "password" "Password"
    (html
      (:form :method "post" :class "password" :autocomplete "off" :action "/settings"
       (:input :type "hidden" :name "next" :value "/settings/personal")
       (:div :class "submit-settings"
         (:button :class "yes" :type "submit" :class "submit" "Change password"))
       (:div
         (:label "Current password:")
         (:input :type "password"
                 :name "password"
                 :placeholder "verify your current password"))
       (:div
         (:label "New password:")
         (:input :type "password"
                 :name "new-password-1"
                 :placeholder "new password: at least 8 characters"))   
       (:div
         (:label "Confirm your new password:")
         (:input :type "password"
                 :name "new-password-2"
                 :placeholder "please retype your new password"))))   

  :editable t
  :help-text (s+ "Minimum of 8 characters. "
                 "We strongly recommend using either a mix of upper- and "
                 "lower-case letters, numbers, and symbols; or a sentence "
                 "of at least 8 words.")))

(defun settings-donate (base)
  (let ((plan (getf *user* :plan)))
    (settings-item-html base "donate" "Donate"
      (html
        (:form :method "post" :class "password" :action "/settings"
         (:input :type "hidden" :name "next" :value "/settings/personal")
         (:div :class "submit-settings"
           (:button :class "cancel" :type "submit" :class "submit" :name "cancel-plan" "Cancel plan")
           (:button :class "yes" :type "submit" :class "submit" "Change plan"))
         (:div
           (:label "Current monthly donation: " (:strong "$" (str plan)))
           (:select :name "plan"
             (:option :disabled "disabled" "Select a new plan")
             (:option :value "5" "$5/month")
             (:option :value "10" "$10/month")
             (:option :value "20" "$20/month")
             (:option :value "35" "$35/month")
             (:option :value "50" "$50/month")
             (:option :value "100" "$100/month")))))

    :editable t
    :help-text (s+ "Your monthly donation is specified in US Dollars. Changes take place on your "
                   "next monthly bill&mdash;we do not prorate plan changes. Thank you for your "
                   "financial support!"))))

(defun settings-deactivate (base)
  (let ((action (if (eq (getf *user* :active) t)
                  "deactivate"
                  "reactivate"))) 
    (settings-item-html base action (string-capitalize action)
      (html
        (:form :method "post" :action "/settings"
          (:button :class "link no-padding green" 
                   :name action
                   :type "submit"
                   (str (s+ (string-capitalize action) " account")))))
    :editable t
    :help-text (s+ "Warning: "
                   "Deactivating your account will delete all of your current offers "
                   "and requests, and prevent people from "
                   "contacting you through Kindista or finding you using the search bar. "
                   "Deactivating your account will not remove any statements of gratitude "
                   "you have given or received. "
                   "You may reactivate your account anytime by logging into Kindista and "
                   "clicking \"Reactivate account\" on this page."))))

(defun confirm-deactivation ()
  (standard-page
    "Confirm Deactivation"
    (html
      (:div :class "item"
        (:h1 "Are you sure you want to deactivate your account?")
        (:p
          (:strong "Warning: ")
          "Deactivating your account will delete all of your current offers " 
          "and requests, and prevent people from "
          "contacting you through Kindista or finding you using the search bar. "
          "Deactivating your account will not remove any statements of gratitude "
          "you have given or received. "
          "You may reactivate your account anytime by logging into Kindista and "
          "clicking \"Reactivate account\" on this page.")  
        (:form :method "post" :action "/settings"
          (:input :type "hidden" :name "next" :value "/settings")
          (:a :class "cancel" :href "/settings" "No, I didn't mean it!")
          (:button :class "yes" 
                   :type "submit" 
                   :class "submit" 
                   :name "confirm-deactivation" 
                   "Yes, deactivate my Kindista account."))
        ))
    :class "text"))

(defun settings-emails (base editable &key activate)
  (let* ((emails (getf *user* :emails))
         (alternates (cdr emails))
         (pending (getf *user* :pending-alt-emails)))
    (settings-item-html base "email" "Email"
      (html
        (:form :method "post" :action "/settings"
          (:input :type "hidden" :name "next" :value "/settings/communication")
          (:ul
            (:li (:span :class "email-item" (:strong (str (car emails))))
                 (:span :class "help-text" (:em "primary email")))
            (dolist (email alternates)
              (htm (:li (:span :class "email-item" (str email))
                        (:button :class "simple-link green"
                                 :name "make-email-primary"
                                 :type "submit"
                                 :value email
                                 "Make primary")
                        " | "
                        (:button :class "simple-link red"
                                 :name "remove-email"
                                 :type "submit"
                                 :value email
                                 "Remove")
                        )))
            (dolist (invite-id pending)
              (let ((email (getf (db invite-id) :recipient-email)))
                (htm
                  (:li
                    (:span :class "email-item" (str email))
                    (:input :type "hidden"
                            :name "invitation-id" :value invite-id)
                    (cond
                      ((string= email activate)
                       (htm
                         (:span
                           (:input :type "text"
                                   :name "token"
                                   :placeholder "please enter your activation code"))
                         (:button :class "yes"
                                  :type "submit"
                                  :class "submit"
                                  "Activate")
                         (:a :class "red" :href "/settings/communication" "Cancel")))
                      (t
                       (htm
                         (:span :class "red" "(pending)")
                         (:a :href (url-compose "/settings/communication"
                                                "edit" "email"
                                                "activate" email)
                             "Enter code")
                         (when (not editable)
                           (htm
                             " | "
                             (:button :type "submit"
                                      :class "simple-link "
                                      :name "resend-code"
                                      :value email
                                      "Resend code")))))))))))

          (cond
            ((not editable)
             (htm (:p (:a :href "/settings/communication?edit=email" "add another email address"))))
            ((not activate)
              (htm
                (:li
                  (:input :type "text"
                          :name "new-email"
                          :placeholder "new alternate email")
                  (:button :class "yes" :type "submit" :class "submit" :name "submit" "Confirm new email")   
                  (:a :class "red" :href "/settings/communication" "Cancel"))) ))))

      :editable T
      :help-text (s+ "Adding additional email address helps people find you "
                     "and keeps you from getting invites to Kindista at "
                     "your other addresses. "
                     "Kindista will never show your email addresses to anyone. "
                     "Only your primary email address will receive "
                     "notifications." ))))

(defun activate-email-address (invitation-id test-token)
  (let* ((invitation (db invitation-id))
         (true-token (getf invitation :token))
         (email (getf invitation :recipient-email)))
    (cond
     ((not (eq (getf invitation :host) *userid*))
      (flash (s+ "It appears that the email address you are trying to "
                 "verify belongs to another user. "
                 "If you are the actual owner of " email
                 " please contact us to correct this error.")
             :error t)
      (see-other (url-compose "/settings/communication"
                              "edit" "email"
                              "activate" email)))
    
    ((or (not (eq (getf invitation :type) :invitation))
         (not (string= test-token true-token)))
     (flash (s+ "The activation code you have entered does not match "
                "this email address. "
                "If you have received multiple verification emails for "
                "this email address, please enter the code from the most "
                "recent email you received.")
            :error t)
     (see-other (url-compose "/settings/communication"
                             "edit" "email"
                             "activate" email)))
    (t
     (add-alt-email invitation-id)
     (flash (s+ "You have successfully added " email
                " to your Kindista account."))
     (see-other "/settings/communication")))))

(defun settings-notifications (base)
  (settings-item-html base "notifications" "Notify me"
    (html
      (:form :method "post" :action "/settings"
        (:input :type "hidden" :name "next" :value "/settings/communication")
        (:div :class "submit-settings"
          (:button :class "yes" :type "submit" :class "submit" :name "save-notifications" "Save notification preferences"))
        (:ul
          (:li (:input :type "checkbox"
                :name "gratitude"
                :checked (when (getf *user* :notify-gratitude) "checked"))
               "when someone posts gratitude about me")
          (:li (:input :type "checkbox"
                :name "message"
                :checked (when (getf *user* :notify-message) "checked"))
               "when someone sends me a message")
          (:li (:input :type "checkbox"
                :name "reminders"
                :checked (when (getf *user* :notify-reminders) "checked"))
               "with occasional suggestions about how I can get the most out of Kindista")
          (:li (:input :type "checkbox"
                :name "kindista"
                :checked (when (getf *user* :notify-kindista) "checked"))
               "with updates and information about Kindista"))))

    :editable t))

(defun get-settings ()
  (require-user
    (let ((base "/settings/personal")
          (edit (get-parameter "edit")))
      (standard-page
        "Personal settings"
        (html
          (str (settings-tabs-html "personal"))
          (str (settings-avatar base (string= edit "avatar")))
          (str (settings-name base (string= edit "name")))
          (str (settings-address base (string= edit "address")))
          (str (settings-password base))
          (when (getf *user* :plan) (str (settings-donate base)))
          (str (settings-deactivate base)))
        :selected :settings))))

(defun get-settings-error ()
  (flash "The avatar you uploaded is too large. Please upload an image smaller than 10MB." :error t)
  (go-settings))

(defun go-settings ()
  (see-other "/settings/personal"))

(defun get-settings-communication ()
  (require-user
    (cond
      ((and (scan +number-scanner+ (get-parameter "invitation-id"))
            (get-parameter "token"))

       (activate-email-address (parse-integer (get-parameter "invitation-id"))
                               (get-parameter "token")))
      (t
       (let ((base "/settings/communication"))
         (standard-page
           "Communication settings"
           (html
             (:div :class "settings"
               (str (settings-tabs-html "communication"))
               (:p "We'll email you whenever something happens on Kindista that "
                   "involves you. You can specify which actions you would like "
                   "to be notified about.")
               (:p "Notifications will be sent to your primary email address: "
                 (:strong (str (car (getf *user* :emails)))))


               (str (settings-notifications base))

               (str (settings-emails base
                                     (string= (get-parameter "edit") "email")
                                     :activate (get-parameter "activate")))))
           :selected :settings))))))

(defun post-settings ()
  (require-user
    (acond
      ((post-parameter "cancel")
       (see-other (or (post-parameter "next") "/home")))

      ((post-parameter "name")
       (cond
         ((validate-name it)
          (let ((aliases (remove-duplicates
                           (loop for (x . y) in (post-parameters*) 
                                 when (and (string= x "aliases")
                                           (not (string= y ""))) 
                                 collect y)
                           :test #'string=)))
            (cond
              ((and (not (equal (getf *user* :aliases) aliases))
                    (equal (getf *user* :name) it))
               (modify-db *userid* :aliases aliases)
               (reindex-person-names *userid*))
              ((and (equal (getf *user* :aliases) aliases)
                    (not (equal (getf *user* :name) it)))
               (modify-db *userid* :name it)
               (reindex-person-names *userid*))
              ((and (not (equal (getf *user* :aliases) aliases))
                    (not (equal (getf *user* :name) it)))
               (modify-db *userid* :name it :aliases aliases)
               (reindex-person-names *userid*)))))
         (t
           (flash "You must use your true full name (first and last) for your primary name on Kindista.  Single word names are permitted for your nicknames." :error t))) 
       (see-other (or (post-parameter "next") "/home")))

      ((post-parameter "address")
       (multiple-value-bind (lat long address city state country street zip)
         (geocode-address it)
         (modify-db *userid* :lat lat :long long :address address :city city :state state :street street :zip zip :country country :location nil))
       (see-other (url-compose "/settings/verify-address" 
                               "next" (or (post-parameter "next") "/home")))) 

      ((post-parameter "avatar")
       (handler-case
         (let ((id (create-image (first it) (third it))))
           (modify-db *userid* :avatar id))
         (t () (flash "Please use a .jpg, .png, or .gif" :error t)))
       (see-other "/settings/personal"))

      ((post-parameter "reset-location")
       (modify-db *userid* :lat nil :long nil :address nil :location nil)
       (see-other (or (post-parameter "next") "/home")))

      ((post-parameter "password")
       (cond
         ((not (password-match-p *userid* (post-parameter "password")))
          (flash "The password you entered is incorrect. Please try again." :error t)
          (see-other (post-parameter "next")))
         ((< (length (post-parameter "new-password-1")) 8)
          (flash "Your new password is too short. Please use at least 8 characters." :error t)
          (see-other (post-parameter "next")))
         ((not (string= (post-parameter "new-password-1")
                        (post-parameter "new-password-2")))
          (flash "The confirmation text you entered does not match the new password you entered. Please try again." :error t)
          (see-other (post-parameter "next")))
         (t
          (modify-db *userid* :pass (new-password (post-parameter "new-password-1")))
          (flash "You have successfully changed your password.")
          (see-other (or (post-parameter "next") "/home")))))

      ((post-parameter "deactivate")
       (flash "Please confirm your choice to deactivate your account." :error t)
       (see-other "/deactivate-account"))

      ((post-parameter "confirm-deactivation")
       (deactivate-person *userid*)
       (flash "You have deactivated you account. If you change your mind you can reactivate your account on the settings page.")

       (see-other "/settings/personal"))

      ((post-parameter "cancel-plan")

       (acond
         ((and (getf *user* :plan)
               (getf *user* :custid))

          (handler-case
            (progn (stripe:delete-subscription it)
                   (flash "Your plan has been cancelled, effective immediately. Thank you for your financial support!") 
                   (notice :cancel-plan :plan (getf *user* :plan) :custid (getf *user* :custid)) 
                   (modify-db *userid* :plan nil) 
                   (see-other "/settings/personal"))
            (t (err)
               (declare (ignore err))
               (flash "There was an error deleting your subscription. Humans have been notified!" :error t)
               (notice :error :on :cancel-plan :custid (getf *user* :custid)))))

         (t
          (flash "You do not currently have an active subscription.")
          (see-other "/settings/personal"))))

      ((post-parameter "plan")

       (acond
         ((and (getf *user* :plan)
               (getf *user* :custid))

          (let ((plan (parse-integer (post-parameter "plan"))))
            (stripe:update-subscription it
              :plan (make-donation-plan (* 100 plan))
              :prorate nil)
            (notice :change-plan :old (getf *user* :plan) :new plan :custid (getf *user* :custid))
            (modify-db *userid* :plan plan) 
            (flash "We've updated your subscription. Thank you very much for your financial support!") 
            (see-other "/settings/personal")))))

      ((post-parameter "reactivate")
       (reactivate-person *userid*)
       (flash "You have reactivated your Kindista account.")
       (see-other "/settings/personal"))

      ((post-parameter "save-notifications")
       (modify-db *userid* :notify-gratitude (when (post-parameter "gratitude") t))
       (modify-db *userid* :notify-message (when (post-parameter "message") t))
       (modify-db *userid* :notify-reminders (when (post-parameter "reminders") t))
       (modify-db *userid* :notify-kindista (when (post-parameter "kindista") t))
       (flash "Your notification preferences have been saved.")
       (see-other (or (post-parameter "next") "/home")))

      ((post-parameter "new-email")
       (let* ((new-email (post-parameter "new-email"))
              (id (gethash new-email *email-index*))
              (emails (getf *user* :emails))
              (pending (awhen (getf *user* :pending-alt-emails) it)))
         (cond
           ((equal (car emails) new-email)
            (flash (s+ new-email " is already your primary email address. "
                       "If you want to use " new-email
                       " as an alternate email address, you must first "
                       "set another email to be your primary email "
                       "address.") :error t)
            (see-other "/settings/communication?edit=email")) 
           ((member new-email emails)
            (see-other "/settings/communication?edit=email")) 
           (id
            (flash (s+ "The email address you have submitted, " new-email
                       ", already belongs to another Kindista member. "
                       "Please contact us if " new-email
                       " is actually your email address.") :error t)
            (see-other "/settings/communication?edit=email")) 
           ((not (scan +email-scanner+ new-email))
            (flash (s+ new-email " is not a valid email address. "
                       "Please try again.") :error t)
            (see-other "/settings/communication?edit=email"))
           (t
            (let ((confirmation nil))
              (setf confirmation (create-invitation new-email :self t))
              (modify-db *userid* :pending-alt-emails (cons confirmation pending)))
            (flash (s+ "A verification email has been sent to " new-email
                       ". Please click on the link provided in that email "
                       "to complete the email verification process."))
            (see-other "/settings/communication")))))

      ((and (post-parameter "invitation-id")
            (post-parameter "token"))
       (activate-email-address (parse-integer (post-parameter "invitation-id"))
                               (post-parameter "token")))

      ((post-parameter "resend-code")
       (let* ((id (parse-integer (post-parameter "invitation-id")))
              (invitation (db id))
              (email (getf invitation :recipient-email)))
         (cond
           ((< (getf invitation :valid-until) (get-universal-time))
            (modify-db id :valid-until (+ (get-universal-time) 2592000))
            (send-email-verification id))
           (t
            (send-email-verification id)))
       (flash (s+ "Your activation code has been resent to " email "."))
       (see-other "/settings/communication")))
     
      ((post-parameter "make-email-primary")
       (let ((new-primary (post-parameter "make-email-primary")))
         (amodify-db *userid* :emails
                              (cons new-primary
                                    (remove new-primary it :test #'string=)))
         (flash (s+ new-primary " is now your primary email addrss."))
         (see-other "/settings/communication")))

      ((post-parameter "remove-email")
       (let ((email (post-parameter "remove-email")))
         (amodify-db *userid* :emails (remove email it :test #'string=))
         (remhash email *email-index*)
         (flash (s+ email " has been removed from your Kindista account."))
         (see-other "/settings/communication")))

      ((post-parameter "bio-doing")
       (unless (getf *user* :bio)
         (modify-db *userid* :bio t))
       (modify-db *userid* :bio-doing (post-parameter "bio-doing"))
       (see-other (or (post-parameter "next") "/home")))

      ((post-parameter "bio-summary")
       (unless (getf *user* :bio)
         (modify-db *userid* :bio t))
       (modify-db *userid* :bio-summary (post-parameter "bio-summary"))
       (see-other (or (post-parameter "next") "/home")))

      ((post-parameter "bio-into")
       (unless (getf *user* :bio)
         (modify-db *userid* :bio t))
       (modify-db *userid* :bio-into (post-parameter "bio-into"))
       (see-other (or (post-parameter "next") "/home")))

      ((post-parameter "bio-contact")
       (unless (getf *user* :bio)
         (modify-db *userid* :bio t)) (modify-db *userid* :bio-contact (post-parameter "bio-contact"))
       (see-other (or (post-parameter "next") "/home")))

      ((post-parameter "bio-skills")
       (unless (getf *user* :bio)
         (modify-db *userid* :bio t))
       (modify-db *userid* :bio-skills (post-parameter "bio-skills"))
       (see-other (or (post-parameter "next") "/home")))

      ((scan +number-scanner+ (post-parameter "rdist"))
       (modify-db *userid* :rdist (parse-integer (post-parameter "rdist")))
       (flash "Your search distance for offers and requests has been changed!")
       (see-other (or (post-parameter "next") "/requests")))

      ((scan +number-scanner+ (post-parameter "sdist"))
       (modify-db *userid* :rdist (parse-integer (post-parameter "sdist")))
       (flash "Your default search distance has been changed!")
       (see-other (or (post-parameter "next") "/requests")))

      ((scan +number-scanner+ (post-parameter "distance"))
       (modify-db *userid* :distance (parse-integer (post-parameter "distance")))
       (if (string= (post-parameter "distance") "0")
         (flash "Now showing world-wide activity.")
         (flash (format nil "Now showing activity within ~a miles." (post-parameter "distance"))))
       (see-other (or (post-parameter "next") "/home")))

      ((equalp (post-parameter "help") "0")
       (modify-db *userid* :help nil)
       (see-other (or (referer) "/home")))

      ((equalp (post-parameter "help") "1")
       (modify-db *userid* :help t)
       (see-other (or (referer) "/home")))

      ((and (post-parameter "confirm-location")
            (getf *user* :lat)
            (getf *user* :long))
       (modify-db *userid* :location t)
       (reindex-person-location *userid*)
       (see-other (or (post-parameter "next") "/home"))))))
