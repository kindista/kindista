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

(defun settings-tabs-html (tab &optional groupid)
  (html
    (:menu :class "bar"
      (unless groupid
        (if (equal tab "personal")
          (htm (:li :class "selected" "Personal Settings"))
          (htm (:li (:a :href "/settings/personal" "Personal Settings")))))
      (when groupid
        (if (equal tab "public")
          (htm (:li :class "selected" "Public Info"))
          (htm (:li (:a :href (url-compose "/settings/public"
                                           "groupid" groupid)
                        "Public Info")))))
      (if (equal tab "communication")
        (htm (:li :class "selected" "Communication Settings"))
        (htm (:li (:a :href (url-compose "/settings/communication"
                                         "groupid" groupid)
                      "Communication Settings")))))))

(defun settings-item-html (item body &key title help-text editable edit-text)
  (html
    (:div :class "settings-item"
      (:div :class "settings-item title" (str (or title
                                                  (string-capitalize item))))
      (:div :class "settings-item content"
        (unless editable
          (htm
            (:div :class "button"
              (:a :class "yes" :href (url-compose *base-url* "edit" item)
                                     (or (str edit-text) (htm "Edit"))))))
        (str body)
        (:p :class "help-text" (:em (str help-text)))))))

(defun settings-avatar (editable &optional groupid)
  (settings-item-html "avatar"
    (cond
      (editable
        (new-image-form "/settings" "/settings/personal" :class "submit-settings" :on groupid))
      (t
        (html
          (:img :class "bigavatar" :src (get-avatar-thumbnail (or groupid *userid*) 300 300)))))

  :editable editable))

(defun settings-name (editable &optional groupid group-name)
  (let ((aliases (unless groupid (getf *user* :aliases))))
    (settings-item-html "name"
      (cond
        (editable
          (html
            (:form :method "post" :action "/settings"
              (:input :type "hidden" :name "next" :value *base-url*)
              (when groupid
                (htm (:input :type "hidden" :name "groupid" :value groupid)))
              (:div :class "submit-settings"
                (:button :class "cancel" :type "submit" :class "submit" :name "cancel" "Cancel")
                (:button :class "yes" :type "submit" :class "submit" :name "submit" "Submit"))
              (:ul
                (:li (:span (:input :type "text"
                                    :name "name"
                                    :value (str (or group-name
                                                    (getf *user* :name)))))
                     (unless groupid
                       (htm (:span (:strong "display name")))))
                (unless groupid
                  (loop for i to 3
                        do (htm (:li
                                  (:span (:input :type "text"
                                                 :name "aliases"
                                                 :value (awhen (nth i aliases)
                                                       (str it))))
                                  (:span "nickname")))))))))
        (t
          (html
            (:ul
              (:li (:span (:strong (str (or group-name (getf *user* :name)))))
                   (when aliases (htm (:span (str "(displayed)")))))
              (dolist (alias aliases)
                (htm (:li (:span (str alias))
                          (:span :class "help-text" "(nickname)"))))))))
    :editable editable
    :help-text (unless groupid
                 (s+ "If you are known by multiple names or nicknames, "
                     "enter up to 5 to help people find you. ")))))

(defun settings-address (editable &optional groupid group)
  (let* ((entity (or group *user*))
         (address (getf entity :address))
         (public-location (awhen (eql (getf group :location-privacy) :public) it)))
    (settings-item-html "address"
      (cond
        (editable
          (html
            (:form :method "post" :class "address" :action "/settings"
             (:input :type "hidden" :name "next" :value *base-url*)
             (when groupid
               (htm (:input :type "hidden" :name "groupid" :value groupid)))
             (:div :class "submit-settings"
               (:button :class "cancel" :type "submit" :class "submit" :name "cancel" "Cancel")
               (:button :class "yes" :type "submit" :class "submit" :name "confirm-address" "Submit"))
             (:input :type "text" :name "address" :value (str address))
             (when groupid
               (htm
                 (:br)
                 (:input :type "checkbox"
                         :name "public-location"
                         :value (str (when public-location "checked")))
                 "Display this address publicly to anyone looking at this group's profile page.")))))
        (t
          (if (and address (getf entity :location))
            (html (:p (str address)))
            (html (:p (:strong "You have not set your address yet."))))))
    :editable editable
    :edit-text (unless address "Add address")
    :help-text (cond
                 (groupid
                   (html
                     (if public-location
                       (htm "This address is currently "
                             (:strong "set to be displayed publicly ")
                             "to anyone viewing "
                             (str (s+ (getf entity :name) "'s"))
                             " profile. ")
                       (htm "This address is currently "
                             (:strong "set to be hidden. ")
                             "It is not visible to other Kindista users and is only being "
                             "used to calculate distances for offers, requests, "
                             " and other activity."))
                      (unless editable
                        (htm "You can choose whether or not this address will be displayed "
                              "publicly by clicking on the \"Edit\" button."))))
                 (t
                   (html "Addresses help people find nearby offers and requests. "
                         "Your address will never be displayed or shared; "
                         "it is used only to calculate distance. "))))))

(defun get-verify-address (&key next-url)
  (let* ((next (or next-url (get-parameter "next")))
         (groupid (when (scan +number-scanner+ (get-parameter "groupid"))
                    (parse-integer (get-parameter "groupid"))))
         (group (when groupid (db groupid)))
         (entity (or group *user*)))
    (if (or (not groupid)
            (member *userid* (getf group :admins)))
      (standard-page
        (s+ "Please verify "
            (aif (getf group :name) (s+ it "'s") "your")
            " location.")
        (html
          (:div :class "item"
            (:div :class "setup"
              (:h2 "Verify "
                   (str (aif (getf group :name) (s+ it "'s") "your"))
                   " location")
              (:p (str
                    (if groupid
                      (s+ "The address for this group is currently set to be "
                          (if (eql (getf entity :location-privacy) :public)
                            "displayed publicly" "hidden")
                          ". You can change the visibility of this address on your group's settings page. ")
                    (s+ "We will never share your exact location with anyone else.")))
                  "If you would like to know more about how we use the information you share with us,
                   please read our " (:a :href "/privacy" "privacy policy") ".")
              (str (static-google-map :size "280x150" :zoom 12 :lat (getf entity :lat) :long (getf entity :long)))

              (:form :method "post" :action "/settings"
                (:h3 "Is this location correct?")
                (:input :type "hidden" :name "next" :value (str next))
                (when groupid
                  (htm (:input :type "hidden" :name "groupid" :value groupid)))
                (:button :class "cancel"
                         :type "submit"
                         :name "reset-location"
                         :value (referer)
                         "No, go back")
                (:button :class "yes"
                         :type "submit"
                         :name "confirm-location"
                         :value "1"
                         "Yes, this is correct"))))))
        (permission-denied))))

(defun settings-password ()
  (settings-item-html "password"
    (html
      (:form :method "post" :class "password" :autocomplete "off" :action "/settings"
       (:input :type "hidden" :name "next" :value *base-url*)
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

(defun settings-donate ()
  (let ((plan (getf *user* :plan)))
    (settings-item-html "donate"
      (html
        (:form :method "post" :class "password" :action "/settings"
         (:input :type "hidden" :name "next" :value *base-url*)
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

(defun settings-deactivate ()
  (let ((action (if (eq (getf *user* :active) t)
                  "deactivate"
                  "reactivate"))) 
    (settings-item-html action
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

(defun settings-emails (editable &key activate)
  (let* ((emails (getf *user* :emails))
         (alternates (cdr emails))
         (pending (getf *user* :pending-alt-emails)))
    (settings-item-html "email"
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
                        (:button :class "simple-link gray"
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

(defun settings-notifications (&optional groupid group)
  (let ((entity (or group *user*))
        (group-name (when group (getf group :name))))
    (labels ((checkbox-value (notify-key)
               (when (or (and (not group)
                              (getf *user* notify-key))
                         (member *userid* (getf entity notify-key)))
                             "checked")))
     (settings-item-html "notifications"
      (html
        (:form :method "post" :action "/settings"
          (:input :type "hidden" :name "next" :value *base-url*)
          (when groupid
            (htm (:input :type "hidden" :name "groupid" :value groupid)))
          (:div :class "submit-settings"
            (:button :class "yes" :type "submit" :class "submit" :name "save-notifications" "Save notification preferences"))
          (:ul
            (:li (:input :type "checkbox"
                  :name "gratitude"
                  :checked (checkbox-value :notify-gratitude))
                 "when someone posts gratitude about "
                 (str (if group group-name "me")))
            (unless group
              (htm
                 (:li (:input :type "checkbox"
                       :name "message"
                       :checked (when (getf entity :notify-message) "checked"))
                      "when someone sends me a message")
                (:li (:input :type "checkbox"
                      :name "expired-invites"
                      :checked (when (getf entity :notify-expired-invites) "checked"))
                     "when invitatations I send for my friends to join Kindista expire")
                (:li (:input :type "checkbox"
                      :name "reminders"
                      :checked (when (getf entity :notify-reminders) "checked"))
                     "with occasional suggestions about how "
                     (str (if group "our group" "I"))
                     " can get the most out of Kindista")
                (:li (:input :type "checkbox"
                       :name "kindista"
                       :checked (when (getf entity :notify-kindista) "checked"))
                      "with updates and information about Kindista"))))))

    :title "Notify me"
    :editable t))))

(defun settings-identity-selection-html (selected groups)
"Groups should be an a-list of (groupid . group-name)"
  (html
    (:form :method "post" :action "/settings"
      (:strong "Viewing settings for: ")
      (str (identity-selection-html selected groups :onchange "this.form.submit()"))
      " "
      (:input :type "submit" :class "no-js" :value "apply"))))

(defun go-settings ()
  (aif (get-parameter "groupid")
    (see-other (url-compose "/settings/public"
                            "groupid" it))
    (see-other "/settings/personal")))

(defmacro settings-template-html (base-url &body body)
  `(require-user
     (let* ((edit (get-parameter "edit"))
            (groupid (when (scan +number-scanner+ (get-parameter "groupid"))
                       (parse-integer (get-parameter "groupid"))))
            (group (db groupid))
            (group-name (getf group :name))
            (*base-url* ,base-url))
       (declare (ignorable group-name edit))
       (if (or (not groupid)
               (member *userid* (getf group :admins)))
         (standard-page
           (if (not groupid)
             "Personal settings"
             (s+ (getf group :name) " settings"))
           (html
             (:div :class "settings"
              (awhen (groups-with-user-as-admin)
                (str (settings-identity-selection-html (or groupid *userid*)
                                                       it)))
              (str ,@body)))
           :selected :settings)
         (permission-denied)))))

(defun get-settings ()
  (settings-template-html
    (aif (get-parameter "groupid")
      (url-compose "/settings/public"
                   "groupid" it)
      "/settings/personal")
    (if (and (not (get-parameter "groupid"))
             (string= (script-name*) "/settings/public"))
        (see-other "/settings/personal")
        (html
          (str (settings-tabs-html (if (not groupid) "personal" "public")
                                   (awhen groupid it)))
          (when groupid
            (str (settings-item-html
                   "profile"
                   (html
                     (:a :href (strcat "/groups/" (username-or-id groupid))
                       (str (strcat +base-url+
                                    "groups/"
                                    (username-or-id groupid))))))))
          (str (settings-avatar (string= edit "avatar") groupid))
          (str (settings-name (string= edit "name")
                              groupid
                              (getf group :name)))
          (str (settings-address (string= edit "address") groupid group))
          (when (not groupid)
            (htm (str (settings-password))
                 (when (getf *user* :plan)
                   (str (settings-donate))))
            (str (settings-deactivate)))))))

(defun get-settings-communication ()
  (require-user
    (cond
      ((and (scan +number-scanner+ (get-parameter "invitation-id"))
            (get-parameter "token"))

       (activate-email-address (parse-integer (get-parameter "invitation-id"))
                               (get-parameter "token")))
      (t
       (settings-template-html
         (aif (get-parameter "groupid")
           (url-compose "/settings/communication"
                        "groupid" it)
           "/settings/communication")
         (html
          (str (settings-tabs-html "communication" (awhen groupid it)))
          (:p "We'll email you whenever something happens on Kindista that involves "
              (str (or group-name "you"))". "
              "You can specify which actions you would like to be notified about.")
          (:p "Notifications will be sent to your primary email address: "
              (:strong (str (car (getf *user* :emails)))))
          (str (settings-notifications groupid group))
          (unless groupid
            (str (settings-emails (string= (get-parameter "edit") "email")
                                  :activate (get-parameter "activate"))))))))))

(defun get-settings-error ()
  (flash "The avatar you uploaded is too large. Please upload an image smaller than 10MB." :error t)
  (go-settings))

(defun post-settings ()
  (require-user
    (let* ((groupid (or (post-parameter "on")
                        (post-parameter "groupid")))
           (id (or (when groupid
                     (unless (string= groupid "")
                       (parse-integer groupid)))
                   *userid*))
           (entity (if (eql id *userid*) *user* (db id))))

      (if (or (eql id *userid*)
              (member *userid* (getf entity :admins)))
        (acond
          ((post-parameter "cancel")
           (see-other (or (post-parameter "next") "/home")))

          ((post-parameter "identity-selection")
           (if (eql (parse-integer it) *userid*)
             (see-other "/settings/personal")
             (see-other (url-compose "/settings/public"
                                     "groupid" it))))

          ((post-parameter "image")
           (let ((groupid (awhen (post-parameter "on") (parse-integer it))))
             (handler-case
               ;hunchentoot returns a list containing
               ;(path file-name content-type)
               ;when the post-parameter is a file, i.e. (first it) = path
               (let* ((id (create-image (first it) (third it)))
                      (old-avatar (or (db groupid :avatar) (getf *user* :avatar))))
                 (when (integerp old-avatar)
                   (delete-image old-avatar))
                 (modify-db (or groupid *userid*) :avatar id))
               (t () (flash "Please use a .jpg, .png, or .gif" :error t)))
             (see-other (if groupid
                          (url-compose "/settings/public"
                                       "groupid" (post-parameter "on"))
                          "/settings/personal"))))

          ((post-parameter "name")
           (cond
             ((if groupid
                (scan +text-scanner+ it)
                (validate-name it))
              (let ((aliases (remove-duplicates
                               (loop for (x . y) in (post-parameters*)
                                     when (and (string= x "aliases")
                                               (not (string= y "")))
                                     collect y)
                               :test #'string=)))
                (cond
                  ((and groupid
                        (not (equal (getf entity :name) it)))
                   (modify-db id :name it)
                   (reindex-group-name id))
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
           (cond
             ((and groupid
                    (string= it (getf entity :address))
                    (getf entity :location)
                    (getf entity :lat)
                    (getf entity :long))
              (modify-db id :location-privacy (if (post-parameter "public-location") :public :private))
              (see-other (or (post-parameter "next") "/home")))
             (t
              (multiple-value-bind (lat long address city state country street zip)
                (geocode-address it)
                (modify-db id :lat lat
                              :long long
                              :address address
                              :city city
                              :state state
                              :street street
                              :zip zip
                              :country country
                              :location nil
                              :location-privacy (if (post-parameter "public-location") :public :private)))
              (see-other (if groupid
                           (url-compose "/settings/verify-address"
                                        "groupid" id
                                        "next" (or (post-parameter "next") "/home"))
                           (url-compose "/settings/verify-address"
                                        "next" (or (post-parameter "next") "/home")))))))

          ((and (post-parameter "confirm-location")
                (getf entity :lat)
                (getf entity :long))
           (modify-db id :location t)
           (case (getf entity :type)
             (:person (reindex-person-location *userid*))
             (:group (reindex-group-location id)))
           (see-other (or (post-parameter "next") "/home")))

          ((post-parameter "reset-location")
           (modify-db id :lat nil :long nil :address nil :location nil)
           (see-other (or it "/home")))

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
           (when (and (getf *user* :notify-message)
                      (not groupid)
                      (not (post-parameter "message")))
             (flash "Warning: You will not recieve any email notifications when people reply to your Offers and Requests unless you choose to be notified \"when someone sends me a message\"!" :error t))
           (if groupid
             (amodify-db id :notify-gratitude (if (post-parameter "gratitude")
                                                (pushnew *userid* it)
                                                (remove *userid* it)))
             (modify-db *userid* :notify-gratitude (when (post-parameter "gratitude") t)
                                 :notify-message (when (post-parameter "message") t)
                                 :notify-reminders (when (post-parameter "reminders") t)
                                 :notify-expired-invites (when (post-parameter "expired-invites") t)
                                 :notify-kindista (when (post-parameter "kindista") t)))
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
           (see-other (or (referer) "/home"))))

      (permission-denied)))))
