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

(defun add-notify-inventory-expiration (&aux (people-modified 0) (groups-modified 0))
  (dolist (id (hash-table-keys *db*))
    (let* ((data (db id))
           (user-db (when (eq (getf data :type) :person) data))
           (group-db (when (eq (getf data :type) :group) data))
           (admins (getf group-db :admins))
           (user-notify-message-p (getf user-db :notify-message))
           (user-notify-expiration-p (getf user-db :notify-inventory-expiration )))
      (when (and user-notify-message-p
                (not user-notify-expiration-p))
                ;user has notify-message but not notify-expiration
        (modify-db id :notify-inventory-expiration t)
        (incf people-modified))

      (when group-db
        (modify-db id :notify-inventory-expiration admins)
        (incf groups-modified))))

  (list :groups-modified groups-modified
        :people-modified people-modified))

(defun reset-blog-notification-settings (&aux (count 0))
  "accidentally unsubscribed people from the blog when they were trying to unsubscribe from reminders"
  (dolist (id *active-people-index*)
    (let ((data (db id)))
      (when (and (getf data :notify-kindista)
                 (not (getf data :notify-blog)))
        (modify-db id :notify-blog t))))
  count)

(defun settings-tabs-html (tab &optional groupid)
  (html
    (:menu :type "toolbar ":class "bar"
      (unless groupid
        (if (equal tab "personal")
          (htm (:li :class "selected" "Personal Settings"))
          (htm (:li (:a :href "/settings/personal" "Personal Settings")))))
      (when groupid
        (if (equal tab "public")
          (htm (:li :class "selected" "Public Info"))
          (htm (:li (:a :href (url-compose "/settings/public" "groupid" groupid)
                        "Public Info"))))
        (if (equal tab "admin")
          (htm (:li :class "selected" "Admin Roles"))
          (htm (:li (:a :href (url-compose "/settings/admin-roles" "groupid" groupid)
                        "Admin Roles")))))
      (if (equal tab "communication")
        (htm (:li :class "selected" "Communication Settings"))
        (htm (:li (:a :href (url-compose "/settings/communication" "groupid" groupid)
                      "Communication Settings"))))
      (when (and (not groupid)
                 (or *enable-facebook-login*
                     (find *userid* *alpha-testers*)
                     (getf *user* :test-user)))
        (if (equal tab "social")
          (htm (:li :class "selected" "Social Media Settings"))
          (htm (:li (:a :href "/settings/social" "Social Media Settings"))))))))

(defun settings-item-html
  (item
   body
   &key title
        help-text
        editable
        edit-text
        buttons
        hidden-input
        extra-form
        class
        (form-markup t)
        (method "post")
        (action "/settings")
   &aux (title-div
          (html (:div :class "title-container"
                 (:div :class "title"
                  (str (or title (string-capitalize item))))))))
  (html
    (:div :id (hyphenate item) :class "item settings-item"
      (:div :class "settings-item-table"
        (str title-div)
        (:div :class (s+ "details " class)
          (htm
            (if editable
              (if form-markup
                (htm (:form :method method
                            :action action
                            (:div :class "content-container"
                              (awhen hidden-input
                                (str it))
                              (if buttons
                                (htm
                                  (:div :class "form-elements" (str body))
                                  (:div :class "buttons" (str buttons)))
                                (str body))))
                     (awhen extra-form (str it)))
                (htm (:div :class "content-container"
                       (str body)
                       (when buttons
                         (htm (:div :class "buttons" (str buttons)))))))
             (htm (:div :class "content-container"
                    (:div :class "current-value" (str body))
                    (:div :class "buttons"
                      (:a :class "yes small"
                          :href (url-compose *base-url* "edit" item)
                          (or (str edit-text)
                              (htm "Edit"))))))))))
     (:p :class "help-text" (:em (str help-text))))))

(defun settings-group-category (editable groupid group)
 (settings-item-html
   "category"
   (if (or editable (not (getf group :category)))
     (group-category-selection
            :next *base-url*
            :selected (or (get-parameter "group-category")
                          (getf group :category)))
     (getf group :category))
   :class "group-category"
   :buttons (html
              (:button :type "input" :class "yes small" "Save Changes")
              (:button :type "input" :class "cancel small" :name "cancel" "Cancel"))
   :action (strcat "/groups/" groupid)
   :editable editable ))

(defun settings-avatar (&optional groupid)
  (let ((id (or groupid *userid*))
        (entity (aif groupid
                  (db it)
                  *user*)))
   (settings-item-html
     "profile picture"
     (html
       (:div :class "settings-avatar"
         (:img :class "bigavatar"
               :src (get-avatar-thumbnail id 300 300)
               :alt (getf entity :name))
         (when (getf entity :avatar)
           (htm (:form :method "post"
                       :action "/settings"
                       :class "activity-image"
                  (:button :class "simple-link green"
                           :type "submit"
                           :name "rotate-avatar"
                           "Rotate"))))))
  :buttons (new-image-form "/settings"
                           *base-url*
                           :on groupid
                           :button (if (getf entity :avatar)
                                     "Edit photo"
                                     "Add a photo"))
  :form-markup nil
  :editable t)))

(defun settings-name (editable &optional groupid group-name)
  (let ((aliases (unless groupid (getf *user* :aliases))))
    (settings-item-html "name"
      (if editable
        (html
          (when groupid
            (htm (:input :type "hidden" :name "groupid" :value groupid)))
          (:input :type "hidden" :name "next" :value *base-url*)
            (:ul :id "names"
              (:li (:span (:input :type "text"
                                  :name "name"
                                  :value (or group-name
                                             (getf *user* :name))))
                   (unless groupid
                     (htm (:span (:strong "display name")))))
              (unless groupid
                (loop for i to (max 3 (length aliases))
                      do (htm (:li
                                (:span (:input :type "text"
                                               :name "aliases"
                                               :value (awhen (nth i aliases)
                                                        it)))
                                (:span "nickname")))))))
        (html
          (:ul :id "names"
            (:li (:span (:strong (str (or group-name (getf *user* :name)))))
                 (when aliases (htm (:span (str "(displayed)")))))
            (dolist (alias aliases)
              (htm (:li (:span (str alias))
                        (:span :class "help-text" "(nickname)")))))))
    :editable editable
    :buttons (html
                (:button :class "cancel small"
                         :type "submit"
                         :name "cancel"
                         "Cancel")
                (:button :class "yes small"
                         :type "submit"
                         :name "submit"
                         "Submit"))

    :help-text (unless groupid
                 (s+ "If you are known by multiple names or nicknames, "
                     "enter up to 5 to help people find you. ")))))

(defun settings-address (editable &optional groupid group)
  (let* ((entity (or group *user*))
         (address (getf entity :address))
         (public-location (awhen (eql (getf group :location-privacy) :public) it)))
    (settings-item-html
      "address"
      (if editable
        (html
          (:input :type "hidden" :name "next" :value *base-url*)
          (when groupid
            (htm (:input :type "hidden" :name "groupid" :value groupid)))
          (:div :class "address-input"
           (:input :type "text" :name "address" :value address)
           (when groupid
             (htm
               (:br)
               (:input :type "checkbox"
                :name "public-location"
                :value (when public-location "checked"))
               "Display this address publicly to anyone looking at this group's profile page."))))
        (if (and address (getf entity :location))
          (html (:p (str address)))
          (html (:p (:strong "You have not set your address yet.")))))
    :editable editable
    :action "/settings/location"
    :class "address"
    :buttons (html (:button :class "yes small"
                            :type "submit"
                            :name "confirm-address"
                            "Submit") 
                   (:button :class "cancel small"
                            :type "submit"
                            :name "cancel"
                            "Cancel"))
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
      (header-page
        (s+ "Please verify "
            (aif (getf group :name) (s+ it "'s") "your")
            " location.")
        nil
        (html
          (:div :id "body"
            (:div :class "address setup"
              (:h2 "Verify "
                   (str (aif (getf group :name) (s+ it "'s") "your"))
                   " location")
              (:p (str
                    (if groupid
                      (s+ "The address for this group is currently set to be "
                          (if (eql (getf entity :location-privacy) :public)
                            "displayed publicly" "hidden")
                          ". You can change the visibility of this address on your group's settings page. ")
                    (s+ "We will never share your exact location with anyone else. ")))
                  "If you would like to know more about how we use the information you share with us,
                   please read our " (:a :href "/privacy" "privacy policy") ".")
              (str (static-google-map :size "280x150"
                                      :zoom 12
                                      :lat (getf (getf (token-session-data *token*)
                                                       :unverified-location)
                                                 :lat)
                                      :long (getf (getf (token-session-data *token*)
                                                        :unverified-location)
                                                  :long)))

              (:form :method "post" :action "/settings/location"
                (:h3 "Is this location correct?")
                (:input :type "hidden" :name "next" :value next)
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
                         "Yes, this is correct")))))
        :hide-menu t)
        (permission-denied))))

(defun settings-password ()
  (settings-item-html
    "password"
    (html
      (:form :method "post" :class "password" :autocomplete "off" :action "/settings"
       (:input :type "hidden" :name "next" :value *base-url*)
       (:div :class "submit-settings"
         (:button :class "yes small" :type "submit" "Change password"))
       (:div :class "content"
         (:div
           (:label :for "current-password" "Current password:")
           (:input :type "password"
                   :id "current-password"
                   :name "password"
            :placeholder "verify your current password"))
         (:div
           (:label :for "new-password" "New password:")
           (:input :type "password"
                   :id "new-password"
                   :name "new-password-1"
                   :placeholder "new password: at least 8 characters"))   
         (:div
           (:label :for "confirm-new-password"
             "Confirm your new password:")
           (:input :type "password"
                   :id "confirm-new-password"
                   :name "new-password-2"
                   :placeholder "please retype your new password")))))   

  :form-markup nil
  :editable t
  :help-text (s+ "Minimum of 8 characters. "
                 "We strongly recommend using either a mix of upper- and "
                 "lower-case letters, numbers, and symbols; or a sentence "
                 "of at least 8 words.")))

(defun settings-donate ()
  (let* ((plan (getf *user* :plan))
         (customer (stripe:retrieve-customer (getf *user* :custid)))
        ;(default-card-id (stripe:sstruct-get customer :default-card))
         (active-card (stripe:sstruct-get customer :active-card))
        ;(active-card-id (stripe:sstruct-get active-card :id))
        ;(card-is-active-p (string= default-card-id active-card-id))
        )
    (settings-item-html "donate"
      (html
        (:input :type "hidden" :name "next" :value *base-url*)
        (:div
          (:label :for "donation-plan"
            "Current monthly donation: " (:strong "$" (str plan)))
          (:select :id "donation-plan" :name "plan"
           (:option :disabled "disabled" "Select a new plan")
           (:option :value "5" "$5/month")
           (:option :value "10" "$10/month")
           (:option :value "20" "$20/month")
           (:option :value "35" "$35/month")
           (:option :value "50" "$50/month")
           (:option :value "100" "$100/month")))
        (str (settings-card-details active-card)))

    :buttons (html (:button :class "cancel small"
                            :type "submit"
                            :name "cancel-plan"
                            "Cancel plan")
                   (:button :class "yes small"
                            :type "submit"
                            "Change plan"))
    :class "password"
    :editable t
    :help-text (s+ "Your monthly donation is specified in US Dollars. Changes take place on your "
                   "next monthly bill&mdash;we do not prorate plan changes. Thank you for your "
                   "financial support!"))))

(defun settings-card-details
  (card
   &aux (edit-card-p (string= (get-parameter-string "edit") "card"))
        (name (stripe:sstruct-get card :name))
        (address (strcat* (stripe:sstruct-get card :address-line1)
                          (unless (string= (stripe:sstruct-get card
                                                               :address-line2)
                                           "NULL")
                            (s+ " " (stripe:sstruct-get card :address-line2)))))
        (city (stripe:sstruct-get card :address-city))
        (state (stripe:sstruct-get card :address-state))
        (zip (stripe:sstruct-get card :address-zip))
        (last-4 (stripe:sstruct-get card :last4))
        (card-type (stripe:sstruct-get card :type))
        (exp-month (stripe:sstruct-get card :exp-month))
        (exp-year (stripe:sstruct-get card :exp-year)))

  (html
    (:form :method "post"
           :action "/settings/ccard"
           :onsubmit (when (and edit-card-p
                                (not (post-parameter "cancel")))
                       "return tokenize(this);")
      (:blockquote :id "donate"
        (:input :id "cctoken" :name "token" :type "hidden")
        (if edit-card-p
          (htm
            (dolist (flash (flashes))
              (str flash))
            (str (stripe-tokenize :name name
                                  :address address
                                  :city city
                                  :state state
                                  :zip zip))
            (:h3 "Billing address")
            (:ul
              (:li :class "full"
                (:label :for "name" "*Name on card")
                (:input :type "text"
                        :id "name"
                        :name "name"
                        :value name))
              (:li :class "full"
                (:label :for "address" "*Address")
                (:input :type "text"
                        :id "address"
                        :name "address"
                        :value address))
              (:li :class "half"
                (:label :for "city" "*City")
                (:input :type "text"
                        :id "city"
                        :name "city"
                        :value city))
              (:li :class "quarter"
                (:label :for "state" "*State")
                (:select :name "state"
                   (str (state-options state))))
              (:li :class "quarter"
                (:label :for "zip" "*Zip")
                (:input :type "text"
                        :name "zip"
                        :value zip)))

            (:h3 "Credit card info")
            (str (credit-card-details-form :show-error t :card card))
            (:button :class "blue float-right update" :type "submit" :name "update-card" "Submit changes")
            (:a :href "/settings/personal#donate" :class "cancel button float-right update" "Cancel"))

          (htm
            (:span (:strong "Card info:"))
            (:span (:strong (str card-type)))
            (:span "****" (str last-4))
            (:span (:strong "Expires:"))
            (:span (str exp-month)
                   "/"
                   (str exp-year))
            (:button :class "blue small float-right" :type "submit" :name "edit-card" "Update card")))))
    ))

(defun settings-deactivate
  (&optional groupid
   &aux (group (db groupid)))
  (let ((action (if (eq (getf (or group *user*) :active) t)
                  "deactivate"
                  "reactivate")))
    (settings-item-html
      action
      nil
      :buttons (html
                 (:button :class "simple-link red link no-padding"
                          :name action
                          :type "submit"
                          (str (strcat* (string-capitalize action)
                                        (when groupid " group")
                                        " account"))))
      :hidden-input (when groupid
                      (html (:input :type "hidden"
                                    :name "groupid"
                                    :value groupid)))
      :editable t
      :help-text (s+ (account-deactivation-warning-text groupid)
                     "You may reactivate this account anytime by logging into Kindista and clicking \"Reactivate account\" on this page."
                     ))))

(defun account-deactivation-warning-text (groupid)
  (if groupid
    (s+ "Warning: "
        "Deactivating a group account will delete all of its current offers "
        "and requests, and prevent people from "
        "finding it using the search bar. "
        "Deactivating a group account will not remove any statements of gratitude "
        "it has given or received.")
    (s+ "Warning: "
        "Deactivating your account will delete all of your current offers "
        "and requests, and prevent people from "
        "contacting you through Kindista or finding you using the search bar. "
        "Deactivating your account will not remove any statements of gratitude "
        "you have given or received.")))

(defun confirm-deactivation ()
  (standard-page
    "Confirm Deactivation"
    (let ((groupid (get-parameter-integer "groupid")))
      (html
        (:div :class "item"
          (:h1 (str
                 (s+ "Are you sure you want to deactivate "
                     (aif groupid
                       (s+ (db it :name ) "'s group")
                       "your")
                     " account?")))
          (:p
            (str (account-deactivation-warning-text groupid)))
          (:form :method "post" :action "/settings"
            (:input :type "hidden" :name "next" :value "/settings")
            (when groupid
              (htm (:input :type "hidden" :name "groupid" :value groupid)))
            (:a :class "cancel" :href "/settings" "No, I didn't mean it!")
            (:button :class "yes"
                     :type "submit"
                     :name "confirm-deactivation"
                     (str (s+ "Yes, deactivate "
                              (if groupid "our" "my")
                              " Kindista account."))))
          )))
    :class "text"))

(defun settings-emails (editable &key activate)
  (let* ((emails (getf *user* :emails))
         (alternates (cdr emails))
         (pending (getf *user* :pending-alt-emails)))
    (settings-item-html
      "email"
      (html
        (:input :type "hidden" :name "next" :value "/settings/communication")
        (:ul :class "padding-top"
          (:li (:span :class "email-item" (:strong (str (car emails))))
               (:span :class "help-text" (:em "primary email")))
          (dolist (email alternates)
            (htm (:li (:span :class "email-item" (str email))
                      (:div :class "controls"
                         (:button :class "simple-link green"
                          :name "make-email-primary"
                          :type "submit"
                          :value email
                          "Make primary")
                         (:span "|")
                         (:button :class "simple-link red"
                          :name "remove-email"
                          :type "submit"
                          :value email
                          "Remove")))))
          (dolist (invite-id pending)
            (let ((email (getf (db invite-id) :recipient-email)))
              (when email ; there may be a bug that allows the pending invite item to be deleted without removing its id from the user's :pending-alt-emails
                (htm
                  (:li
                    (:span :class "email-item" (str email)
                           (:span :class "red" " (pending)"))
                    (cond
                      ((string= email activate)
                       (htm
                         (:div :class "controls"
                           (:input :type "hidden"
                            :name "invitation-id" :value invite-id)
                            (:div :class "input-wrapper"
                             (:input :type "text"
                              :name "token"
                              :placeholder "enter activation code"))
                            (:span "|")
                            (:button :class "simple-link green"
                             :type "submit"
                             "Activate")
                            (:span "|")
                            (:a :class "red" :href "/settings/communication" "Cancel"))))
                      (t
                       (htm
                         (:div :class "controls"
                          (when (not editable)
                            (htm
                              (:a :href (url-compose "/settings/communication"
                                                     "edit" "email"
                                                     "activate" email)
                                  "Enter code")
                              (:span "|")
                              (:button :type "submit"
                               :class "simple-link no-padding"
                               :name "resend-code"
                               :value invite-id
                               "Resend code")
                              (:span "|")
                              (:button :class "simple-link red"
                               :name "remove-pending-email"
                               :type "submit"
                               :value email
                               "Remove"))))))))))))
         (cond
          ((not editable)
           (htm (:li
                  (:a :href "/settings/communication?edit=email#email" "add another email address"))))
          ((not activate)
            (htm
              (:li
                (:div :class "input-wrapper"
                  (:input :type "text"
                          :name "new-email"
                          :placeholder "new alternate email"))
                (:div :class "controls"
                  (:button :class "yes" :type "submit" :name "submit" "Confirm new email")
                  (:span "|")
                  (:a :class "red" :href "/settings/communication" "Cancel"))))))))

      :editable T
      :class "emails"
      :help-text (html "Adding additional email addresses helps people find "
                       "you and keeps you from getting invites to Kindista at "
                       "your other addresses. "
                       "Kindista will never show your email addresses to anyone. "
                       "Only your primary email address will receive "
                       "notifications. "
                       (:strong "If you want to change your primary email "
                                "address, you must first add a secondary "
                                "email. "
                                "Then you will be able to make the new "
                                "email address primary (and remove the old "
                                "email if you so choose)."
                        )))))

(defun remove-pending-email-address
  (email
   &optional (userid *userid*)
   &aux invitation-id)
  (dolist (invite-code-cons (gethash email *invitation-index*))
    (when (eql userid (db (car invite-code-cons) :host))
      (setf invitation-id (car invite-code-cons))))
  (if invitation-id
    (delete-invitation invitation-id)
    (permission-denied)))

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
     (pending-email-actions email)
     (flash (s+ "You have successfully added " email
                " to your Kindista account."))
     (see-other "/settings/communication")))))

(defun settings-push-notifications
  (&optional group)
  (unless group
    (settings-item-html
     "push notifications"
     (html (:script :type "text/javascript" :src "/push-notification-button.js"))
     :help-text (html (:span :id "push-help-text" "Push notifications allow you to recieve messages from other users on your phone or browser. ")
                      (:strong "Currently only availabe on Chrome and Chromium version 42 and above."))
     :editable t
     :buttons (html (:button :id "push-notification-button"
                             :class "yes small"
                             :disabled t "Enable Push Messages" )))))

(defun settings-notifications (&key groupid group user (userid *userid*))
  (let* ((entity (or group  user *user*))
         (group-name (when group (getf group :name)))
         (subject (or group-name "me")))
    (labels ((checkbox-value (notify-key)
               (when (if group
                       (member userid (getf entity notify-key))
                       (getf entity notify-key))
                 "")))
      (settings-item-html
        "notifications"
        (html
          (awhen (get-parameter-string "k")
            (htm (:input :type "hidden" :name "k" :value it)))
          (awhen (get-parameter-string "email")
            (htm (:input :type "hidden" :name "email" :value it)))
          (when groupid
            (htm (:input :type "hidden" :name "groupid" :value groupid)))
          (:ul :class "padding-top"
            (:li (:input :type "checkbox"
                  :name "gratitude"
                  :checked (checkbox-value :notify-gratitude))
                 "when someone posts gratitude about "
                 (str subject))
            (:li (:input :type "checkbox"
                  :name "message"
                  :checked (checkbox-value :notify-message))
                 (str (s+ "when someone sends "
                          (if group "us" "me")
                          " a message or responds to "
                          (if group "our" "my")
                          " offers/requests")))
           (:li (:input :type "checkbox"
                  :name "inventory-expiration"
                  :checked (checkbox-value :notify-inventory-expiration))
                 (str
                   (s+ "when "
                       (if group-name "this group's" "my")
                       " offers and requests are about to expire ")))

            (when group
              (htm
                (:li (:input :type "checkbox"
                       :name "group-membership-request"
                       :checked (checkbox-value :notify-membership-request))
                      (str (s+ "when someone wants to join " group-name)))))

            (unless group
              (htm
                (:li (:input :type "checkbox"
                      :name "expired-invites"
                      :checked (checkbox-value :notify-expired-invites))
                     "when invitatations I send for my friends to join Kindista expire")
                (:li (:input :type "checkbox"
                      :name "group-membership-invites"
                      :checked (checkbox-value :notify-group-membership-invites))
                     "when someone invites me to join a group on Kindista (e.g. a business, non-profit, or other organization I belong to within my community)")
                (:li (:input :type "checkbox"
                           :name "new-contact"
                           :checked (checkbox-value :notify-new-contact))
                          "when someone adds me to their list of contacts")
                (:li :class "notifications border-top"
                     (:input :type "checkbox"
                      :name "reminders"
                      :checked (checkbox-value :notify-reminders))
                     "with occasional suggestions about how "
                     (str (if group "our group" "I"))
                     " can get the most out of Kindista")
                (:li (:input :type "checkbox"
                       :name "blog"
                       :checked (checkbox-value :notify-blog))
                      "with new articles from the Kindista blog")
                (:li (:input :type "checkbox"
                       :name "kindista"
                       :checked (checkbox-value :notify-kindista))
                      "with updates and information about Kindista")
                (:li (:input :type "checkbox"
                      :name "inventory-digest"
                      :checked (checkbox-value :notify-inventory-digest))
                     "with a weekly email featuring new offers and requests in my area")))))

     :buttons (html (:button :class (s+ "yes " (when *user* "small"))
                             :type "submit"
                             :name "save-notifications"
                       "Save preferences")
                    (unless *user*
                      (htm (:div (:a :class "blue" :href "/login"
                                   "Log into Kindista")))))
     :action "/settings/notifications"
     :class "notifications"
     :title "Notify me"
     :extra-form (unless group
                   (html
                     (:a :id "digest-distance")
                     (str (rdist-selection-html "/settings/communication"
                                                :class "digest-dist"
                                                :text "- email me offers/requests from within: "))))
     :editable t))))

(defun settings-identity-selection-html (selected groups &key userid (url "/settings"))
"Groups should be an a-list of (groupid . group-name)"
  (html
    (:form :method "post" :action url
      ;; for unsubscribing to email notifications when not logged in
      (awhen (get-parameter-string "k")
        (htm (:input :type "hidden" :name "k" :value it)))
      (awhen (get-parameter-string "email")
        (htm (:input :type "hidden" :name "email" :value it)))
      (:input :type "hidden" :name "tab" :value (script-name*))
      (:strong "Viewing settings for: ")
      (str (identity-selection-html selected
                                    groups
                                    :userid userid
                                    :onchange "this.form.submit()"))
      " "
      (:input :type "submit" :class "no-js" :value "apply"))))

(defun go-settings ()
  (aif (get-parameter "groupid")
    (see-other (url-compose "/settings/public"
                            "groupid" it))
    (see-other "/settings/personal")))

(defmacro settings-template-html (base-url &body body)
  `(require-user (:allow-test-user t)
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
          (str (settings-name (string= edit "name")
                              groupid
                              (getf group :name)))
          (when groupid
            (str (settings-item-html
                   "profile"
                   (html
                     (:a :href (strcat "/groups/" (username-or-id groupid))
                       (str (strcat +base-url+
                                    "groups/"
                                    (username-or-id groupid)))))
                   :editable t))
            (str (settings-group-category
                   (or (string= (get-parameter "group-category") "other")
                       (string= edit "category"))
                   groupid
                   group)))

          (str (settings-avatar groupid))
          (str (settings-address (string= edit "address") groupid group))
          (when groupid
            (str (settings-item-html
                   "membership"
                   (html
                     (str
                       (group-membership-method-selection
                         (if (eql (getf group :membership-method)
                                  :invite-only)
                           "invite-only"
                           "admin-approval")
                         :auto-submit t))
                     (:input :type "submit" :class "no-js" :value "apply"))
                   :class "membership-settings"
                   :action (strcat "/groups/" groupid)
                   :editable t)))
          (when (not groupid)
            (htm (str (settings-password))
                 (when (getf *user* :plan)
                   (str (settings-donate)))))
          (str (settings-deactivate groupid))))))

(defun post-settings-social ()
  (require-user (:allow-test-user t)
    (cond
      ((post-parameter "fb-logout")
       (modify-db *userid* :fb-token nil
                           :fb-expires nil
                           :fb-link-active nil)
       (flash "Kindista no longer has access to your Facebook account")))
    (see-other (or (post-parameter-string "next") "/settings/social"))))

(defun get-settings-social ()
  (require-user (:allow-test-user t)
    (let* ((now (get-universal-time))
           (fb-token-data (when (get-parameter "code")
                                  (register-facebook-user "settings/social")))
           (fb-token (getf fb-token-data :access--token))
           (fb-expires (when fb-token-data
                      (+ (get-universal-time)
                         (safe-parse-integer (getf fb-token-data :expires--in)))))
           (fb-scope (when fb-token-data
                       (awhen (get-parameter-string "granted_scopes")
                         (mapcar #'string-to-keyword
                                 (words-from-string it))))))
      (when fb-token
        (apply #'modify-db
               *userid*
               (remove-nil-plist-pairs
                 (list :fb-token fb-token
                       :fb-expires (+ (get-universal-time)
                                      (safe-parse-integer fb-expires))
                       :fb-link-active t
                       :fb-permissions fb-scope)))
        (unless (getf *user* :fb-id)
          (let ((fb-id (get-facebook-user-id fb-token)))
            (modify-db *userid* :fb-id fb-id)
            (with-locked-hash-table (*facebook-id-index*)
              (setf (gethash fb-id *facebook-id-index*) *userid*))))
        (unless (getf *user* :avatar)
          (modify-db *userid* :avatar (save-facebook-profile-picture-to-avatar *userid*)))
        (flash "You have successfully linked Kindista to your Facebook account.")))
    (settings-social-html)))

(defun settings-social-html
  (&aux (fb-token-p (getf *user* :fb-token))
        (sign-in-button (facebook-sign-in-button
                          :redirect-uri "settings/social"
                          :button-text "Sign in to Facebook"))
        (sign-out-button
          (html (:button :class "cancel"
                         :type "submit"
                         :name "fb-logout"
                   "Log out of Facebook"))))
  (settings-template-html
    (aif (get-parameter "groupid")
      (url-compose "/settings/social"
                   "groupid" it)
      "/settings/social")
    (html
      (str (settings-tabs-html "social" (awhen groupid it)))
      (str
        (settings-item-html
          "facebook"
          (html
            (when (get-parameter "error")
              (htm (:p :class "error"
                     "Connecting with Facebook had an error: "
                     (str (get-parameter "error_description")))))
            (:div
              (:span
                (str (if fb-token-p
                       "Your Kindista account is currently linked to your Facebook account."
                       "Your Kindista account is not currently linked to Facebook."))))
            )

       :buttons (if fb-token-p sign-out-button sign-in-button)
       :help-text (if fb-token-p
                    (html
                       "You can manage the types of Facebook data that Kindista has access to "
                       (:strong
                         (:a :href "https://www.facebook.com/settings?tab=applications"
                             "here"))
              ".")
                    "You can connect your Kindista account with Facebook to post your offers, requests, and gratitude to your Facebook timeline.")
       :action "/settings/social"
       :class "facebook"
       :title "Facebook"
       :editable t)))))

(defun unsub-notify-from-email
  (email
   unsub-type
   &aux (userid (gethash email *email-index*))
        (notify-type (string-upcase (s+ "notify-" unsub-type)))
        (unverified-groupid (get-parameter-integer "groupid"))
        (users-groups (groups-with-user-as-admin userid))
        (groupid (awhen (find unverified-groupid users-groups :key #'car)
                   (car it))))

  (when (find notify-type
              (mapcar #'symbol-name *notification-types*)
              :test #'string=)
    (setf unsub-type (intern notify-type :keyword)))

  (flet ((unsub (message)
           (if groupid
             (amodify-db groupid unsub-type (remove userid it))
             (modify-db userid unsub-type nil))
           (flash message)))
    (case unsub-type
      (:notify-expired-invites
        (unsub "You are now unsubscribed from receiving notificaitons when invitations you sent to friends to join Kindista expire"))
      (:notify-gratitude
        (unsub
          (if groupid
            "You are now unsubscribed from receiving notifications when this group recieves a statment of gratitude"
            "You are now unsubscribed from receiving notificaitons when someone posts gratitude about you")))
      (:notify-group-membership-invites
        (unsub "You are now unsubscribed from receiving notificaitons when someone invites you to join a group on Kindista "))
      (:notify-membership-request
        (unsub "You are now unsubscribed from reciving notifications when someone requests to join this group"))
      (:notify-inventory-expiration
        (unsub
          (if groupid
            "You are now unsubscribed from receiving notifications when this group's offers and requests are about to expire"
            "You are now unsubscribed from receiving notificaitons when your offers and requests are about to expire")))
      (:notify-inventory-digest
        (unsub "You are now unsubscribed from receiving notificaitons of new offers and requests in your area"))
      (:notify-kindista
        (unsub "You are now unsubscribed from receiving updates and information about Kindista"))
      (:notify-blog
        (unsub "You are now unsubscribed from receiving new articles from the Kindista blog"))
      (:notify-message
        (unsub
          (if groupid
            "You are now unsubscribed from receiving notifications when this group recives a message or a response for an offer/request"
            "You are now unsubscribed from receiving notificaitons when someone sends you a message or responds to your offers/requests")))
      (:notify-new-contact
        (unsub "You are now unsubscribed from receiving notificaitons when someone adds you to their list of contacts"))
      (:notify-reminders
        (unsub "You are now unsubscribed from receiving reminders for how to get the most out of Kindista")))))

(defun settings-unsubscribe-all ()
     (settings-item-html
     "Unsubscribe"
     (html
       (awhen (get-parameter-string "k")
         (htm (:input :type "hidden" :name "k" :value it)))
       (awhen (get-parameter-string "email")
         (htm (:input :type "hidden" :name "email" :value it)))
       (:p (:strong "Warning: If you unsubscribe from all notifications, you will not recieve email notifications when people post gratitude about you or want to share with you."))
       (:p "If you only want to unsubscribe from notifications sent by our system (and still want messages from other Kindista members), please adjust your individual notification preferences above."))
     :editable t
     :buttons (html (:button :class (s+ "red" (when *user* " small"))
                             :type "submit"
                             :name "unsubscribe-all"
                             "Unsubscribe From All Notifications" ))
     :action "/settings/notifications"))

(defun get-settings-communication
  (&aux (unsub-email (get-parameter-string "email"))
        (unsub-type (get-parameter-string "type"))
        (unsub-key (get-parameter-string "k"))
        (unsub-id (awhen unsub-email
                    (gethash it *email-index*)))
        (unsub-user (awhen unsub-id (db it)))
        (valid-unsub (string= (getf unsub-user :unsubscribe-key)
                              unsub-key)))
  (cond
    ((and (not *user*) (not valid-unsub))
     (login-required))

    ((not *user*)
     (let* ((unverified-groupid (get-parameter-integer "groupid"))
            (groups (groups-with-user-as-admin unsub-id))
            (groupid (awhen (find unverified-groupid groups :key #'car)
                       (car it)))
            (group (db groupid)))
       (unsub-notify-from-email unsub-email unsub-type )
       (header-page
       "Communication Settings"
       nil
       (html
         (dolist (flash (flashes)) (str flash))
         (:div :class "logged-out unsubscribe"
          (:h2 "Please let us know what types of information you would like to be notified about")
          (:p "Notifications will be sent to your primary email address: "
           (:strong (str (car (getf unsub-user :emails)))))
          (when groups
            (str (settings-identity-selection-html
                   (or groupid unsub-id)
                   groups
                   :userid unsub-id
                   :url "/settings/notifications")))
          (str (settings-notifications :user unsub-user
                                       :userid unsub-id
                                       :group group
                                       :groupid groupid))
          (unless group (str (settings-unsubscribe-all)))))
       :hide-menu t)))

    ((and (scan +number-scanner+ (get-parameter "invitation-id"))
              (get-parameter "token"))
     (activate-email-address (parse-integer (get-parameter "invitation-id"))
                                (get-parameter "token")))

    (t ;when *user*
     (when unsub-id
       (if (not (eql unsub-id *userid*))
         (flash "You must first sign out of this account before you can change settings on a different account." :error t)
         (when valid-unsub
           (unsub-notify-from-email unsub-email unsub-type))))
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
            (:strong (str (car (getf *user* :emails))))
            (:a :id "email-link" :href "/settings/communication#email" "change"))
        (str (settings-notifications :groupid groupid :group group))
        (when (and (not groupid) (eql unsub-id *userid*))
          (str (settings-unsubscribe-all)))
      ;; Push notifications are currently broken. Android changed the API.
      ;;(str (settings-push-notifications group))
        (unless groupid
          (str (settings-emails (string= (get-parameter "edit") "email")
                                :activate (get-parameter "activate")))))))))

(defun get-settings-admin-roles ()
  (require-user ()
    (aif (get-parameter "groupid")
      (let* ((groupid (parse-integer it))
             (adminp (group-admin-p groupid)) )
        (cond
          ((not groupid)
           (flash "Your request could not be processed" :error t)
           (see-other "/settings"))

          ((not adminp)
           (permission-denied))

          ((get-parameter "add-admin")
           (confirm-add-admin groupid (parse-integer (get-parameter "add-admin"))))

          ((get-parameter "revoke-admin-role")
           (confirm-revoke-admin-role groupid))

          (t
           (settings-admin-roles-html))))
      (see-other "settings"))))

(defun confirm-add-admin (groupid new-admin)
  (let ((admin-name (db new-admin :name)))
    (confirm-action
      (s+ "Make " admin-name " an admin?")
      (strcat "Are you sure you want to make "
              admin-name
              " an administrator for "
              (db groupid :name)
              "'s group account?" )
      :url (strcat "/groups/" groupid)
      :item-id new-admin
      :post-parameter "confirm-new-admin"
      :details (s+ "At this time, each group account can have a maximum of four admins. "
                   "We recommend that you only give admin privileges to people who are "
                   "actually in leadership or administrative roles within this group. "
                   "Once you make "
                   admin-name
                   " an admin, you will not have the ability to "
                   "remove their admin privileges (however they may revoke their admin "
                   "status themselves). " 
                   "<strong>Therefore, we strongly recommend that you confirm that "
                   admin-name
                   " is willing to act as an admin for this account before you give them "
                   "admin privileges.</strong>" ))))

(defun confirm-revoke-admin-role (groupid)
  (let ((group-name (db groupid :name)))
    (confirm-action
      (s+ "Revoke your admin role for " group-name "'s account?")
      (strcat "Are you sure you want to revoke your admin role for "
              group-name
              "'s account?" )
      :url (strcat "/groups/" groupid)
      :post-parameter "confirm-revoke-admin-role"
      :class "revoke-admin-role"
      :details (html
                 (:p "If you revoke your admin role for this group, you will no longer be "
                     "able to take any of the following actions:")
                 (:ul
                   (:li "post offers, requests, events, or statements of gratitude on behalf of the group")
                   (:li "respond to messages sent to the group")
                   (:li "approve or invite new members to join the group"))))))

(defun settings-admin-roles-html ()
  (settings-template-html
        "/settings/admin-roles"
        (html
          (:div :class "admin-roles"
            (str (settings-tabs-html "admin" groupid))
            (:p :class "help-text"
              "Admins have the ability to post offers, requests, events, and statements of "
              "gratitude on behalf of your group. They receive and respond to messages on behalf "
              "of the group.  And they can invite people to become members of the group.")
            (:h3 "Current admins for " (str group-name))
            (dolist (admin (getf group :admins))
              ;; when there is another admin to fulfil admin duties,
              ;; user can step down from admin role
              (htm
                (:div :class "current-admin"
                  (when (and (eq admin *userid*)
                            (> (length (getf group :admins)) 1))
                   (htm
                     (:form :method "get" :action "/settings/admin-roles" :class "float-right"
                      (:input :type "hidden" :name "groupid" :value groupid)
                      (:button :type "submit"
                               :class "yes"
                               :name "revoke-admin-role"
                               "revoke admin role"))))
                 (str (person-card admin )))))
            (:div :class "invite-members"
              (:h3 "Add another admin for "
               (str
                 (if (eq groupid +kindista-id+)
                   "Kindista's group account"
                   (db groupid :name))))
              (cond
                ((eql 0 (length (getf group :members)))
                 (htm
                   (:p (str (getf group :name))
                       " does not have any members who are not already admins for its group "
                       "account.")
                   (:p (:a :href (strcat "/groups/" groupid "/invite-members")
                         "+add group members"))))

                ((< (length (getf group :admins)) 4)
                 (htm
                   (:form :method "get"
                          :action "/settings/admin-roles"
                     (:input :type "hidden" :name "groupid" :value (str groupid))
                     (:input :type "text"
                             :name "search-name"
                             :placeholder "search by name")
                     (:button :class "yes" :type "submit" "search"))
                   (:p "Or, select from the list of current members below:")
                   (flet ((member-card (person-cons)
                            (html
                              (:form :method "get"
                                     :class "invite-member-result"
                                     :action (strcat "/settings/admin-roles" )
                                 (:input :type "hidden" :name "groupid" :value groupid)
                                 (str
                                   (person-card
                                     (car person-cons)
                                     :alias (cdr person-cons)
                                     :button (html
                                               (:button :class "yes"
                                                        :name "add-admin"
                                                        :value (car person-cons)
                                                        :type "submit"
                                                        "Make admin"))))))))
                     (if (and (get-parameter "search-name")
                              (scan +text-scanner+
                                    (get-parameter "search-name")))
                       (let* ((q (search-people (get-parameter "search-name")))
                              (results (remove-if-not #'(lambda (x)(member x (getf group :members)))
                                                      q
                                                      :key #'car)))
                         (if results
                           (dolist (result results)
                             (str (member-card result)))
                           (htm
                             (:p
                               "there are no members in this group  with the name "
                               (str (get-parameter "search-name"))
                               " .  please try again."))))
                       (let* ((members (mapcar #'(lambda (id)
                                                   (cons id (db id :name)))
                                               (getf group :members)))
                              (alpha-members (sort members
                                                   #'string-lessp
                                                   :key #'cdr)))
                         (dolist (person alpha-members)
                           (str (member-card person))))))))

                (t
                 (htm
                   (:p :class "help-text"
                    "this account already has the maximum of three administrators. "
                    "if you would like to add an additional admin for this account, "
                    "one of the current admins must first revoke their admin privileges." )))))))))

(defun get-settings-error ()
  (flash "the avatar you uploaded is too large. please upload an image smaller than 10mb." :error t)
  (go-settings))

(defun post-settings-notification
  (&aux (k (post-parameter-string "k"))
        (unverified-email (post-parameter-string "email"))
        (unsub-type (post-parameter-string "type")))
  "Requires a logged in user or credentials from an unsubscribe link.  When both are present, unsubscribe link credentials are used."
  (if (and (not *user*)
           (or (not k) (not unverified-email)))
    (login-required)

    (let* ((unverified-userid (gethash unverified-email *email-index*))
           (unverified-user (db unverified-userid))
           (verified-user (when (string= (getf unverified-user
                                               :unsubscribe-key)
                                         k)
                              unverified-user))
           (userid (or (when verified-user unverified-userid)
                       *userid*))
           (user (or verified-user *user*))
           (unsub-all-p (post-parameter "unsubscribe-all"))
           (save (post-parameter "save-notifications"))
           ;; for identity-selection-html
           (identity (post-parameter-integer "identity-selection"))
           (unverified-groupid (or (post-parameter-integer "groupid") identity))
           (groupid (when (group-admin-p unverified-groupid userid) unverified-groupid))
           (group (db groupid))
           (next (or (post-parameter "next")
                     (url-compose "/settings/communication"
                          "groupid" groupid
                          "type" unsub-type
                          "email" unverified-email
                          "k" k))))

      (unless (if save
                (post-parameter "message")
                (if groupid
                  (find userid (getf group :notify-message))
                  (getf user :notify-message)))
        (flash "Warning: You will not receive any email notifications when people reply to your Offers and Requests unless you choose to be notified \"when someone sends me a message\"!" :error t))

      (cond
        ((and unverified-email (not userid))
         (flash "The unsubscribe link you are using is not valid." :error t)
         (permission-denied "/home"))

        ((and identity (not save))
         (see-other (url-compose "/settings/communication"
                                 "k" k
                                 "email" unverified-email
                                 "groupid" groupid)))

        ((and save groupid)
         (amodify-db groupid
                    :notify-gratitude (if (post-parameter "gratitude")
                                        (pushnew userid it)
                                        (remove userid it))
                    :notify-message (if (post-parameter "message")
                                      (pushnew userid it)
                                      (remove userid it))
                    :notify-inventory-expiration (if (post-parameter "inventory-expiration")
                                                   (pushnew userid it)
                                                   (remove userid it))
                    :notify-membership-request (if (post-parameter "group-membership-request")
                                                 (pushnew userid it)
                                                 (remove userid it)))
         (notice :updated-notifications :userid userid :groupid groupid)
         (flash "Your notification preferences have been saved.")
         (see-other next))
        (unsub-all-p
         (modify-db userid
                    :notify-gratitude nil
                    :notify-message nil
                    :notify-inventory-expiration nil
                    :notify-new-contact nil
                    :notify-reminders nil
                    :notify-inventory-digest nil
                    :notify-blog nil
                    :notify-expired-invites nil
                    :notify-group-membership-invites nil
                    :notify-kindista nil)
         (notice :updated-notifications :userid userid)
         (flash "You are now unsubscribed from reciving any notifications from Kindista")
         (see-other next))
        (save
         (modify-db userid
                    :notify-gratitude (when (post-parameter "gratitude") t)
                    :notify-message (when (post-parameter "message") t)
                    :notify-inventory-expiration (when (post-parameter "inventory-expiration") t)
                    :notify-new-contact (when (post-parameter "new-contact") t)
                    :notify-reminders (when (post-parameter "reminders") t)
                    :notify-inventory-digest (when (post-parameter "inventory-digest") t)
                    :notify-blog (when (post-parameter "blog") t)
                    :notify-expired-invites (when (post-parameter "expired-invites") t)
                    :notify-group-membership-invites (when (post-parameter "group-membership-invites") t)
                   :notify-kindista (when (post-parameter "kindista") t))
         (notice :updated-notifications :userid userid)
         (flash "Your personal notification preferences have been saved.")
         (see-other next))))))

(defun post-settings-ccard ()
  (require-user ()
    (cond
      ((post-parameter "cancel")
       (see-other "/settings/personal#donate"))
      ((post-parameter "edit-card")
       (see-other "/settings/personal?edit=card#donate"))
      ((post-parameter "token")
       (let ((token (post-parameter "token")))
         (handler-case
           (progn
             (stripe:update-customer (getf *user* :custid) :card token)
             (flash "Your card has been updated")
             (see-other "/settings/personal#donate"))
           (stripe::stripe-error (err)
             (let ((code (stripe:sstruct-get (stripe::stripe-error-reply err) :error :code)))
               (flash
                 (cond
                   ((string= code "card_declined")
                    "Your card was declined")

                   ((string= code "processing_error")
                    "Our payment processor encountered an error while processing your card.")

                   ((string= code "expired_card")
                    "This card has expired.")

                   (t "An error occurred while processing your card."))
                 :error t)
               (see-other "/settings/personal?edit=card#donate")    
               ))))
       ))))

(defun post-settings-location ()
  (require-user (:require-active-user t :allow-test-user t)
    (let* ((groupid (or (post-parameter "on")
                        (post-parameter "groupid")))
           (id (or (when groupid
                     (unless (string= groupid "")
                       (parse-integer groupid)))
                   *userid*))
           (next (awhen (post-parameter "next")
                   (url-encode it)))
           (entity (if (eql id *userid*) *user* (db id))))
      (if (or (eql id *userid*)
              (member *userid* (getf entity :admins)))
        (acond
         ((post-parameter "address")
          (cond
            ((string= it "")
             ; try-again, we need an actual address
             (see-other (referer)))
            ((and groupid
                   (string= it (getf entity :address))
                   (getf entity :location)
                   (getf entity :lat)
                   (getf entity :long))
             (modify-db id :location-privacy (if (post-parameter "public-location") :public :private))
             (see-other (or next "/home")))
            (t
             (log-unverified-token-location it)
             (see-other (if groupid
                          (url-compose "/settings/verify-address"
                                       "groupid" id
                                       "next" (or next "/home"))
                          (url-compose "/settings/verify-address"
                                       "next" (or next "/home")))))))

         ((and (post-parameter "confirm-location")
               (getf (getf (token-session-data *token*) :unverified-location) :lat)
               (getf (getf (token-session-data *token*) :unverified-location) :long))
          (apply #'modify-db id :location t (getf (token-session-data *token*) :unverified-location))
          (case (getf entity :type)
            (:person (reindex-person-location *userid*))
            (:group (reindex-group-location id)))
          ;; don't use encoded url here:
          (see-other (or (post-parameter "next") "/home")))

         ((post-parameter "reset-location")
          (see-other (or it "/home"))))

        (permission-denied)))))

(defun post-settings ()
  (require-user ()
    (let* ((groupid (or (post-parameter-integer "on")
                        (post-parameter-integer "groupid")))
           (id (or groupid *userid*))
           (entity (if (eql id *userid*) *user* (db id))))

      (if (or (eql id *userid*)
              (member *userid* (getf entity :admins)))
        (acond
          ((post-parameter "cancel")
           (see-other (or (post-parameter "next") "/home")))

          ((post-parameter "identity-selection")
           (see-other
             (url-compose (post-parameter-string "tab")
                          "groupid" (post-parameter-integer
                                      "identity-selection")
                          "email" (post-parameter-string "email")
                          "k" (post-parameter-string "k"))))

          ((post-parameter "image")
           (let ((groupid (when (post-parameter "on") groupid)))
             (handler-case
               ;hunchentoot returns a list containing
               ;(path file-name content-type)
               ;when the post-parameter is a file, i.e. (first it) = path
               (let* ((id (create-image (first it) (third it)))
                      (old-avatar (if groupid
                                    (db groupid :avatar)
                                    (getf *user* :avatar))))
                 (when (integerp old-avatar)
                   (delete-image old-avatar))
                 (modify-db (or groupid *userid*) :avatar id))
               (t () (flash "Please use a .jpg, .png, or .gif" :error t)))
             (see-other (if groupid
                          (url-compose "/settings/public"
                                       "groupid" groupid)
                          "/settings/personal"))))

          ((post-parameter "rotate-avatar")
           (rotate-image (getf entity :avatar))
           (see-other (referer)))

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
              (delete-all-but-current-token-cookie)
              (flash "You have successfully changed your password.")
              (see-other (or (post-parameter "next") "/home")))))

          ((post-parameter "deactivate")
           (flash "Please confirm your choice to deactivate this account." :error t)
           (see-other (url-compose "/deactivate-account"
                                   "groupid" groupid)))

          ((post-parameter "confirm-deactivation")
           (if groupid
             (progn
               (deactivate-group groupid)
               (flash (s+ "You have deactivated "
                          (db groupid :name)
                          "'s group account. If you change your mind you can reactivate this account on the group's settings page."))
               (see-other (url-compose "/settings/public"
                                       "groupid" groupid)))
             (progn
               (deactivate-person *userid*)
               (flash "You have deactivated you account. If you change your mind you can reactivate your account on the settings page.")
               (see-other "/"))))

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
                (stripe::stripe-not-found (err)
                   (declare (ignore err))
                       (modify-db *userid* :plan nil)
                       (flash "Your plan has been cancelled, effective immediately. Thank you for your financial support!")
                       (see-other "/settings/personal"))
                (t (err)
                   (declare (ignore err))
                   (flash "There was an error deleting your subscription. Humans have been notified!" :error t)
                   (notice :error
                           :on :cancel-plan
                           :url "/settings/personal"
                           :userid *userid*
                           :custid (getf *user* :custid)))))

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
           (if groupid

             (progn (reactivate-group groupid)
                    (flash (s+ "You have reactivated "
                               (db groupid :name)
                               "'s Kindista account."))
                    (see-other (url-compose "/settings/public"
                                            "groupid" groupid)))
             (progn (reactivate-person *userid*)
                    (flash "You have reactivated your Kindista account.")
                    (see-other "/settings/personal"))))

          ((post-parameter "remove-email")
           (let ((email (post-parameter "remove-email")))
             (amodify-db *userid* :emails (remove email it :test #'string=))
             (with-locked-hash-table (*email-index*)
               (remhash email *email-index*))
             (flash (s+ email " has been removed from your kindista account."))
             (see-other "/settings/communication")))

          ((post-parameter "remove-pending-email")
           (let ((email (post-parameter "remove-pending-email")))
             (remove-pending-email-address email)
             (flash (s+ email " has been removed from your kindista account."))
             (see-other "/settings/communication")))

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
               ((find new-email emails)
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
                (let (confirmation)
                  (setf confirmation (create-invitation new-email :self t))
                  (modify-db *userid* :pending-alt-emails (cons confirmation pending)))
                (flash (s+ "A verification email has been sent to " new-email
                           ". Please click on the link provided in that email "
                           "to complete the email verification process."))
                (see-other "/settings/communication")))))

          ((and (post-parameter "invitation-id")
                (post-parameter "token"))
           (activate-email-address (post-parameter-integer "invitation-id")
                                   (post-parameter "token")))

          ((post-parameter "resend-code")
           (let* ((id (parse-integer (post-parameter "resend-code")))
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
             (flash (s+ new-primary " is now your primary email address."))
             (see-other "/settings/communication")))

          ((post-parameter "bio-doing")
           (unless (getf entity :bio)
             (modify-db id :bio t))
           (modify-db id :bio-doing (post-parameter "bio-doing"))
           (see-other (or (post-parameter "next") "/home")))

          ((post-parameter "bio-summary")
           (unless (getf entity :bio)
             (modify-db id :bio t))
           (modify-db id :bio-summary (post-parameter "bio-summary"))
           (see-other (or (post-parameter "next") "/home")))

          ((post-parameter "bio-into")
           (unless (getf entity :bio)
             (modify-db id :bio t))
           (modify-db id :bio-into (post-parameter "bio-into"))
           (see-other (or (post-parameter "next") "/home")))

          ((post-parameter "bio-contact")
           (unless (getf entity :bio)
             (modify-db id :bio t))
           (modify-db id :bio-contact (post-parameter "bio-contact"))
           (see-other (or (post-parameter "next") "/home")))

          ((post-parameter "bio-skills")
           (unless (getf entity :bio)
             (modify-db id :bio t))
           (modify-db id :bio-skills (post-parameter "bio-skills"))
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
