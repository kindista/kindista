;; Copyright 2012-2013 CommonGoods Network, Inc.
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

(defun get-name-collection ())

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
          (htm (:a :href (s+ base "?edit=" item) 
                (or (str edit-text) (htm "Edit")))))
        (str body)
        (:p :class "help-text" (:em (str help-text)))))))

(defun settings-name (base editable)
  (let ((aliases (getf *user* :aliases)))
    (settings-item-html base "name" "Name"
    (cond 
      (editable
        (html
          (:form :method "post" :action "/settings"
           (:input :type "hidden" :name "next" :value "/settings/personal")
           (:div :class "submit-settings"
             (:button :class "no" :type "submit" :class "submit" :name "cancel" "Cancel")
             (:button :class "yes" :type "submit" :class "submit" :name "submit" "Submit"))   
           (:ul 
             (:li (:input :type "text" 
                          :name "name" 
                          :value (str (getf *user* :name)))
                  (:span (:strong "display name"))) 
             (iter (for i to 3) 
                   (htm (:li 
                          (:input :type "text" 
                                  :name "aliases"
                                  :value (awhen (nth i aliases) 
                                                (str it)))
                          (:span "nickname")))))
           )))
      (t
        (html
          (:ul
            (:li (:strong (str (getf *user* :name))) 
                 (when aliases (htm (:span (str "(displayed)")))))
            (dolist (alias aliases)
              (htm (:li (str alias) 
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
             (:button :class "no" :type "submit" :class "submit" :name "cancel" "Cancel")
             (:button :class "yes" :type "submit" :class "submit" :name "confirm-address" "Submit"))
           (:input :type "text" :name "address" :value (str address))))) 
      (t
        (aif address 
          (html (:p (str it)))
          (html (:p (:strong "You have not set your address yet."))))
            ))
    :editable editable 
    :edit-text (unless address "Add address")
    :help-text (s+ "Addresses help people find nearby resources and requests. "
                   "Your address will never be displayed or shared; "
                   "it is used only to calculate distance. "))))

(defun verify-address (&key next-url)
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
              (:button :class "yes" 
                       :type "submit" 
                       :name "confirm-location" 
                       :value "1"
                       "Yes, this is correct")
              (:button :class "no" 
                       :type "submit" 
                       :name "reset-location"
                       :value "1"
                       "No, go back"))))))))

(defun settings-password (base)
  (let ((password (getf *user* :pass)))
    (settings-item-html base "password" "Password"
      (html
        (:form :method "post" :class "password" :action "/settings"
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
                   "lower-case letters, numbers, and symbols; or a sentance "
                   "of at least 8 words."))))

(defun settings-emails (base editable)
  (let* ((emails (getf *user* :email))
         (alternates (cdr emails)))

    (settings-item-html base "email" "Email"
      (cond 
        (editable
          (html
            (:form :method "post" :action "/settings"
              (:input :type "hidden" :name "next" :value "/settings/communication")
              (:ul 
                (:li (str (car emails))
                     (:span (:strong "primary email"))) 
                (dolist (email alternates)
                  (htm (:li (str email))))
                (:li 
                  (:input :type "text" 
                          :name "aliases"
                          :placeholder "new alternate email"
                          )
                  (:button :class "yes" :type "submit" :class "submit" :name "submit" "Confirm new email")    
                  (:a :class "red" :href "/settings/communication" "Cancel"))))))
        (t
          (html
            (:ul
              (:li (:strong (str (car emails))) 
                   (:span (str "(primary email)")))
              (dolist (email alternates)
                (htm (:li (str email))))) 
            (:p (:a :href "/settings/communication?edit=email" "add another email address")))))

      :edit-text ""
      :editable editable 
      :help-text (s+ "Adding additional email address helps people find you "
                     "and keeps you from getting invites to Kindista at "
                     "your other addresses. "
                     "Kindista will never show your email addresses to anyone. "
                     "Only your primary email address will receive "
                     "notifications." ))))

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
                :name "kindista"
                :checked (when (getf *user* :notify-kindista) "checked"))
               "with updates and information about Kindista"))))

    :editable t))

(defun get-personal-settings ()
  (let ((base "/settings/personal")) 
    (standard-page
      "Settings"
      (html
        (:h2 "Settings")
        (str (settings-tabs-html "personal"))
        (str (settings-name base 
                            (awhen 
                              (string= (get-parameter "edit") "name") it))) 
        (str (settings-address base 
                               (awhen 
                                 (string= (get-parameter "edit") "address") it))) 
        (str (settings-password base))
        ))))

(defun get-communication-settings ()
  (let ((base "/settings/communication")) 
    (standard-page
      "Settings"

      (html
        (:div :class "settings"
          (:h2 "Settings")
          (str (settings-tabs-html "communication")) 
          (:p "We'll email you whenever something happens on Kindista that "
              "involves you. You can specify which actions you would like "
              "to be notified about.") 
          (:p "Notifications will be sent to your primary email address: "
            (:strong (str (car (getf *user* :email))))) 
          

          (str (settings-notifications base))

          (str (settings-emails base 
                                (awhen 
                                  (string= (get-parameter "edit") "email") it)))
          
          )))))

(defun post-settings ()
  
  (awhen (post-parameter "cancel")
    (see-other (or it "/home")))
  
  (awhen (post-parameter "name")
    (cond 
      ((validate-name it)
       (unless (equal (getf *user* :name) it)
         (modify-db *userid* :name it))) 
      (t
        (flash "You must use your true full name (first and last) for your primary name on Kindista.  Single word names are permitted for your nicknames." :error t)) 
    (see-other (or (post-parameter "next") "/home"))))

  (when (post-parameter "aliases")
    (let ((aliases (iter (for pair in (post-parameters*))
                         (unless (string= (cdr pair) "")
                           (when (string= (car pair) "aliases")
                             (collect (cdr pair)))))))
      (unless (equal (getf *user* :aliases) aliases)
        (modify-db *userid* :aliases aliases))) 
    (see-other (or (post-parameter "next") "/home")))
  
  (when (post-parameter "address")
    (multiple-value-bind (lat long address city state country street zip)
      (geocode-address (post-parameter "address"))
      (modify-db *userid* :lat lat :long long :address address :city city :state state :street street :zip zip :country country))
    (see-other (aif (post-parameter "next") 
                 (url-compose "/settings/verify-address" "next" it) 
                 (url-compose "/settings/verify-address" "next" "/home"))))

  (when (post-parameter "reset-location")
    (modify-db *userid* :lat nil :long nil :address nil :location nil)
    (see-other (or (post-parameter "next") "/home")))

  (when (post-parameter "password")
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
       (modify-db *userid* :pass (new-password 
                                       (post-parameter "new-password-1")))
       (flash "You have successfully changed your password.")
       (see-other (or (post-parameter "next") "/home")))))

  (when (post-parameter "save-notifications")
    (pprint (post-parameter "gratitude")) (terpri)
    (modify-db *userid* :notify-gratitude (when (post-parameter "gratitude") t))
    (modify-db *userid* :notify-message (when (post-parameter "message") t))
    (modify-db *userid* :notify-kindista (when (post-parameter "kindista") t))
    (flash "Your notification preferences have been saved.")
    (see-other (or (post-parameter "next") "/home")))

  (when (post-parameter "bio-doing")
    (unless (getf *user* :bio)
      (modify-db *userid* :bio t))
    (modify-db *userid* :bio-doing (post-parameter "bio-doing"))
    (see-other (or (post-parameter "next") "/home")))

  (when (post-parameter "bio-summary")
    (unless (getf *user* :bio)
      (modify-db *userid* :bio t))
    (modify-db *userid* :bio-summary (post-parameter "bio-summary"))
    (see-other (or (post-parameter "next") "/home")))

  (when (post-parameter "bio-into")
    (unless (getf *user* :bio)
      (modify-db *userid* :bio t))
    (modify-db *userid* :bio-into (post-parameter "bio-into"))
    (see-other (or (post-parameter "next") "/home")))

  (when (post-parameter "bio-contact")
    (unless (getf *user* :bio)
      (modify-db *userid* :bio t)) (modify-db *userid* :bio-contact (post-parameter "bio-contact"))
    (see-other (or (post-parameter "next") "/home")))

  (when (post-parameter "bio-skills")
    (unless (getf *user* :bio)
      (modify-db *userid* :bio t))
    (modify-db *userid* :bio-skills (post-parameter "bio-skills"))
    (see-other (or (post-parameter "next") "/home")))

  (when (scan +number-scanner+ (post-parameter "rdist"))
    (modify-db *userid* :rdist (parse-integer (post-parameter "rdist")))
    (flash "Your search distance for resources and requests has been changed!")
    (see-other (or (post-parameter "next") "/requests")))

  (when (scan +number-scanner+ (post-parameter "sdist"))
    (modify-db *userid* :rdist (parse-integer (post-parameter "sdist")))
    (flash "Your default search distance has been changed!")
    (see-other (or (post-parameter "next") "/requests")))

  (when (scan +number-scanner+ (post-parameter "distance"))
    (modify-db *userid* :distance (parse-integer (post-parameter "distance")))
    (flash (format nil "Now showing activity within ~a miles." (post-parameter "distance")))
    (see-other (or (post-parameter "next") "/home")))

  (when (equalp (post-parameter "help") "0")
    (modify-db *userid* :help nil)
    (see-other (or (referer) "/home")))

  (when (equalp (post-parameter "help") "1")
    (modify-db *userid* :help t)
    (see-other (or (referer) "/home")))

  (when (and (post-parameter "confirm-location")
        (getf *user* :lat)
        (getf *user* :long))
    (modify-db *userid* :location t)
    (see-other (or (post-parameter "next") "/home"))))
