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

(defun settings-item-html (item title body &key help-text editable no-data)
  (html
    (:div :class "settings-item"
      (:div :class "settings-item title" (str title))
      (:div :class "settings-item content"
        (unless editable 
          (htm (:a :href (s+ "/settings?edit=" item) (if no-data 
                                                       (htm "Add " (str title))
                                                       (htm "Edit")))))
        (str body)
        (:p :class "help-text" (str help-text))))))

(defun settings-name (editable)
  (let ((aliases (getf *user* :aliases)))
    (settings-item-html "name" "Name"
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

(defun settings-address (editable)
  (let ((address (getf *user* :address)))
    (settings-item-html "address" "Address"
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
    :no-data (not address)
    :help-text (s+ "Addresses help people find nearby resources and requests. "
                   "Your address will never be displayed or shared; "
                   "it is used only to calculate distance. "))))

(defun get-personal-settings ()
  (standard-page
    "Settings"
    (html
      (:h2 "Settings")
      (str (settings-tabs-html "personal"))
      (str (settings-name (awhen 
                            (string= (get-parameter "edit") "name") it))) 
      (str (settings-address (awhen 
                               (string= (get-parameter "edit") "address") it))) 
      )))

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

  (when (post-parameter "address")
    (multiple-value-bind (lat long address city state country street zip)
        (geocode-address (post-parameter "address"))
      (declare (ignore country))
      (modify-db *userid* :lat lat :long long :address address :city city :state state :street street :zip zip)
      (see-other (or (post-parameter "next") "/home"))))

  (when (post-parameter "reset-location")
    (modify-db *userid* :lat nil :long nil :address nil :location nil)
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
