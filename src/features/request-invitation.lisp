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

(defun new-invite-request-notice-handler ()
  (send-invite-request-notification-email (getf (cddddr *notice*) :id)))

(defun create-invite-request (&key name email address offering into events resources invite gratitude other (time (get-universal-time)))
  (let ((invite-request (insert-db `(:type :invite-request
                                     :name ,name
                                     :email ,email
                                     :address ,address
                                     :offering ,offering
                                     :into ,into
                                     :events ,events
                                     :resources ,resources
                                     :invite ,invite
                                     :gratitude ,gratitude
                                     :other ,other
                                     :requested ,time))))
  (notice :new-invite-request :time :time :id invite-request)
  invite-request))

(defun index-invite-request (id data)
  (let ((result (make-result :time (getf data :requested)
                             :type :invite-request
                             :id id)))

    (with-locked-hash-table (*db-results*)
      (setf (gethash id *db-results*) result))

    (with-mutex (*invite-request-mutex*)
      (push result *invite-request-index*))))

(defun delete-invite-request (id)
  (let ((result (gethash id *db-results*)))
    (with-locked-hash-table (*db-results*)
      (remhash id *db-results*))
    (when (member id *invite-request-index* :key #'result-id)
      (with-mutex (*invite-request-mutex*)
       (asetf *invite-request-index* (remove id it :key #'result-id))))
    (remove-from-db id)))

(defun request-invitation (&key error name email address offering into events resources invite gratitude other)
  (standard-page
    "Request an Invitation"
    (html
      (when error
        (case error
          (:name
            (flash "Please enter your true full name (first and last).":error t))
          (:email
            (flash "Please correct your email address.":error t))
          (:address
            (flash "Please correct your street address (minimum City, State, and Postal Code).":error t))
          (:offering
            (flash "Please tell us a little more about your skills and what you would like to share with your community."))
          (:into
            (flash "Please tell us a little more about what you are into."))))
      (:div :id "request-invite" :class "settings-item" (:form :method "post" :action "/request-invitation" 
          (:div
            (:label :for "name" "Full Name")
            (:input :type "text" :name "name" :value name :placeholder "Enter your full name"))
          (:div
            (:label :for "email" "Email Address")
            (:input :type "text" :name "email" :value email :placeholder "Please enter your email address"))

          (:div
            (:label :for "address" "Street Address (for information on how we use your location data, please see our " (:a :href "/privacy" "privacy policy")").")
            (:input :type "text" :name "address" :value address :placeholder "Please enter your street address"))

          (:div
            (:label :for "offering" "Please tell us about your skills and what you would like to share with your community: (minimum 150 characters)")
            (:textarea  :cols "150" :rows "5" :name "offering" :placeholder "skills, tools, materials, work or meeting space, housing, food, or other resources..." (str offering)))

          (:div
            (:label :for "into" "What else are you into? (minimum 150 characters)")
            (:textarea :cols "150" :rows "5" :name "into" :placeholder "Interests, activites, etc." (str into)))
          
          (:p "Are you willing to help promote Kindista in your local community?")
          (:ul
            (:li (:input :type "checkbox"
                         :name "events"
                         :checked (when events "on")
                         "I'll help organize an event to attract new members near me!"))
            (:li (:input :type "checkbox"
                         :name "resources"
                         :checked (when resources "on")
                         "I'll post at least 5 offers and 5 requests to help seed my local sharing network!")) 
            (:li (:input :type "checkbox"
                         :name "invite"
                         :checked (when invite "on")
                         "I'll invite at least 10 people who live near me and encourage them to post offers and requests to help grow my network!"))
            (:li (:input :type "checkbox"
                         :name "gratitude"
                         :checked (when gratitude "on")
                         "I'll post statements of gratitude for at least 5 people who are sharing their generosity with me and/or my community!")))

          (:div
            (:label :for "other" "Do you have any other ideas for how you want to help grow the sharing economy in your area? (optional)")
            (:textarea :cols "150" :rows "5" :name "other" :placeholder "Any other ideas?" (str other)))
          (:div
            (:button :class "cancel"
                     :type "submit"
                     :name "cancel"
                     "Cancel")
            (:button :class "yes"
                     :type "submit"
                     :name "request-invite"
                     "Request invite")))))))

(defun get-request-invitation ()
  (cond
    (*user* 
      (flash "You already have a Kindista account.")
      (see-other "/home"))
    (t (request-invitation))))


(defun post-request-invitation ()
  (let* ((name (post-parameter "name"))
         (email (post-parameter "email"))
         (address (post-parameter "address"))
         (offering (post-parameter "offering"))
         (into (post-parameter "into"))
         (events (post-parameter "events"))
         (resources (post-parameter "resources"))
         (invite (post-parameter "invite"))
         (gratitude (post-parameter "gratitude"))
         (other (post-parameter "other")))
   (cond
    ((post-parameter "cancel")
     (see-other "/"))

    ((not (validate-name (post-parameter "name")))
     (request-invitation :name name
                         :email email
                         :address address
                         :offering offering
                         :into into
                         :events events
                         :resources resources
                         :invite invite
                         :gratitude gratitude
                         :other other
                         :error :name))

    ((not (validate-email email))
     (request-invitation :name name
                         :email email
                         :address address
                         :offering offering
                         :into into
                         :events events
                         :resources resources
                         :invite invite
                         :gratitude gratitude
                         :other other
                         :error :email))

    ((< (length address) 10)
     (request-invitation :name name
                         :email email
                         :address address
                         :offering offering
                         :into into
                         :events events
                         :resources resources
                         :invite invite
                         :gratitude gratitude
                         :other other
                         :error :address))

    ((< (length offering) 150)
     (request-invitation :name name
                         :email email
                         :address address
                         :offering offering
                         :into into
                         :events events
                         :resources resources
                         :invite invite
                         :gratitude gratitude
                         :other other
                         :error :offering))

    ((< (length into) 150)
     (request-invitation :name name
                         :email email
                         :address address
                         :offering offering
                         :into into
                         :events events
                         :resources resources
                         :invite invite
                         :gratitude gratitude
                         :other other
                         :error :into))

    (t
     (create-invite-request :name name
                            :email email
                            :address address
                            :offering offering
                            :into into
                            :events (when events t)
                            :resources (when resources t)
                            :invite (when invite t)
                            :gratitude (when gratitude t)
                            :other other)
     (flash "Thanks for your interest in joining Kindista! We'll email you after we have a chance to look over your invitation request.")
     (see-other "/home")))))
