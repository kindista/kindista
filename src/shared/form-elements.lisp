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

(defun number-selection-html (name end &optional selected)
  (html
    (:select :name name
      (loop for num from 1 to end
            do (htm
                 (:option :value (strcat num)
                          :selected (when (eql selected num) "")
                          (str (strcat num))))))))

(defun group-category-selection (&key next selected submit-buttons (class "identity"))
  (let* ((default-options '("business"
                            "church/spiritual community"
                            "community organization"
                            "government agency"
                            "intentional community"
                            "nonprofit organization"
                            "school/educational organization"))
         (custom (unless (member selected
                                 (cons "other" default-options)
                                 :test #'equalp)
                   selected)))
    (html
      (awhen next (htm (:input :type "hidden" :name "next" :value it)))
      (:select :name "group-category"
               :class (s+ "group-category-selection " class)
               :onchange "this.form.submit()"
         (unless selected
           (htm (:option :value ""
                         :style "display:none;"
                         :selected "selected"
                   "Please select ...")))
         (dolist (option default-options)
           (htm (:option :value option
                         :selected (when (string= selected option) "")
                         (str (string-capitalize option)))))
         (:option :value "other"
                  :selected (when (or (string= selected "other")
                                      custom)
                              "")
                  "Other ..."))

      (:input :type "submit" :class "no-js" :value "apply")

      (when (or (string= selected "other") custom)
        (htm
          (:br)
          (:input :type "text"
                  :class "group-category-selection float-left"
                  :name "custom-group-category"
                  :placeholder (if submit-buttons
                                 "What type of group is this?"
                                 "Please specify...")
                  :value (awhen custom (escape-for-html it)))
          (when submit-buttons
            (htm
              (:div :class "group-category-buttons float-left"
               (:button :type "input" :class "yes input-height" "Save Changes")
               (:button :type "input" :class "cancel input-height" :name "cancel" "Cancel")))))))))

(defun group-membership-method-selection (current &key auto-submit)
  (html
    (:h3 :class "membership-settings" "How can members join this group?")
    (:input :type "radio"
            :name "membership-method"
            :class "membership-settings"
            :value "group-admin-approval"
            :onclick (when auto-submit "this.form.submit()")
            :checked (unless (string= current "invite-only") ""))
    "Anyone can request to join this group. Admins can invite people to join and approve/deny membership requests."
    (:br)
    (:input :type "radio"
            :name "membership-method"
            :class "membership-settings"
            :value "invite-only"
            :onclick (when auto-submit "this.form.submit()")
            :checked (when (string= current "invite-only") "checked"))
    "By invitaion only. Members must be invited by group admins and cannot request membership."))

(defun identity-selection-html (selected groups &key (class "identity") onchange)
"Groups should be an a-list of (groupid . group-name)"
  (html
    (:select :name "identity-selection" :class class :onchange onchange
      (:option :value *userid*
               :selected (when (eql selected *userid*) "")
               (str (getf *user* :name))" ")
      (dolist (group (sort groups #'string< :key #'cdr))
        (htm (:option :value (car group)
                      :selected (when (eql selected (car group)) "")
                      (str (cdr group))" "))))))

(defun privacy-selection-html (item-type restrictedp my-groups groups-selected &key (class "privacy-selection") onchange)
"Groups should be an a-list of (groupid . group-name)"
  (let* ((my-group-ids (mapcar #'car my-groups))
         (group-ids-user-has-left (set-difference groups-selected my-group-ids))
         (groups-user-has-left (mapcar #'(lambda (id) (cons id (db id :name)))
                                       group-ids-user-has-left)))
    (html
      (:h2  "Who can see this " (str item-type) "?")
      (:div :class (s+ class (when (and restrictedp
                                        (or groups-user-has-left
                                            (> (length my-groups) 1)))
                               " privacy-selection-details"))
        (:select :name "privacy-selection" :class class :onchange onchange
          (:option :value "public"
                   :selected (unless restrictedp "")
                   "Anyone")
          (:option :value "restricted"
                   :selected (when restrictedp "")
                   (str
                     (cond
                      ((= 1 (length my-groups))
                       (s+ (cdar my-groups)
                           (when (= (caar my-groups) +kindista-id+)
                             " account group")
                           " members "))
                      (groups-user-has-left
                       "Groups I'm no longer a member of")
                      (t "People in my groups")))))
        (when restrictedp
          (if (or groups-user-has-left
                  (> (length my-groups) 1))
            (progn
              (dolist (group (sort my-groups #'string-lessp :key #'cdr))
                (htm
                  (:br)
                  (:div :class "item-group-privacy"
                    (:input :type "checkbox"
                            :name "groups-selected"
                            :checked (when (or (not groups-selected)
                                               (member (car group) groups-selected))
                                       "checked")
                            :value (car group)
                            (str (cdr group))
                            (str (when (= (car group) +kindista-id+)
                                 " group account "))
                            " members"))))
              ;; for groups the user has left but are still being shown 
              ;; this item
              (dolist (group groups-user-has-left)
                (htm
                  (:br)
                  (:div :class "item-group-privacy"
                    (:input :type "checkbox"
                            :name "groups-selected"
                            :checked (when (member (car group) groups-selected)
                                       "checked")
                            :value (car group)
                            (str (cdr group))
                            (str (when (= (car group) +kindista-id+)
                                 " group account "))
                            " members")))))
            (htm (:input :type "hidden" :name "groups-selected" :value (caar my-groups))))))
      (when (and groups-user-has-left restrictedp)
        (let ((plural (> (length groups-user-has-left) 1)))
          (htm
            (:br)
            (:div :class "privacy-selection-warning"
              (:p
                (:span :class "red" "Warning: ")
                "You are no longer a member of the following group"
                (when plural (str "s"))
                ":"
                (:br)
              (str (format nil *english-list* (mapcar #'cdr groups-user-has-left))))
              (:p "Members of "
                  (str (if plural "those groups" "that group"))
                  " will be able to continue to see this "
                  (str item-type)
                  " until you edit this privacy setting and resave your "
                  (str item-type)
                  "."))))))))

