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

(defmacro require-admin (&body body)
  `(with-user
     (if (getf *user* :admin)
       (progn ,@body)
       (not-found))))

(defun get-admin ()
  (require-admin
    (standard-page
      "Admin"
      (html
        (:div :id "admin-page"
          (:h1 "Admin")
          (:ul
            (:li (:a :href "/admin/invite-requests" "invitation requests"))
            (:li (:a :href "/admin/recent" "recently added"))
            (:li (:a :href "/admin/sendmail" "send email to everyone")))))
      :selected "admin")))

(defun get-admin-invite-requests ()
  (require-admin
    (standard-page
      "Kindista Invitation Requests"
      (html
        (:p (:a :href "/admin" "back to admin"))
        (:h1 "Kindista Invitation Requests")
        (dolist (result *invite-request-index*)
          (let* ((id (result-id result)) 
                 (data (db id))
                 (events (getf data :events))
                 (resources (getf data :resources))
                 (invite (getf data :invite))
                 (gratitude (getf data :gratitude)))
            (str
              (card
                (html
                  (str (h3-timestamp (getf data :requested)))
                  (:p (:strong "Invitation Request ID: ")(str id))
                  (:p (:strong "Requested by: ")(str (getf data :name)))
                  (:p (:strong "Location: ") (str (getf data :address)))
                  (:p (:strong "Resources offered:")
                   (:br)
                   (str (getf data :offering)))
                  (:p (:strong "Interests and activities:")
                   (:br)
                   (str (getf data :into)))
                  (awhen (getf data :other)
                    (htm
                      (:p (:strong "Other ideas for sharing economy:")
                        (:br)
                        (str it))))
                  (when (or events resources invite gratitude)
                    (htm
                      (:p (:strong "Offering to help with:")
                        (:ul
                          (when events (htm (:li "organizing events")))
                          (when resources (htm (:li "seeding resources")))
                          (when invite (htm (:li "inviting new users")))
                          (when gratitude (htm (:li "seeding gratitudes")))))))
                 (aif (getf data :invite-id)
                   (htm 
                     (:h2 "Invitation number " (str it) " has been sent for this request."))
                   (htm
                     (:div :class "confirm-invite"
                     (:form :method "post" 
                          :action (strcat "/admin/invite-request/" id)
                     (:textarea :cols "150" :rows "5" :name "message" :placeholder "Personal message to this person along with the response... (optional)")
                     (:button :type "submit" 
                              :name "delete" 
                              :class "cancel" 
                              "Delete request")
                     (:button :type "submit" 
                              :name "invite" 
                              :class "yes" 
                              "Send Invitation"))))))))))))))

(defun post-admin-invite-request (id)
  (require-admin
    (let* ((request-id (parse-integer id))
           (request (db request-id)))
      (cond
        ((post-parameter "delete")
         (confirm-delete :url (strcat "/admin/invite-request/" request-id)
                         :next-url "/admin/invite-requests"
                         :type "invitation request"
                         :text (strcat "An request from " (getf request :name)
                                   " , offering: \"" (getf request :offering)
                                   "\"")))
        ((post-parameter "really-delete")
         (delete-invite-request request-id)
         (flash (strcat "Invitation request " request-id " has been deleted."))
         (see-other "/admin/invite-requests"))
        ((post-parameter "invite")
         (let ((invitation-id (create-invitation (getf request :email)
                                                 :text (post-parameter "message")
                                                 :invite-request-id request-id
                                                 :expires 5184000
                                                 :host +kindista-id+
                                                 :name (getf request :name))))
                 (modify-db request-id :invite-id invitation-id))
         (flash (strcat "You have sent an invitation to Request ID: " request-id))
         (see-other "/admin/invite-requests"))))))

(defun get-admin-recent ()
  (require-admin
    (standard-page
      "Recently Added"
      (html
        (:p (:a :href "/admin" "back to admin"))
        (:h1 "recent sitewide activity")
        (let ((page (if (scan +number-scanner+ (get-parameter "p"))
                     (parse-integer (get-parameter "p"))
                     0)))
          (with-location
            (str (activity-items (sort (copy-list *recent-activity-index*) #'> :key #'result-time)
                                 :url "/admin/recent"
                                 :page page)))))
      :selected "admin")))

(defun get-admin-sendmail ()
  (require-admin
    (standard-page
      "Send Broadcast"
      (html
        (:div :class "item" :id "broadcast"
          (:p (:a :href "/admin" "back to admin"))
          (:h1 "send broadcast email") 
          (:form :action "/admin/sendmail" :method "post"
            (:p
              (:label "From:")
              (:input :type "text" :name "from" :value "\"Nicholas E. Walker\" <root@kindista.org>"))
            (:p
              (:label "Subject:")
              (:input :type "text" :name "subject"))
            (:textarea :name "markdown")
            (:button :type "submit" :name "test" :class "yes" "Send Test")
            (:button :type "submit" :class "yes" "Send to everyone"))))
      :selected "admin")))

(defun post-admin-sendmail ()
  (require-admin
    (if (and (post-parameter "markdown")
             (post-parameter "from")
             (post-parameter "subject"))
      (let* ((text (post-parameter "markdown"))
             (html (html-email-base (nth-value 1 (markdown text :stream nil))))
             (subject (post-parameter "subject"))
             (from (post-parameter "from")))
        (if (post-parameter "test")
          (cl-smtp:send-email +mail-server+
                              from
                              from
                              subject
                              text
                              :html-message html)
          (flet ((send-mail (id)
                   (let ((data (db id)))
                     (when (getf data :notify-kindista)
                       (let ((name (getf data :name))
                             (email (first (getf data :emails))))
                         (when email
                           (cl-smtp:send-email +mail-server+
                                               from
                                               (format nil "\"~A\" <~A>" name email)
                                               subject
                                               text
                                               :html-message html)))))))
            (dolist (id (hash-table-keys *db*))
              (send-mail id))))
        (flash "your message has been sent"))

      (flash "specify everything please" :error t))
    (see-other "/admin/sendmail")))
