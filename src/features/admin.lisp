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

(defun account-approval-notice-handler ()
  (let ((data (cddddr *notice*)))
   (send-account-approval-email (getf data :id)
                               :text (getf data :text))))

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
            (:li (:a :href "/admin/pending-accounts" "pending accounts to review"))
            (:li (:a :href "/admin/invite-requests" "invitation requests"))
            (:li (:a :href "/admin/recent" "recently added"))
            (:li (:a :href "/admin/sendmail" "send email to everyone")))))
      :selected "admin")))

(defun get-admin-pending-accounts ()
  (require-admin
    (standard-page
      "Pending Accounts"
      (html
        (:p (:a :href "/admin" "back to admin"))
        (:h2 "Kindista Accounts Pending Admin Approval")
        (dolist (id (hash-table-keys *pending-person-items-index*))
          (let* ((person (db id))
                 (email (first (getf person :emails)))
                 (link (person-link id))
                 (items (gethash id *pending-person-items-index*)))
            (str
              (card
                (html
                  (:p (str link)
                    " joined "
                    (str (humanize-universal-time (getf person :created))))
                  (:p (:strong "ID: ") (str id))
                  (:p (:strong "Email: ")
                   (:a :href (s+ "mailto:" email) (str email)))
                  (:p (:strong "Location: ") (str (awhen (getf person :address) it)))
                  (dolist (item items)
                    (let ((data (db item)))
                      (htm
                        (:div :class "item pending-account"
                          (case (getf data :type)
                            (:gratitude
                              (htm
                                (str (timestamp (getf data :created)))
                                (:p (str link)
                                 " posted a statement of gratitude about "
                                 (str (name-list-all (getf data :subjects))))
                                (:blockquote :class "review-text"
                                  (str (getf data :text)))))
                            (:offer
                              (htm
                                (str (timestamp (getf data :created)))
                                (:p (str link)
                                 " posted an offer")
                                (:blockquote :class "review-text"
                                  (str (getf data :text)))
                                (:p (:strong "Tags: ")
                                 (str (format nil *english-list* (getf data :tags))))))
                            (:request
                              (htm
                                (str (timestamp (getf data :created)))
                                (:p (str link)
                                 " posted a request")
                                (:blockquote :class "review-text"
                                  (str (getf data :text)))
                                (:p (:strong "Tags: ")
                                 (str (format nil *english-list* (getf data :tags)))))))))))
                  (:div :class "confirm-invite"
                    (:form :method "post" 
                           :action (strcat "/admin/pending-accounts/" id)
                      (:textarea :cols "150" :rows "5" :name "message" :placeholder "Personal message to this person along with the approval... (optional)")
                      (:p (:strong (:em "Please email the user with the email link above before deleting their account!")))
                      (:button :type "submit"
                               :name "delete"
                               :class "cancel"
                               "Delete spammer")
                      (:button :type "submit"
                               :name "approve"
                               :class "yes"
                               "Approve account"))))))))))))

(defun post-admin-pending-account (id)
  (require-admin
    (let* ((userid (parse-integer id))
           (user (db userid)))
      (cond
        ((post-parameter "delete")
         (confirm-delete :url (strcat "/admin/pending-accounts/" userid)
                         :next-url "/admin/pending-accounts"
                         :type (strcat "user account (" userid ")")))
        ((post-parameter "really-delete")
         (delete-pending-account userid)
         (flash (strcat "Pending account " userid " has been deleted."))
         (see-other "/admin/pending-accounts"))
        ((post-parameter "approve")
         (modify-db userid :pending nil)
         (let ((item-ids (gethash userid *pending-person-items-index*)))
            (dolist (item-id item-ids)
              (let ((item (db item-id)))
                (case (getf item :type)
                  (:gratitude
                    (notice :new-gratitude :id item-id)))
                (index-item item-id item)))
           (with-locked-hash-table (*pending-person-items-index*)
             (remhash userid *pending-person-items-index*)))
         (notice :account-approval :id userid
                                   :text (post-parameter "message"))
         (flash (strcat "You have approved "
                        (getf user :name)
                        "'s account: "
                        userid))
         (see-other "/admin/pending-accounts"))))))

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
                   (str (html-text (getf data :offering))))
                  (:p (:strong "Interests and activities:")
                   (:br)
                   (awhen (getf data :bio-into)
                     (str (html-text it))))
                  (awhen (getf data :other)
                    (htm
                      (:p (:strong "Other ideas for sharing economy:")
                        (:br)
                        (str (html-text it)))))
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
           (request (db request-id))
           (text (strcat
                   (awhen (post-parameter "message")
                     (strcat
                       it
                       #\return
                       #\linefeed
                       #\linefeed
                       ))
                   (awhen (getf request :offering)
                     (strcat
                       "For your reference, here is the text you entered "
                       "about resources you might share on Kindista: "
                       #\return
                       #\linefeed
                       #\linefeed
                       it
                       #\return
                       #\linefeed
                       #\linefeed
                       "Save this email so you can cut and paste your text "
                       "into the new offers you post on Kindista."
                       )))))
      (cond
        ((post-parameter "delete")
         (confirm-delete :url (strcat "/admin/invite-request/" request-id)
                         :next-url "/admin/invite-requests"
                         :type "invitation request"
                         :text (strcat "An invitation request from "
                                       (getf request :name)
                                       " , offering: \""
                                       #\return
                                       #\linefeed
                                       #\linefeed
                                       (getf request :offering)
                                       "\"")))
        ((post-parameter "really-delete")
         (delete-invite-request request-id)
         (flash (strcat "Invitation request " request-id " has been deleted."))
         (see-other "/admin/invite-requests"))
        ((post-parameter "invite")
         (let ((invitation-id (create-invitation (getf request :email)
                                                 :text text
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
              (:input :type "text" :name "from" :value "\"Benjamin Crandall\" <ben@kindista.org>"))
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
        (cond
          ((post-parameter "test")
           (cl-smtp:send-email +mail-server+
                               from
                               from
                               subject
                               text
                               :html-message html))
          ((post-parameter "unread-mail")
           (dolist (id (users-with-new-mail))
             (let* ((data (db id))
                    (name (getf data :name))
                    (email (first (getf data :emails))))
               (when email
                (cl-smtp:send-email +mail-server+
                                    from
                                    (format nil "\"~A\" <~A>" name email)
                                    subject
                                    text
                                    :html-message html)))))

          (t
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
               (send-mail id)))))
        (flash "your message has been sent"))

      (flash "specify everything please" :error t))
    (see-other "/admin/sendmail")))
