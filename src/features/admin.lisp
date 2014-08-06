;;; Copyright 2012-2013 CommonGoods Network, Inc.
;;;
;; This file is part of Kindista.
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

(defun get-admin ()
  (require-admin
    (standard-page
      "Admin"
      (html
        (:div :id "admin-page"
          (:h1 "Admin")
          (:ul
            (:li (:a :href "/admin/metrics" "user metrics"))
            (:li (:a :href "/admin/pending-accounts" "pending accounts to review"))
            (:li (:a :href "/admin/matchmaker" "matchmaker"))
            (:li (:a :href "/admin/recent" "recently added"))
            (:li (:a :href "/admin/sendmail" "send email to everyone")))))
      :selected "admin")))

(defun get-admin-metrics ()
  (require-admin
    (standard-page
      "Admin"
       (html
         (:p (:a :href "/admin" "back to admin"))
         (:img :src "/admin/metrics/metrics.png")))))

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
            (labels ((inventory-item (id data preposition type)
                       (html
                         (str (timestamp (getf data :created)))
                         (:p (str link)
                           (str (s+ " posted " preposition " " type)))
                         (:blockquote :class "review-text"
                           (awhen (getf data :title)
                             (htm (:strong (str it))))
                           (awhen (getf data :details)
                             (htm (:br)
                                  (str it))))
                         (:p (:strong "Tags: ")
                          (str (format nil *english-list* (getf data :tags))))
                         (:form :method "post"
                                :action (strcat "/" type "s/" id)
                           (:button :type "submit"
                                    :class "cancel"
                                    :name "delete-pending-item"
                                    (str (s+ "Delete junk data")))
                           (:button :type "submit"
                                    :class "cancel"
                                    :name "inappropriate-item"
                                    (str (s+ "Inappropriate " type)))))))

              (when items
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
                                  (str (inventory-item item data "an" "offer")))
                                (:request
                                  (str (inventory-item item data "a" "request"))))))))
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
                                   "Approve account"))))))))))))))

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
                (index-item item-id item)
                (case (getf item :type)
                  (:gratitude
                    (notice :new-gratitude :id item-id))
                  (:offer
                    (update-matchmaker-offer-data item-id)))))
           (with-locked-hash-table (*pending-person-items-index*)
             (remhash userid *pending-person-items-index*)))
         (notice :account-approval :id userid
                                   :text (post-parameter "message"))
         (flash (strcat "You have approved "
                        (getf user :name)
                        "'s account: "
                        userid))
         (see-other "/admin/pending-accounts"))))))

(defun get-admin-matchmaker ()
  (require-admin
    (let ((page (if (scan +number-scanner+ (get-parameter "p"))
                (parse-integer (get-parameter "p"))
                0)))
      (standard-page
        "Admin Matchmaker"
        (html
          (:p (:a :href "/admin" "back to admin"))
          (:h2 "Admin Matchmaker")
          (:div :class "item-matches"
            (aif (requests-without-matchmakers-html :page page)
              (htm
                (str it))
              (htm "There are currently no requests without matchmakers"))))))))

(defun requests-without-matchmakers-html (&key (page 0) (count 20))
  (let* ((start (* page count))
         (requests (sublist *requests-without-matchmakers-index* start count))
         (more (nth (+ start count 1) *requests-without-matchmakers-index*)))
    (when requests
      (with-location
        (html
          (dolist (request requests)
            (str (inventory-activity-item request :truncate t
                                                  :show-distance t
                                                  :show-what t
                                                  :show-tags t)))
          (str (paginate-links page more (url-compose  "matchmaker"
                                                       "selected" "without-matchmaker"))))))))

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
            (str (activity-items (safe-sort *recent-activity-index* #'> :key #'result-time)
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
            (:button :type "submit" :name "eugene-only" :class "yes" "Send to Eugene Members")
            (:button :type "submit" :name "test" :class "yes" "Send Test")
            (:button :type "submit" :class "yes" "Send to everyone"))))
      :selected "admin")))

(defvar *last-broadcast-email-time* 0)

(defun post-admin-sendmail ()
  (require-admin
    (if (and (post-parameter "markdown")
             (post-parameter "from")
             (post-parameter "subject"))
      (let* ((text (post-parameter "markdown"))
             (html (html-email-base (strcat (nth-value 1 (markdown text :stream nil))
                                            (unsubscribe-notice-ps-html))))
             (subject (post-parameter "subject"))
             (text-with-unsubscribe (s+ text (unsubscribe-notice-ps-text)))
             (from (post-parameter "from")))
        (cond
          ((post-parameter "test")
           (cl-smtp:send-email +mail-server+
                               from
                               from
                               subject
                               text-with-unsubscribe
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

          ((< *last-broadcast-email-time* (- (get-universal-time) 900))
           (setf *last-broadcast-email-time* (get-universal-time))
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
                                                text-with-unsubscribe
                                                :html-message html)))))))
             (dolist (id (cond
                           ((not *productionp*)
                            (when (getf *user* :admin)
                              (list *userid*)))
                           ((post-parameter "eugene-only")
                            (remove-duplicates (local-members)))
                           (t (remove-duplicates *active-people-index*))))
               (send-mail id)))))
        (flash "your message has been sent"))

      (flash "specify everything please" :error t))
    (see-other "/admin/sendmail")))
