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
            (:li (:a :href "/admin/mail-system" "mail system administration"))
            (:li (:a :href "/admin/recent" "recently added"))
            (:li (:a :href "/admin/sendmail" "send email to everyone")))))
      :selected "admin")))

;(defun get-admin-metrics ()
;  (require-admin
;    (standard-page
;      "Admin"
;       (html
;         (:p (:a :href "/admin" "back to admin"))
;         (:div (:img :src "/admin/metrics/metrics-chart.png"))
;         (:div (:img :src "/admin/metrics/active-accounts.png"))))))

(defun get-admin-pending-accounts
  (&aux (pending-accounts (sort (hash-table-alist *pending-person-items-index*)
                                #'>
                                :key #'(lambda (account)
                                         ;; there may be no results if they
                                         ;; are inappropriate and we delete
                                         ;; them
                                         (aif (cadr account)
                                           (result-time it)
                                           0)))))
  (require-admin
    (standard-page
      "Pending Accounts"
      (html
        (:p (:a :href "/admin" "back to admin"))
        (:h2 "Kindista Accounts Pending Admin Approval")
        (dolist (account pending-accounts)
          (let* ((id (car account))
                 (person (db id))
                 (email (first (getf person :emails)))
                 (link (person-link id))
                 (items (remove nil (cdr account))))
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
                  (card nil
                    (html
                      (:p (str link)
                        " joined "
                        (str (humanize-universal-time (getf person :created))))
                      (:p (:strong "ID: ") (str id))
                      (:p (:strong "Email: ")
                       (:a :href (s+ "mailto:" email) (str email)))
                      (:p (:strong "Location: ") (str (awhen (getf person :address) it)))
                      (dolist (item items)
                        (let* ((id (result-id item))
                               (data (db id)))
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
                                  (str (inventory-item id data "an" "offer")))
                                (:request
                                  (str (inventory-item id data "a" "request"))))))))
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

(defun approve-account (userid &optional message)
  (modify-db userid :pending nil)
  (let ((results (copy-list (gethash userid
                                     *pending-person-items-index*))))
     (dolist (result results)
       (let* ((item-id (result-id result))
              (item (db item-id)))
         (index-item item-id item)
         (when (getf item :publish-fb-on-account-approval)
           (notice :new-facebook-action :item-id item-id
                                        :userid userid))
         (case (getf item :type)
           ;; We probably don't have any more gratitudes in
           ;; *pending-person-items-index* because pending accounts
           ;; can now post gratitude.
           ;; Confirm, then delete the following line.
           (:gratitude
             (notice :new-gratitude :id item-id))
           (:offer
             (update-matchmaker-offer-data item-id)))))
    (with-locked-hash-table (*pending-person-items-index*)
      (remhash userid *pending-person-items-index*)))
  (notice :account-approval :id userid :text message))

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
         (approve-account userid (post-parameter "message"))
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
                                                  :show-icon t
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
              (card id
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

(defun get-admin-mail-system ()
  (require-admin
    (let ((mailbox-count (sb-concurrency:mailbox-count *notice-mailbox*)))
      (standard-page
        "Mail System Status"
        (html
          (:h2 (str (strcat "Mail System Queue: " mailbox-count)))
          (when (> mailbox-count 0)
            (htm
              (:form :method "post" :action "/admin/mail-system"
                (:button :type "submit"
                         :class "yes"
                         :name "reboot-notice-thread"
                         "Reboot Mail System")))))
        :selected "admin"))))

(defun post-admin-mail-system ()
  (require-test ((or (find *userid* *alpha-users*) (getf *user* :admin)))
    (when (post-parameter "reboot-notice-thread")
      (reboot-notice-thread)
      (see-other (or (referer) "/admin/mail-system")))))

(defun get-admin-sendmail ()
  (require-admin (new-broadcast-html "/admin/sendmail")))

(defun post-admin-sendmail ()
  (require-admin (post-broadcast-new)
                 (see-other "/admin/sendmail")))



