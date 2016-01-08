;;; Copyright 2012-2015 CommonGoods Network, Inc.
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

(defun requests-help-text ()
  (welcome-bar
    (html
      (:h2 "Getting started with requests")
      (:p "Here are some things you can do to get started:")
      (:ul
        (:li (:a :href "/requests/new" "Post a request") " to the community for something you need.")
        (:li "Browse recently posted requests listed below.")
        (:li "Find specific requests by selecting keywords from the " (:strong "browse by keyword") " menu.")
        (:li "Search for requests using the search "
          (:span :class "menu-button" "button")
          (:span :class "menu-showing" "bar")
          " at the top of the screen.")))))

(defun get-requests-new ()
  (require-user
    (enter-inventory-item-details :page-title "Post a request"
                                  :action "/requests/new"
                                  :button-text "Post request"
                                  :selected "requests")))

(defun post-requests-new ()
  (post-new-inventory-item "request" :url "/requests/new"))

(defun get-request
  (id
   &key all-terms
        any-terms
        without-terms
        distance
        notify-matches
   &aux (k (get-parameter-string "k"))
        (unverified-email (get-parameter-string "email")))

  (unless (integerp id)
    (setf id (parse-integer id)))

  (let* ((request (db id))
         (by (getf request :by))
         (unverified-userid (gethash unverified-email *email-index*))
         (unverified-user (db unverified-userid))
         (verified-user (when (string= (getf unverified-user
                                             :unsubscribe-key)
                                       k)
                            unverified-user))
         (action-type (get-parameter-string "action-type"))
         (userid (or (when verified-user unverified-userid)
                     *userid*))
         (self (eql userid by))
         (facebook-item-id (when (string= (referer)
                                          "https://www.facebook.com")
                             (get-parameter-integer "post_id")))
         (matchmaker-admin (matchmaker-admin-p))
         (result (gethash id *db-results*)))

    (when (and facebook-item-id
               (not (eql (getf request :facebook-id) facebook-item-id)))
      (modify-db id :facebook-id facebook-item-id))

    (cond
     ((or (not request)
          (not (eql (getf request :type) :request)))
      (not-found))

     ((and (getf request :violates-terms)
           (not self)
           (not (getf *user* :admin)))
        (item-violates-terms))

     ((and (not self)
           (item-view-denied (result-privacy result)))
       (permission-denied))

     (action-type
      ;; email buttons need a get-request
      ;; see inventory-digest-email
      (register-inventory-item-action id
                                      action-type
                                      :item request
                                      :reply t))

     ((and mine (get-parameter "deactivate"))
      (post-existing-inventory-item "request"
                                    :id id
                                    :deactivate t
                                    :url (script-name*)))

     ((and self (get-parameter "edit"))
      (post-existing-inventory-item "request"
                                    :id id
                                    :edit t
                                    :url (script-name*)))

     (t
      (with-location
        (standard-page
          "Requests"
          (html
            (:div :class "inventory-item-page"
              (when self
                (str (menu-horiz
                       (html (:a :href (s+ "/people/"
                                           (username-or-id)
                                           "/requests")
                              "show my requests"))
                      ;to manually go through all items and create matchmakers for them
                      ;(html (:a :href "/admin/matchmaker"
                      ;       "show requests without matchmakers"))
                       )))

              (unless (getf request :active)
                (htm
                  (:h2 :class "red" "This request is no longer active.")))
              (str (inventory-activity-item result :show-distance t :show-tags t))
              (str (item-images-html id))
              (when (and (getf request :active)
                         (or self
                             (group-admin-p by)
                             matchmaker-admin))
                (str (item-matches-html id :data request
                                           :self self
                                           :all-terms all-terms
                                           :any-terms any-terms
                                           :without-terms without-terms
                                           :distance distance
                                           :notify-matches notify-matches)))))
          :extra-head (facebook-item-meta-content id
                                                  "request"
                                                  (getf request :title)
                                                  (getf request :details))
          :selected "requests"))))))

(defun get-request-reply (id)
  (require-user
    (let* ((id (parse-integer id))
           (data (db id)))
      (if (eql (getf data :type) :request)
        (inventory-item-reply "request" id data)
        (not-found)))))

(defun post-request (id)
  (post-existing-inventory-item "request" :id id :url (script-name*)))

(defun get-requests ()
  (when *userid*
    (send-metric* :got-requests *userid*))
  (with-location
    (let* ((page (if (scan +number-scanner+ (get-parameter "p"))
                   (parse-integer (get-parameter "p"))
                   0))
           (q (get-parameter "q"))
           (base (iter (for tag in (split " " (get-parameter "kw")))
                       (when (scan *tag-scanner* tag)
                         (collect tag))))
           (start (* page 20)))
      (when (string= q "") (setf q nil))
      (multiple-value-bind (tags items)
          (nearby-inventory-top-tags :request :base base :q q)
        (standard-page
          "Requests"
          (inventory-body-html "a"
                               "request"
                               :base base
                               :q q
                               :items items
                               :start start
                               :page page)
          :top (when (getf *user* :help)
                 (requests-help-text))
          :search q
          :search-scope (if q "requests" "all")
          :right (html
                   (str (donate-sidebar))
                   (str (browse-inventory-tags "request" :q q :base base :tags tags)))
          :selected "requests")))))


(defun get-requests-all ()
  (with-user
    (with-location
      (let ((base (iter (for tag in (split " " (get-parameter "kw")))
                        (when (scan *tag-scanner* tag)
                          (collect tag)))))
        (multiple-value-bind (tags items)
            (nearby-inventory-top-tags :request :count 10000 :subtag-count 10)
          (declare (ignore items))
          (standard-page
           "Requests"
             (browse-all-inventory-tags "a" "request" :base base :tags tags)
             :top (when (getf *user* :help)
                   (requests-help-text))
             :selected "requests"))))))
