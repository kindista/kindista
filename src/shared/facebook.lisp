;;; Copyright 2015-2021 CommonGoods Network, Inc.
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

(defparameter *fb-graph-url* "https://graph.facebook.com/")

(defvar *facebook-app-token* nil)
(defvar *facebook-user-token* nil)
(defvar *facebook-user-token-expiration* nil)
(defvar *fb-id* nil)
(defparameter *fb-share-dialog-on-page-load*
"(function() {
    FB.ui(
       {
         method: 'share',
         link: 'http://developers.facebook.com/docs/',
       },
       function(response) {
         if (response && response.post_id) {
           alert('Post was published.');
         } else {
           alert('Post was not published.');
         }
       }
     );
    });
"
)

(defun find-nil-fb-actions (&aux items)
  (dolist (result *recent-activity-index*)
    (when (find (result-type result) '(:gratitude :offer :request))
      (let* ((id (result-id result))
             (data (db id)))
        (when (and (getf data :fb-actions)
                   (not (getf (first (getf data :fb-actions))
                              :fb-action-id)))
          (push (list :userid (or (getf data :author)
                                  (getf data :by))
                      :item-id id)
                items)
          (modify-db id :fb-actions nil)))))
  (values items
          (mapcar (lambda (item) (db (getf item :userid) :name)) items)))

(defun fix-fb-publishing-error-time-data ()
  (dolist (id (hash-table-keys *db*))
    (let* ((data (db id))
           (fb-errors (getf data :fb-publishing-error))
           (new-error-data)
           (modify-data nil))
      (when fb-errors
        (dolist (fb-error fb-errors)
          (push
            (if (and (getf fb-error :time)
                     (eql (type-of (getf fb-error :time))
                          'local-time:timestamp))
              (progn
                (setf modify-data t)
                (list :time (local-time:timestamp-to-universal
                            (getf fb-error :time))
                      :userid (getf fb-error :userid)))
              fb-error)
            new-error-data))
        (when modify-data
          (modify-db id :fb-publishing-error new-error-data))))))

(defun show-fb-p (&key (userid *userid*) (user *user*))
  (and (getf user :fb-id)
       (getf user :fb-link-active)
       (getf user :fb-token)
      ;(or *enable-facebook-posting*
      ;    (find userid *alpha-testers*)
      ;    (getf user :test-user))
       *enable-facebook-posting*))

(defun fix-fb-link-active-error ()
  (dolist (id *active-people-index*)
    (let ((person (db id)))
      (when (and (getf person :fb-link-active)
                 (not (getf person :fb-id)))
        (modify-db id :fb-link-active nil)))))

(defun get-facebook-app-token ()
  (getf
    (alist-plist
      (decode-json-octets
        (http-request
          (url-compose "https://graph.facebook.com/oauth/access_token"
                       "client_id" *facebook-app-id*
                       "client_secret" *facebook-secret*
                       "grant_type" "client_credentials"))))
    :access--token))

(defun current-fb-token-p (&optional permission)
  (with-user (and (integerp *facebook-user-token-expiration*)
                  (> *facebook-user-token-expiration*
                     (+ (get-universal-time) +day-in-seconds+))
                  (not (facebook-token-validation-error-p *userid*))
                  (or (not permission)
                      (check-facebook-permission permission)))))

(defun renew-fb-token
  (&key item-to-publish
        (next "/home")
        fb-permission-requested)
  (setf (getf (token-session-data *token*) :fb-permission-requested)
        fb-permission-requested)
  (setf (getf (token-session-data *token*) :publish-to-fb)
        item-to-publish)
  (setf (getf (token-session-data *token*) :login-redirect)
        (url-decode next))
  (see-other (url-compose "/renew-fb-token")))

(defun facebook-item-meta-content
  (id
   typestring
   title
   &key description
        url
        image)
  (html
    (:meta :property "fb:app_id"
           :content *facebook-app-id*)
    (:meta :property "og:url"
           :content (or url
                        (strcat* +base-url+

                                 typestring
                                 (when (or (string= typestring "offer")
                                           (string= typestring "request"))
                                   "s")
                                 "/"
                                 id)))
    (:meta :property "og:title"
           :content (escape-for-html
                      (or title (s+ "Kindista " (string-capitalize typestring)))))
    (awhen description
      (htm (:meta :property "og:description"
                  :content (escape-for-html it))))
    (:meta :property "og:image:secure_url"
           :content (s+ "https://kindista.org" (or image "/media/biglogo4fb.jpg")))
    (:meta :property "og:image"
           :content (s+ "http://media.kindista.org"
                        (aif image
                          (regex-replace "/media" it "")
                          "/biglogo4fb.jpg")))))

(defun facebook-share-button (url &optional text)
  (html
    (:a :href (s+ "https://www.facebook.com/sharer/sharer.php?u="
                  +base-url+
                  (url-encode
                    (if (eql (char url 0) #\/)
                      (subseq url 1)
                      url)))
      (str (or text "Share on Facebook"))))

   ;(:div
   ;  :class "fb-share-button"
   ;  :data-href url
   ;  :data-layout "button"
   ;  :data-size "small"
   ;  :data-mobile-iframe "true"
   ;  (:a :class "fb-xfbml-parse-ignore"
   ;      :target "_blank"
   ;      :href (s+ "https://www.facebook.com/sharer/sharer.php?u="
   ;                (url-encode url)
   ;                "&amp;src=sdkpreparse")
   ;      (or text "Share on Facebook")))
    )

(defun facebook-sign-in-button
  (&key (redirect-uri "home")
        (button-text "Sign in with Facebook")
        re-request
        state
        scope)
  (asetf scope
         (strcat* (if (listp scope)
                    (format nil "~{~A,~}" scope)
                    (s+ scope ","))
                 ;"public_profile,publish_actions,email"
                  "public_profile,email"))
  (html
    (:a :class "blue facebook-button"
        :href (apply #'url-compose
                     "https://www.facebook.com/dialog/oauth"
                     (remove nil
                       (append
                         (list "client_id" *facebook-app-id*
                               "scope" scope
                               "response_type" "code,granted_scopes"
                               "redirect_uri" (url-encode
                                                (s+ +base-url+ redirect-uri)))
                         (when re-request
                           (list "auth_type" " rerequest"))
                         (when state
                           (list "state" state)))))
        (str (icon "facebook-white")) (str button-text))))

(defun get-renew-fb-token
  (&aux (next (or (get-parameter-string "next") "/home"))
        (new-fb-item (getf (token-session-data *token*) :publish-to-fb))
        (permission-requested (getf (token-session-data *token*) :fb-permission-requested))
        (item-type (string-downcase (awhen new-fb-item (symbol-name (db it :type))))))
  (standard-page
    (if permission-requested
     "Authorize Facebok App"
     "Reauthorize Facebook App")
   (html
     (:h2
       (str (aif permission-requested
              (case it
                (:publish-actions
                  "Kindista needs your permission to be able to publish items to Facebook")
                (t "Kindista needs special permission for this action on Facebook"))
              "Your Facebook session has expired on Kindista")))
     (str (facebook-sign-in-button
            :redirect-uri "/login"
            :button-text (if permission-requested
                           "Authorize Facebook"
                           "Reauthorize Facebook")
            :re-request permission-requested
            :scope (mapcar (lambda (permission)
                             (regex-replace-all
                               "-"
                               (string-downcase (symbol-name permission))
                               "_"))
                           (cons permission-requested (getf *user* :fb-permissions)))))
     (:p (:strong "Please note: ")
      (awhen item-type
        (htm "We will publish your " (str it) " to Facebook after you reauthorize Kindista's access to your Facebook account. "))
      "We are unable to publish "
      (str (if new-fb-item
             "this or any other"
             "any"))
      " items to Facebook on your behalf until you reauthorize your account. ")
     (:div :class "small"
      (:span "No longer want to use Facebook with Kindista?")
      (:form :method "post"
       :action "/settings/social"
       :class "inline-block"
       (:input :type "hidden" :name "next" :value next)
       (:button :class "simple-link red bold"
        :type "submit"
        :name "fb-logout"
        "Deactivate Facebook App"))))
    :class "fb-reauth"))

(defun facebook-debugging-log (userid response-code &rest messages)
  (with-open-file (s (s+ +db-path+ "facebook-log")
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :append)
    (let ((*print-readably* nil))
      (format s "USERID:~A STATUS:~A TIME:~A~%~{~S~%~}"
                userid
                response-code
                (local-time:now)
                messages))
    (fsync s)))

(defun register-facebook-user
  (&optional (redirect-uri "home")
   &aux (request (url-compose
                   "https://graph.facebook.com/oauth/access_token"
                   "client_id" *facebook-app-id*
                   "redirect_uri" (s+ +base-url+ redirect-uri)
                   "client_secret" *facebook-secret*
                   "code" (get-parameter "code")))
        reply)
  (when (and *token* (get-parameter "code"))
    (setf reply (multiple-value-list (http-request request :force-binary t)))
    (let* ((token-data (when (<= (second reply) 200)
                         (alist-plist (decode-json-octets (first reply))))))
      (facebook-debugging-log
        *userid*
        (second reply)
        (or token-data
            (cdr (assoc :message
                        (cdr (assoc :error (decode-json-octets (first reply)))))))
        token-data)

      token-data)))

(defun check-facebook-user-token
  (&optional (userid *userid*)
             (fb-token (or *facebook-user-token* (db userid :fb-token)))
   &aux (*user* (or *user* (db userid)))
        reply
        data)

  (setf reply
        (multiple-value-list
          (http-request
            (s+ *fb-graph-url* "debug_token")
            :parameters (list (cons "input_token" fb-token)
                              (cons "access_token" *facebook-app-token*)
                              (cons "appsecret_proof" (fb-app-secret-proof))
                              ))))

  (when (= (second reply) 200)
    (setf data
          (alist-plist
            (cdr (find :data
                       (decode-json-octets (first reply))
                       :key #'car)))))
  (facebook-debugging-log userid (second reply) fb-token data)
  (when (and (string= (getf data :app--id) *facebook-app-id*)
             (eql (safe-parse-integer (getf data :user--id))
                  (getf *user* :fb-id)))
    data))

(defun facebook-token-validation-error-p
  (&optional (userid *userid*)
   &aux (fb-response (check-facebook-user-token userid)))
  (or (not (getf fb-response :is--valid))
      (eql (getf fb-response :expires--at) 0)
      (eql 190 (getf (alist-plist (getf fb-response :error)) :code))))

(defun get-facebook-user-data
  (fb-token
   &aux initial-response
        user-data)
  (setf initial-response
        (alist-plist (decode-json-octets
                       (http-request
                         (strcat *fb-graph-url* "me")
                         :parameters (list (cons "access_token" fb-token)
                                           (cons "method" "get"))))))
  (setf user-data (flatten
                    (decode-json-octets
                      (http-request (strcat *fb-graph-url* "v12.0/" (getf initial-response :id))
                                    :parameters (list (cons "access_token" fb-token)
                                                      (cons "fields" "id,name,email")
                                                      (cons "method" "get"))))))
  (facebook-debugging-log *userid* user-data)
  user-data)

(defun get-facebook-user-id (fb-token)
  (safe-parse-integer (getf (get-facebook-user-data fb-token) :id)))

(defun fb-k-id (fb-id)
  (gethash fb-id *facebook-id-index*))

(defun get-facebook-profile-picture
  (k-user-id
   &aux (user (db k-user-id))
        (fb-token (getf user :fb-token))
        (fb-user-id (getf user :fb-id))
        (response))
  (when (and fb-token fb-user-id (getf user :fb-link-active))
   (setf response
         (multiple-value-list
           (http-request (strcat *fb-graph-url* "v12.0/" fb-user-id "/picture")
                         :parameters (list (cons "access_token" fb-token)
                                           (cons "appsecret_proof" (fb-app-secret-proof fb-token))
                                           (cons "type" "large")
                                           (cons "method" "get")))))
   (facebook-debugging-log (or k-user-id
                               *userid*
                               (token-userid *token*))
                           (second response)
                           (unless (eql (second response) 200)
                             (decode-json-octets (first response)))
                           (fourth response))
   (values (first response)
           (second response)
           (cdr (assoc :content-type (third response)))
           (fourth response))))

(defun save-facebook-profile-picture-to-avatar (k-userid)
  (multiple-value-bind
    (octet-array status-code content-type image-url)
    (get-facebook-profile-picture k-userid)
    (declare (ignore image-url))
    (when (eql status-code 200)
      (create-image octet-array content-type))))

(defun facebook-image-identifyier (k-userid)
  (multiple-value-bind
    (octet-array status-code content-type image-url)
    (get-facebook-profile-picture k-userid)
    (declare (ignore octet-array content-type))
    (case status-code
      (200 (awhen image-url (car (last (puri:uri-parsed-path it)))))
      (t :authentication-error))))

(defun get-facebook-user-permissions
  (k-id
   &optional (user (db k-id))
   &aux (fb-id (getf user :fb-id))
        (user-token (getf user :fb-token))
        response
        fb-permissions
        current-permissions)
  (when (and fb-id (getf user :fb-link-active))
     (setf response
           (multiple-value-list
             (http-request
               (strcat *fb-graph-url*
                       "v12.0/"
                       fb-id "/permissions")
               :parameters (list (cons "access_token" *facebook-app-token*)
                                 (cons "access_token" user-token)
                                 (cons "appsecret_proof" (fb-app-secret-proof user-token))
                                 (cons "method" "get")))))
     (setf fb-permissions (getf (alist-plist (decode-json-octets
                                               (first response)))
                                  :data)))
  (mapcar
    (lambda (pair)
      (push (string-to-keyword (car pair))
            (getf current-permissions (cdr pair))))
    (loop for permission in fb-permissions
          collect (cons (cdar permission)
                        (cond
                          ((string= (cdadr permission) "granted") :granted)
                          ((string= (cdadr permission) "declined") :declined)
                          ((string= (cdadr permission) "expired") :expired)
                          (t :error)))))
   ;(facebook-debugging-log k-id
   ;                        (second response)
   ;                        (strcat* "FB Permissions:" current-permissions))
    current-permissions)

(defun check-facebook-permission
  (permission
   &optional (userid *userid*)
   &aux (user (db userid))
        (saved-fb-permissions (getf user :fb-permissions))
        (current-fb-permissions (get-facebook-user-permissions userid user))
        (granted-fb-permissions (getf current-fb-permissions :granted)))
  (when (set-exclusive-or saved-fb-permissions granted-fb-permissions)
    (modify-db userid :fb-permissions granted-fb-permissions))
  (values (find permission granted-fb-permissions)
          (cond
            ((find permission (getf current-fb-permissions :declined))
             :declined)
            ((find permission (getf current-fb-permissions :expired))
             :expired)
            (t nil))))

(defun get-facebook-kindista-friends
  (&optional (k-id *userid*)
   &aux (user (db k-id))
        (user-token (getf user :fb-token))
        (fb-id (getf user :fb-id))
        (response)
        friends)

  (when (and fb-id (getf user :fb-link-active))
    (setf response
          (multiple-value-list
            (http-request
              (strcat *fb-graph-url*
                      "v3.0/"
                      fb-id "/friends")
              :parameters (list (cons "access_token" *facebook-app-token*)
                                (cons "access_token" user-token)
                                (cons "appsecret_proof" (fb-app-secret-proof user-token))
                                (cons "method" "get"))))))
  (pprint (find :data
                           (decode-json-octets (first response))
                           :key 'car))
  (terpri)
  (when (= (second response) 200)
    (setf friends
          (mapcar (lambda (friend)
                    (gethash (safe-parse-integer
                               (cdr (find :id friend :key 'car)))
                             *facebook-id-index*))
                (cdr (find :data
                           (decode-json-octets (first response))
                           :key 'car)))))
  (facebook-debugging-log k-id
                          (second response)
                          (strcat* "K-FB-Friends: " friends))
  friends)

(defun active-facebook-user-p
  (&optional (userid *userid*)
             (user (if (eql userid *userid*) *user* (db userid))))
  (and (getf user :fb-id) (getf user :fb-link-active) ))

(defun get-facebook-location-data (fb-location-id fb-token)
  (alist-plist
    (cdr
      (assoc :location
             (decode-json-octets
               (http-request (strcat *fb-graph-url*
                                     "v12.0/"
                                     fb-location-id)
                             :parameters (list (cons "access_token" fb-token)
                                               (cons "access_token" *facebook-app-token*)
                                               (cons "appsecret_proof" (fb-app-secret-proof fb-token))
                                               (cons "fields" "location")
                                               (cons "method" "get"))))))))

(defun new-facebook-action-notice-handler
  (&aux (notice-data (notice-data))
        (userid (getf notice-data :userid))
        (item-id (getf notice-data :item-id))
        (item (db item-id))
        (action-type (case (getf item :type)
                       (:gratitude "express")
                       (t "post")))
        (fb-action-id)
        (fb-object-id))

  ;; userid is included w/ new publish request but not scraping new data
  (when (and userid (not (first (fb-object-actions-by-user item-id
                                                           :userid userid))))
    (setf fb-action-id
          (publish-facebook-action item-id
                                   :userid userid
                                   :item item
                                   :action-type action-type))
    (setf fb-object-id (get-facebook-object-id item-id)))
  (cond
    ((getf notice-data :object-modified)
     (scrape-facebook-item (getf notice-data :fb-object-id)))
    (userid
      ;; update kindista DB with new facebook object/action ids
      (http-request
        (s+ +base-url+ "publish-facebook")
        :parameters (list (cons "item-id" (strcat item-id))
                          (cons "userid" (strcat userid))
                          (cons "fb-action-id" (strcat fb-action-id))
                          (cons "fb-action-type" action-type)
                          (cons "fb-object-id" (strcat fb-object-id))
                          (cons "fb-id" (strcat (db userid :fb-id))))
        :method :post))))

(defun post-new-facebook-data
  (&aux (item-id (post-parameter-integer "item-id"))
        (userid (post-parameter-integer "userid"))
        (fb-action-id (post-parameter-integer "fb-action-id"))
        (fb-object-id (post-parameter-integer "fb-object-id"))
        (fb-id (post-parameter-integer "fb-id"))
        (fb-action-type (post-parameter-string "fb-action-type")))
  (if (server-side-request-p)
    (progn
      (facebook-debugging-log
        userid
        nil
        "modifying the DB"
        (post-parameters*)
        (strcat* "fb-object-id: " fb-object-id)
        (amodify-db item-id :fb-object-id fb-object-id
                            :fb-publishing-in-process nil
                            :fb-publishing-error (unless fb-action-id
                                                   (cons (list :time (get-universal-time)
                                                               :userid userid)
                                                         it))
                            :fb-actions (if fb-action-id
                                          (cons (list :fb-id fb-id
                                                      :fb-action-type fb-action-type
                                                      :fb-action-id fb-action-id)
                                                 it)
                                          it)))
      (setf (return-code*) +http-no-content+)
      nil)
    (progn
      (setf (return-code*) +http-forbidden+)
      nil)))

(defun publish-facebook-action
  (id
   &key (item (db id))
        (action-type (case (getf item :type)
                       (:gratitude "express")
                       (t "post")))
        (userid *userid*)
   &aux (object-type (string-downcase (symbol-name (getf item :type))))
        (user (db userid))
        (user-token (getf user :fb-token))
        (reply
          (multiple-value-list
            (http-request
              (strcat *fb-graph-url*
                      "v3.0/me"
                      "/kindistadotorg:"
                      action-type)
              :parameters (list (cons "access_token" user-token)
                                (cons "method" "post")
                                (cons "appsecret_proof" (fb-app-secret-proof user-token))
                                (cons object-type
                                      (s+ "https://kindista.org"
                                          (resource-url id item)))
                                (cons "debug" "all")
                                (cons "fb:explicitly_shared" "true"))))))

  (let ((data (when (= (second reply) 200)
                (alist-plist (decode-json-octets (first reply))))))
    (facebook-debugging-log userid
                            (second reply)
                            (strcat* "ITEM-PUBLISHED-TO-FB:" id)
                            (list :fb-id (getf user :fb-id)
                                  :fb-token user-token)
                            (or data
                                (if (stringp (first reply))
                                  (first reply)
                                  (decode-json-octets (first reply)))))
    (awhen (getf data :id) (safe-parse-integer it))))

(defun get-facebook-object-id
  (k-id
   &aux (item (db k-id))
        (reply
          (multiple-value-list
             (http-request
               (strcat *fb-graph-url*)
               :parameters (list (cons "access_token" *facebook-app-token*)
                                 (cons "id" (s+ "https://kindista.org" (resource-url k-id item)))
                                 )))))

    ;; data and object are usefull for debugging
  (let* ((data (when (= (second reply) 200)
                 (alist-plist (decode-json-octets (first reply)))))
         (object (alist-plist (getf data :og--object))))
    (facebook-debugging-log *userid*
                            (second reply)
                            (strcat* "ITEM-ID:" k-id)
                            data)
    (when object (safe-parse-integer (getf object :id)))))

(defun get-user-facebook-objects-of-type
  (typestring
   &optional (userid *userid*)
   &aux (user (db userid))
        (reply
          (multiple-value-list
            (http-request
              (strcat *fb-graph-url*
                      "me"
                     ;(or *fb-id* (getf user :fb-id))
                      "/kindistadotorg:post/"
                      typestring)
              :parameters (list (cons "access_token" (getf user :fb-token))
                                (cons "method" "get"))))))

  (when (= (second reply) 200)
    (decode-json-octets (first reply))))

(defun fb-object-actions-by-user
  (k-item-id
   &key (data (db k-item-id))
        (userid *userid*)
        (fb-id (if (eql *userid* userid)
                 (getf *user* :fb-id)
                 (db userid :fb-id)))
   &aux (actions))
  (when userid
    (dolist (action (getf data :fb-actions))
      (when (eql (getf action :fb-id) fb-id)
        (push (getf action :fb-action-id) actions))))
  actions)

(defun update-facebook-object
  (k-id
   &aux (item (db k-id))
        (facebook-id (getf item :fb-object-id))
        (typestring (string-downcase (symbol-name (getf item :type))))
        (reply (multiple-value-list
                 (http-request
                   (strcat "https://graph.facebook.com/" facebook-id)
                   :parameters (list (cons "access_token"
                                           *facebook-app-token*)
                                     '("method" . "POST")
                                     (cons typestring
                                           (strcat "https://kindista.org"
                                                   (resource-url k-id item))))))))
  "Works the same as (scrape-facebook-item)"
  (when (= (second reply) 200)
    (decode-json-octets (first reply))))

(defun scrape-facebook-item
  (url-or-fb-id
   &aux (reply (multiple-value-list
                 (http-request
                   "https://graph.facebook.com/"
                   :parameters (list (cons "id"
                                           (if (integerp url-or-fb-id)
                                             (write-to-string url-or-fb-id)
                                             url-or-fb-id))
                                      (cons "access_token"
                                            *facebook-app-token*)
                                     '("scrape" . "true"))
                   :method :post)))
        (data (when (= (second reply) 200)
                (decode-json-octets (first reply)))))

  "Works the same as (update-facebook-object)"
  (facebook-debugging-log *userid*
                          (second reply)
                          url-or-fb-id
                          (or data (when (stringp (first reply))
                                     (first reply))))
  data)

(defun get-facebook-action
  (k-id
   &optional (k-userid *userid*)
             (user (or *user* (db k-userid)))
   &aux (k-item (db k-id))
        (action-id (cdr (assoc (getf user :fb-id)
                               (getf k-item :fb-actions))))
        (reply (multiple-value-list
                 (http-request
                   (strcat "https://graph.facebook.com/" action-id)
                   :parameters (list (cons "access_token"
                                           *facebook-app-token*)
                                     (cons "access_token"
                                           (getf user :fb-token))
                                     '("method" . "GET")))))
        (data))
  (unless (stringp (first reply))
    (setf data (decode-json-octets (first reply))))
  (facebook-debugging-log k-userid
                          (second reply)
                          (if (stringp (first reply))
                            (first reply)
                            data))
 (values data (second reply)))

(defun delete-facebook-action
  (facebook-action-id
   &aux (reply (multiple-value-list
                 (http-request
                   (strcat "https://graph.facebook.com/" facebook-action-id)
                   :parameters (list (cons "access_token"
                                           *facebook-app-token*)
                                     '("method" . "DELETE"))))))
 (values
   (decode-json-octets (first reply))
   (second reply)))

(defun get-taggable-fb-friends
  (&optional (userid *userid*)
             (user (if (eql userid *userid*) *user* (db userid)))
             (url (strcat *fb-graph-url*
                          "v3.0/"
                          (getf user :fb-id)
                          "/taggable_friends"))
   &aux (response)
        (user-token (getf user :fb-token)))
  "Performs a call to the FB graph taggable_friends endpoint to get a list of all taggable friends."
  (when (active-facebook-user-p userid user)
    (setf response
          (multiple-value-list
            (http-request
              url
              :parameters (list (cons "access_token" *facebook-app-token*)
                                (cons "access_token" user-token)
                                (cons "appsecret_proof" (fb-app-secret-proof user-token))
                                (cons "limit" "5000")
                                (cons "method" "get"))))))
  (case (second response)
    (200 (decode-json-octets (first response)))
    (t (facebook-debugging-log *userid*
                               (second response)
                               (if (stringp (first response))
                                 (first response)
                                 (decode-json-octets (first response)))))))

(defun get-all-taggable-fb-friends
  (&optional (userid *userid*)
             (user (if (eql userid *userid*) *user* (db userid)))
   &aux (all-results)
        (current-results)
        (next)
        (requests 0))
  "Performs multiple calls to the FB graph taggable_friends endpoint to accumulate a list of all taggable friends. Not currently needed because that endpoint allows a 'limit' get parameter to get a list of all taggable friends."
  (labels ((request-loop ()
             (setf current-results
                   (apply #'get-taggable-fb-friends (remove nil (list userid user next))))
             (setf next (cdr (find :next
                                   (cdr (find :paging current-results :key 'car))
                                   :key 'car)))
             (incf requests)
             (if all-results
               (asetf all-results (append it (cdr (find :data current-results :key 'car))))
               (setf all-results (cdr (find :data current-results :key 'car))))
             ;; don't let it loop indefinitely
             (when (and (< requests 5)
                        (stringp next))
               (request-loop))))
    (request-loop))
  all-results)

(defun find-taggable-fb-friend-by-name (k-id name)
  (find name
        (get-all-taggable-fb-friends k-id)
        :key (lambda (item)
               (cdr (find :name item :key 'car)))
        :test #'string=))

(defun facebook-taggable-friend-tokens
  (k-user-ids-to-test
   &optional (userid *userid*)
   &aux (image-identifiers (mapcar (lambda (id)
                                     (cons id (facebook-image-identifyier id)))
                                   k-user-ids-to-test))
        (logged-out-of-fb)
        (taggable-fb-friends (get-all-taggable-fb-friends userid))
        (taggable-k-users))
  "Returns and a-list of (k-userid . fb-taggable-token)"
  (flet ((get-pic-url-identifier (fb-data)
           (car
             (split "\\?"
               (car
                 (last
                   (url-parts
                     (cdr (find :url
                                (cdr (find :data
                                           (cdr (find :picture fb-data :key 'car))
                                           :key 'car))
                                :key 'car)))))))))
   (dolist (pair image-identifiers)
    (if (eql (cdr pair) :authentication-error)
      (push (car pair) logged-out-of-fb)
      (awhen (find (cdr pair)
                 taggable-fb-friends
                 :key #'get-pic-url-identifier
                 :test #'string=)
      (push (cons (car pair) (getf (alist-plist it) :id))
            taggable-k-users)))))
  (facebook-debugging-log *userid* nil "Facebook Taggable Friend Tokens" taggable-k-users logged-out-of-fb)
  (values taggable-k-users logged-out-of-fb))

(defun fb-taggable-friends-auth-warning (id-list)
  (when id-list
    (flash (s+ (name-list-all id-list)
               (if (> (length id-list) 1)
                 " are"  " is")
               " not logged into Facebook through Kindista. You can tag them in this statement of gratitude after they authorize their Facebook account through Kindista.")
           :error t)))

(defun facebook-friends-permission-html
  (&key redirect-uri
        fb-gratitude-subjects
        (cancel-link "home")
        re-request
        (page-title "Tag your Facebook friends"))
  (standard-page
    page-title
    (html
      (:div :id "tag-fb-friends-auth"
       (:p :class "large"
         "Would you like to tag "
         (:strong (str (name-list-all fb-gratitude-subjects :stringp t
                                                            :conjunction :or)))
         " in the gratitude you published to Facebook?")
       (:p :class "small"
         "To enable tagging, Facebook requires that you give Kindista access to your Facebook friends list. We respect your privacy and your relationships; we will not spam your friends.")
       (str (facebook-sign-in-button :redirect-uri redirect-uri
                                     :scope "user_friends"
                                     :state "user_friends_scope_granted"
                                     :re-request re-request
                                     :button-text "Allow Kindista to see my list of Facebook friends"))
       (:a :href cancel-link :class "gray-text cancel" "Not now")))
    :selected "people"))

(defun tag-facebook-friends-html
  (&key gratitude-id
        fb-gratitude-subjects
        (page-title "Tag your Facebook friends")
   &aux (gratitude (when gratitude-id (db gratitude-id)))
        (fb-g-subject-ids (mapcar #'cdr fb-gratitude-subjects)))
  (awhen (set-difference (getf gratitude :subjects) fb-g-subject-ids)
    (flash (s+ (name-list it :links nil :maximum-links 10)
               " cannot be tagged. Either they have not linked their Facebook"
               " accounts with Kindista or they have not given permission"
               " for Kindista to see if you are friends on Facebook.")))
  (standard-page
    page-title
    (html
      (:div :id "tag-fb-friends"
       (:h3 "You have published this Statement of Gratitude on your Facebook feed:")
       (:blockquote
         (str (gratitude-activity-item (gethash gratitude-id *db-results*)
                                       :show-actions nil)))

       (:form :method "post" :action (strcat "/gratitude/" gratitude-id)
         (:fieldset
           (:legend (str page-title))
           (dolist (pair fb-gratitude-subjects)
             (htm
               (:div :class "friend-to-tag"
                  (:div :class "g-recipient item"
                    (:input :type "checkbox"
                            :name "tag-fb-friend"
                            :value (cdr pair)
                            :id (cdr pair)
                            :checked "")
                    (:label :for (cdr pair)
                     (:img :src (get-avatar-thumbnail (cdr pair) 70 70))
                     (str (car pair))))))))
         (:p (:button :class "cancel" :type "submit" :class "cancel" :name "cancel" "Cancel")
             (:button :class "yes" :type "submit" :class "submit" :name "tag-friends" "Tag Friends")))))
    :selected "people"))

(defun tag-facebook-friends
  (k-item-id
   k-contacts-to-tag-on-fb
   &optional (userid *userid*)
   &aux (item (db k-item-id))
        (user (if (eql userid *userid*) *user* (db userid)))
        (user-token (getf user :fb-token))
        (response)
        (taggable-friend-tokens (facebook-taggable-friend-tokens
                                  k-contacts-to-tag-on-fb
                                  userid))
        ;; taggable-friend-tokens is an a-list of (k-userid . fb-taggable-token)
        (friends-to-tag)
        (tagged-friends)
        (action-id (first (fb-object-actions-by-user k-item-id
                                                     :data item
                                                     :userid userid
                                                     :fb-id (getf user :fb-id)))))
  ;; Tagging is convoluted until Facebook changes it's API to allow apps to pass
  ;; FB user ids to tag an item. Now we have to associate taggable-tokens with known profile
  ;; pic urls associated with fb-ids.

  (dolist (id k-contacts-to-tag-on-fb)
    (awhen (assoc id taggable-friend-tokens)
      (push (car it) tagged-friends)
      (push (cdr it) friends-to-tag)))
  (when (and friends-to-tag (active-facebook-user-p userid user))
    (asetf friends-to-tag (separate-with-commas it :omit-spaces t))
    (setf response
          (multiple-value-list
            (http-request
              (strcat *fb-graph-url*
                      "v3.0/"
                      action-id)
              :parameters (list (cons "access_token" user-token)

                                (cons "appsecret_proof" (fb-app-secret-proof user-token))
                                (cons "tags" friends-to-tag))
              :method :post)))
    (setf response (decode-json-octets (first response)))
    (when (and (eql (caar response) :success)
               (eql (cdar response) t))
      (flash (s+ (name-list-all tagged-friends) " have been tagged in your statement of gratitude on Facebook."))
      (amodify-db k-item-id :fb-tagged-friends (append tagged-friends it)))
    (facebook-debugging-log userid
                            nil
                            action-id
                            (strcat* "Contacts to tag on fb: "
                                     k-contacts-to-tag-on-fb)
                            taggable-friend-tokens
                            friends-to-tag
                            response)
    response))


(defun fb-app-secret-proof
  (&optional (access-token *facebook-app-token*)
             &aux (hmac (ironclad:make-hmac (ironclad:ascii-string-to-byte-array *facebook-secret*) :sha256))
        (hmac-hash (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array access-token)))
        (digest (ironclad:hmac-digest hmac-hash)))
          (ironclad:byte-array-to-hex-string digest))


(defun post-uninstall-facebook
  (&aux (signed-request (post-parameter "signed_request"))
        (split-request (split "\\." signed-request))
        (signature (substitute #\+ #\- (substitute #\/ #\_ (first split-request))))
        (expected-sig)
        (raw-data (second split-request))
        (hmac (ironclad:make-hmac (string-to-octets *facebook-secret*) :sha256))
        (json)
        (fb-id)
        (userid))

  (ironclad:update-hmac hmac (string-to-octets raw-data))
  (setf expected-sig
        (remove #\= (base64:usb8-array-to-base64-string
                      (ironclad:hmac-digest hmac))))
  (facebook-debugging-log nil
                          nil
                          "Uninstalling FB"
                          signed-request
                          expected-sig
                          signature)
  (if (equalp expected-sig signature)
    (progn
      (setf json
            (json:decode-json-from-string
              (with-output-to-string (s)
                (base64:base64-string-to-stream raw-data :uri t :stream s))))
      (setf fb-id (safe-parse-integer (getf (alist-plist json) :user--id)))
      (setf userid (gethash fb-id *facebook-id-index*))
      (facebook-debugging-log userid nil json)
      (when userid
        ;; testing environment may result in no userid if
        ;; user signed up on live server or vice versa.
        (modify-db userid :fb-link-active nil
                          :fb-token nil
                          :fb-expires (get-universal-time)))
      (setf (return-code*) +http-no-content+)
      nil)
    (progn (setf (return-code*) +http-forbidden+)
           nil)))
