;;; Copyright 2016 CommonGoods Network, Inc.
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

(defun post-push-notification-subscription
 (&aux
    (json (alist-plist (json:decode-json-from-string
                         (raw-post-data :force-text t))))
    (subscribe-p (string= (getf json :action) "subscribe"))
    (raw-endpoint (getf json :endpoint))
    (url-parts (split "\\/" raw-endpoint))
    (chrome-p (find "android.googleapis.com" url-parts :test #'string=))
    (registration-id (first (last url-parts))))
  (require-user
    (cond
      ((not chrome-p)
       (setf (return-code*) +http-not-implemented+)
       "Push Notifications have not been implemented for your browser.")
      (t
       (let ((notifications (db *userid* :chrome-push-notifications)))
         (setf (getf notifications :subscription)
               (when subscribe-p registration-id))
         (modify-db *userid* :chrome-push-notifications notifications))
       (setf (return-code*) +http-no-content+)
       nil))))

(defun send-push-through-chrome-api
  (&aux
     (registration-id "cBswLHGQV1A:APA91bHwhGopv0GJ4-k2uonkWHi9TFxXDJV6zmbjezJjZj0gRc2V1ZsvDlmdFeIKXuvsBHd2Ev8mJcUo9HjbXxZpkiA0o4AxMhmONfyD3s-HPr7bTFsyX_dBGmMBge8Jffj9cMjQeBsc")
     (registration-ids (list registration-id))
     (registration-json (json:encode-json-alist-to-string (list (cons "registration_ids" (list registration-id) ))))
   )
  (http-request "https://android.googleapis.com/gcm/send"
                :additional-headers (list (cons "Authorization" "key=AIzaSyAs-MUgFWba1amFkk6SDazVkMIcg_RfPZ4"))
                :method :post
                :content-type "application/json"
                :external-format-out :utf-8
                :external-format-in :utf-8
                :content registration-json))

(defun send-unread-notifications
  (
   &aux
    (title "New Message")
    (body "new message recieved")
    (icon "kindista_favicon_180.png")
    (tag "new-message-tag")
    (url "http://localhost/messages")
    (raw-endpoint (getf (alist-plist (json:decode-json-from-string (raw-post-data :force-text t))) :endpoint))
    (registration-id (first (last (split "\\/" raw-endpoint))))
    ;check users message queue
    ;set title body etc to specific message
    ;dequeue that message from users message queue
    (json-list ( list (cons "title"  title) (cons "body"  body) (cons "icon"  icon) (cons "url" url) (cons "tag"  tag)))
    )
  ;(pprint registration-id)
  ;(pprint json-list)
  ;(terpri)
  (json:encode-json-to-string json-list)
 ; (http-request "http://localhost/home/send-test-notification"
 ;                          :accept "application/json"
 ;                          :method :post
 ;                          :content-type "application/json"
 ;                          :external-format-out :utf-8
 ;                          :external-format-in :utf-8
;                           :redirect 100
;                   :content (json:encode-json-to-string '(("title" . "title") ("body". "body") ("icon" . "icon") ("tag" . "tag")))
;                   :want-stream t
;                           )
  )
