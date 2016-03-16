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
       (let* ((notifications (copy-list
                               (db *userid* :chrome-push-notifications)))
              (old-registration-id (getf notifications :subscription))
              (new-registration-id (when subscribe-p registration-id)))
         (setf (getf notifications :subscription)
               new-registration-id)
         (awhen old-registration-id
           (with-locked-hash-table (*push-subscription-message-index*)
             ;when subscribing update registration hashtable key
             (setf (gethash new-registration-id
                            *push-subscription-message-index*)
                   (gethash it *push-subscription-message-index*))
            ;remove old registration for both subscribe and unsubscribe
             (remhash it *push-subscription-message-index*)
             ))
         (modify-db *userid* :chrome-push-notifications notifications))
       (setf (return-code*) +http-no-content+)
       nil))))

(defun send-push-through-chrome-api
  (recipients
    &key
         message-title
         message-body
         message-tag
         message-url
         message-type
   &aux
     (registration-ids)
     (message-ellipsed (ellipsis message-body :length 100 :plain-text t))
     ;(recipients (list (list :id 1) (list :id 3)))
     (message (list :title message-title
                    :body message-ellipsed
                    :tag message-tag
                    :url message-url))
     (chrome-api-status)
     ;(subscribed-push-users)
     (registration-json))

  ;get registration id's for each recipient
  ;of the notification
  ;if they are subscribed
  (dolist (recipient recipients)
          (awhen (getf (db (getf recipient :id) :chrome-push-notifications)
                                                :subscription)
                 (push it registration-ids)
                 ;(push (getf recipient :id) subscribed-push-users)
                 )
          (case message-type
            (:gratitude
               (setf (getf message :body) (s+ message-body
                                              (aif (getf recipient :group-name)
                                                it
                                                "you"))))
            ;(:offer
            ;  (setf (getf message :body) (s+ message-body
            ;                                 (aif (getf recipient :group-name)
            ;                                   it
            ;                                   "you")))
            ;  )
          ))

  (setf registration-json (json:encode-json-alist-to-string (list (cons "registration_ids" registration-ids))))

  (setf chrome-api-status
        (multiple-value-list
          (http-request "https://android.googleapis.com/gcm/send"
                :additional-headers (list (cons "Authorization" "key=AIzaSyAs-MUgFWba1amFkk6SDazVkMIcg_RfPZ4"))
                :method :post
                :content-type "application/json"
                :external-format-out :utf-8
                :external-format-in :utf-8
                :content registration-json)))

  (when (= (second chrome-api-status) 200)
      (dolist (registration registration-ids)
        (with-locked-hash-table (*push-subscription-message-index*)
          (push message
                (gethash registration *push-subscription-message-index*))))
    ))

(defun send-unread-notifications
  (
   &aux
    (raw-endpoint (getf (alist-plist (json:decode-json-from-string (raw-post-data :force-text t))) :endpoint))
    (registration-id (first (last (split "\\/" raw-endpoint))))
    (message (car (last (gethash registration-id *push-subscription-message-index*))))
    (title (getf message :title))
    (body (getf message :body))
    (icon "kindista_favicon_180.png")
    (tag (getf message :tag))
    (url (getf message :url))
    (json-list ( list (cons "title"  title) (cons "body"  body) (cons "icon"  icon) (cons "url" url) (cons "tag"  tag))))

  (with-locked-hash-table (*push-subscription-message-index*)
    ;dequeue message from users message queue
    (asetf (gethash registration-id *push-subscription-message-index*) (butlast it)))
  (json:encode-json-to-string json-list)
  )
