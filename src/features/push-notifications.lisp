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

(defun remove-push-registration
  (user-id
   sub-type
   registration-id
   &aux (subscriptions (copy-list (db user-id :push-notification-subscriptions))))
  (setf (getf subscriptions sub-type) nil)
    (with-locked-hash-table (*push-subscription-message-index*)
      (remhash registration-id *push-subscription-message-index*))
  (modify-db *userid* :push-notification-subscriptions subscriptions))

(defun post-push-notification-subscription
 (&aux
    (json (alist-plist (json:decode-json-from-string
                         (raw-post-data :force-text t))))
    (subscribe-p (string= (getf json :action) "subscribe"))
    (update-p (string= (getf json :action) "update"))
    (raw-endpoint (getf json :endpoint))
    (url-parts (split "\\/" raw-endpoint))
    (chrome-p (find "android.googleapis.com" url-parts :test #'string=))
    (mobile-chrome-p (string= (getf json :mobile) "true"))
    (registration-id (first (last url-parts)))
    (status-json (list (cons "subscriptionStatus" "null"))))
  (require-user ()
    (cond
      ((not chrome-p)
       (setf (return-code*) +http-not-implemented+)
       "Push Notifications have not been implemented for your browser.")
      (t
       (let* ((subscriptions (copy-list
                               (db *userid* :push-notification-subscriptions)))
              (sub-type (if mobile-chrome-p :mobile-chrome :chrome))
              (old-registration-id (getf subscriptions sub-type))
              (new-registration-id (when (or subscribe-p
                                             (and old-registration-id
                                                  update-p))
                                     registration-id)))
         (setf (getf subscriptions sub-type)
               ;; change reg ID or unsubscribe
               new-registration-id)
         (awhen old-registration-id
           (with-locked-hash-table (*push-subscription-message-index*)
             ;when subscribing update registration hashtable key
             (when new-registration-id
               (setf (gethash new-registration-id
                              *push-subscription-message-index*)
                     (gethash it *push-subscription-message-index*)))
             ;remove old registration when subscribing and unsubscribing
             (remhash it *push-subscription-message-index*)))
         (modify-db *userid* :push-notification-subscriptions subscriptions)
         ;when trying to update but not subscribed
         (when (and update-p (not new-registration-id))
           (setf status-json (list (cons "subscriptionStatus" "unsubscribed")))))
       (setf (return-code*) +http-ok+)
       (json:encode-json-to-string status-json)))))

(defun send-push-through-chrome-api
  (recipients
    &key
      message-title
      message-body
      message-tag
      message-url
      ;message-type
    &aux
      (registration-ids)
      (message-ellipsed (ellipsis message-body :length 100 :plain-text t))
      (message (list :title message-title
                     :body message-ellipsed
                     :tag message-tag
                     :url message-url))
      (chrome-api-status)
      (subscriptions)
      (registration-json)
      (chrome-results))

  ;get registration id's for each recipient
  ;if they are subscribed
  (dolist (recipient recipients)
    (setf subscriptions (db recipient :push-notification-subscriptions))
    ;push both desktop and mobile registration-ids
    (awhen (getf subscriptions :chrome)
      (push it registration-ids))
    (awhen (getf subscriptions :mobile-chrome)
      (push it registration-ids)))
  (when registration-ids
    (setf registration-json (json:encode-json-alist-to-string (list (cons "registration_ids" registration-ids))))

  (dolist (reg-id registration-ids)
    (with-locked-hash-table (*push-subscription-message-index*)
      (push message
        (gethash reg-id *push-subscription-message-index*))))

    (setf chrome-api-status
      (multiple-value-list
        (http-request "https://android.googleapis.com/gcm/send"
                      ;CHANGE to server key when pushing to live
                      :additional-headers (list (cons "Authorization"
                                                      (s+ "key="
                                                          *chrome-push-secret*)))
                      :method :post
                      :content-type "application/json"
                      :external-format-out :utf-8
                      :external-format-in :utf-8
                      :content registration-json)))

    (setf chrome-results
          (getf (alist-plist (decode-json-octets (first chrome-api-status)))
                :results))

    (do ((results chrome-results (rest results))
         (reg-ids registration-ids (rest reg-ids)))
        ((null results) 'done)
        (when (eql (car (first (first results))) :error)
          (with-locked-hash-table (*push-subscription-message-index*)
            (asetf (gethash (first reg-ids) *push-subscription-message-index*)
              (remove message it)))))))

(defun send-unread-notifications
  (&aux
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
  (json:encode-json-to-string json-list))
