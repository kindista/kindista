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

(defun migrate-push-registration-ids (&aux accounts-affected)
  ;move push-notification-subscriptions to a p list of client
  ;with reg-id as a key
  (dolist (id (hash-table-keys *db*))
    (let* ((push-subscription
             (copy-list (getf (db id) :push-notification-subscriptions)))
           (old-chrome-p (stringp (getf push-subscription :chrome)))
           (old-mobile-p (stringp (getf push-subscription :mobile-chrome)))
           (changes-p nil))
      (when push-subscription
        (when old-chrome-p
          (asetf (getf push-subscription :chrome) (list :reg-id it))
          (setf changes-p t))
        (when old-mobile-p
          (asetf (getf push-subscription :mobile-chrome) (list :reg-id it))
          (setf changes-p t))
        (when changes-p
          (modify-db id :push-notification-subscriptions push-subscription)
          (push id accounts-affected)))))
  accounts-affected)

(defun post-push-notification-subscription
 (&aux
   (json (alist-plist (json:decode-json-from-string
                        (raw-post-data :force-text t))))
   (subscribe-p (string= (getf json :action) "subscribe"))
   (update-p (string= (getf json :action) "update"))
   (raw-endpoint (getf json :endpoint))
   (subscription-keys (alist-plist (getf json :keys)))
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
               (old-reg-params (getf subscriptions sub-type))
               (new-reg-params (when (or subscribe-p
                                         (and old-reg-params update-p))
                                      (list :reg-id registration-id
                                            :p-256-dh (getf subscription-keys :p-256-dh)
                                            :auth (getf subscription-keys :auth)))))
          (setf (getf subscriptions sub-type)
               ;; change reg ID or unsubscribe
               new-reg-params)
          (modify-db *userid* :push-notification-subscriptions subscriptions)
          ;when trying to update but not subscribed
          (when (and update-p (not new-reg-params))
            (setf status-json (list (cons "subscriptionStatus" "unsubscribed")))))
        (setf (return-code*) +http-ok+)
        (json:encode-json-to-string status-json)))))

(defun get-encrypted-message
  (subscription-params
   plaintext-data
   &aux (client-public-key (getf subscription-params :p-256-dh))
        (client-auth-secret (getf subscription-params :auth))
        (encryption-results))
  (setf encryption-results
        (split ","
          (with-output-to-string (s)
            (run-program *push-encryption-path*
                         (list client-public-key
                               client-auth-secret
                               plaintext-data)
                         :output s))))
  (list :salt (first encryption-results)
        :publickey (second encryption-results)
        :encrypted-message (third encryption-results)
        :reg-id (getf subscription-params :reg-id)))


(defun send-push-through-chrome-api
(recipients
    &key
      message-title
      message-body
      message-tag
      message-url)
"https://android.googleapis.com/gcm/send is deprecated. Removing this function for now."
nil)

(defun send-push-through-chrome-api-depreciated
  (recipients
    &key
      message-title
      message-body
      message-tag
      message-url
    &aux
      (message-ellipsed (ellipsis message-body :length 100 :plain-text t))
      (icon "kindista_favicon_180.png")
      (json-message-list (list (cons "title"  message-title)
                               (cons "body"  message-ellipsed)
                               (cons "icon"  icon)
                               (cons "url" message-url)
                               (cons "tag"  message-tag)))
      (plaintext-data (json:encode-json-to-string json-message-list))
      (chrome-api-status)
      (subscriptions))
"https://android.googleapis.com/gcm/send is deprecated. Removing this function for now."
  (dolist (recipient recipients)
    (setf subscriptions (db recipient :push-notification-subscriptions))
    (dolist (client '(:chrome :mobile-chrome))
      (awhen (getf subscriptions client)
        (let ((encrypted-results (when (and (getf it :p-256-dh)
                                            (getf it :auth))
                                   (get-encrypted-message
                                      it
                                      plaintext-data))))
          (setf chrome-api-status
            (multiple-value-list
              (http-request
                "https://android.googleapis.com/gcm/send"
                :additional-headers (list (cons "Authorization"
                                                (s+ "key=" *chrome-push-secret*))
                                          (cons "Encryption"
                                                (s+ "salt=" (getf encrypted-results :salt)))
                                          (cons "Crypto-Key"
                                                (s+ "dh=" (getf encrypted-results :publickey)))
                                          (cons "Content-Encoding" "aesgcm"))
                :method :post
                :content-type "application/json"
                :external-format-out :utf-8
                :external-format-in :utf-8
                :content (json:encode-json-alist-to-string
                           (list (cons "registration_ids" (list (getf it :reg-id)))
                                 (cons "raw_data" (getf encrypted-results :encrypted-message)))))))
          (with-open-file (s (s+ +db-path+ "push-log"):direction :output
                                                      :if-exists :append
                                                      :if-does-not-exist :create)
            (let ((*print-readably* nil))
              (format s "USERID:~A STATUS:~A ~A~%~{~S~%~}"
                      recipient
                      (second chrome-api-status)
                      (getf (alist-plist (third chrome-api-status)) :date)
                      (if (stringp (first chrome-api-status))
                        (first chrome-api-status)
                        (decode-json-octets (first chrome-api-status)))))))))))
