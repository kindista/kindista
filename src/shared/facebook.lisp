;;; Copyright 2012-2016 CommonGoods Network, Inc.
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

(defmacro with-facebook-credentials (&body body)
  `(let ((*facebook-app-token* (or *facebook-app-token*
                                   (setf *facebook-app-token*
                                         (get-facebook-app-token))))
         (*fb-id* (getf *user* :fb-id))
         (*facebook-user-token* (getf *user* :fbtoken))
         (*facebook-user-token-expiration* (getf *user* :fbexpires)))
     ,@body))

(defun facebook-item-meta-content (id typestring title &optional description)
  (html
    (:meta :property "og:type"
           :content (s+ "kindistadotorg:" typestring))
    (:meta :property "fb:app_id"
           :content *facebook-app-id*)
    (:meta :property "og:url"
           :content (strcat* +base-url+
                             typestring
                             (when (or (string= typestring "offer")
                                       (string= typestring "request"))
                               "s")
                             "/"
                             id))
    (:meta :property "og:title"
           :content (escape-for-html
                      (or title (s+ "Kindista " (string-capitalize typestring)))))
    (awhen description
      (htm (:meta :property "og:description"
                  :content (escape-for-html it))))))

(defun get-facebook-user-data
  (&optional (userid *userid*)
   &aux (*user* (or *user* (db userid)))
        (reply (multiple-value-list
                 (with-facebook-credentials
                   (http-request (s+ *fb-graph-url* "debug_token")
                                 :parameters (list (cons "input_token"
                                                         *facebook-user-token*)
                                                   (cons "access_token"
                                                         *facebook-app-token*)))))))
  (when (= (second reply) 200)
    (cdr (find :data
                (decode-json-octets (first reply))
               :key #'car))))

(defun get-facebook-user-id
  (&optional (userid *userid*))
  (parse-integer
     (cdr (assoc :user--id (get-facebook-user-data userid)))))

(defun publish-facebook-action
  (id
   &key (userid *userid*)
   &aux (item (db id))
        (object-type (string-downcase (symbol-name (getf item :type))))
        (user (db userid))
        (reply
          (multiple-value-list
            (with-facebook-credentials
              (http-request
                (strcat *fb-graph-url*
                        "me"
                       ;(or *fb-id* (getf user :fb-id))
                        "/kindistadotorg:post")
                :parameters (list (cons "access_token" (getf user :fbtoken))
                                  (cons "method" "post")
                                  (cons object-type
                                        (s+ "https://kindista.org" (resource-url id item)))
                                  (cons "fb:explicitly_shared" "true")
                                  (cons "privacy"
                                         (json:encode-json-to-string
                                           (list (cons "value" "SELF"))))))))))

  (when (= (second reply) 200)
    (parse-integer (cdr (assoc :id
                               (decode-json-octets (first reply)))))))

(defun get-user-facebook-objects-of-type
  (typestring
   &optional (userid *userid*)
   &aux (user (db userid))
        (reply
          (multiple-value-list
            (with-facebook-credentials
              (http-request
                (strcat *fb-graph-url*
                        "me"
                       ;(or *fb-id* (getf user :fb-id))
                        "/kindistadotorg:post/"
                        typestring)
                :parameters (list (cons "access_token" (getf user :fbtoken))
                                  (cons "method" "get")))))))

  (when (= (second reply) 200)
    (decode-json-octets (first reply))))

(defun get-facebook-object-id
  (k-id
   &key (userid *userid*)
   &aux (item (db k-id))
        (object-type (string-downcase (symbol-name (getf item :type))))
        (user (db userid))
        (reply
          (multiple-value-list
            (with-facebook-credentials
              (http-request
                (strcat *fb-graph-url*)
                :parameters (list (cons "access_token" *facebook-app-token*)
                                  (cons "id" (s+ "https://kindista.org" (resource-url k-id item)))
                                  ))))))

 ;(when (= (second reply) 200)
 ;  (parse-integer (cdr (assoc :id (decode-json-octets (first reply))))))

  (when (= (second reply) 200)
    (parse-integer
      (cdr
        (assoc :id
               (cdr
                 (find :og--object
                          (decode-json-octets (first reply))
                          :key #'car)))))
  
  ))

(defun update-facebook-object
  (k-id
   &aux (item (db k-id))
        (facebook-id (getf item :fb-id))
        (typestring (string-downcase (symbol-name (getf item :type))))
        (reply (with-facebook-credentials
                (multiple-value-list
                  (http-request
                    (strcat "https://graph.facebook.com/" facebook-id)
                    :parameters (list (cons "access_token"
                                            *facebook-app-token*)
                                      '("method" . "POST")
                                      (cons typestring
                                            (url-compose
                                              (strcat "https://kindista.org/"
                                                      typestring
                                                      "s/"
                                                      k-id)))))))))
  "Works the same as (scrape-facebook-item)"
  (when (= (second reply) 200)
    (decode-json-octets (first reply))
   ))

(defun scrape-facebook-item
  (url-or-fb-id
   &aux (reply  (multiple-value-list
                  (with-facebook-credentials
                    (http-request
                      "https://graph.facebook.com/"
                      :parameters (list (cons "id"
                                              (if (integerp url-or-fb-id)
                                                (write-to-string url-or-fb-id)
                                                url-or-fb-id))
                                         (cons "access_token"
                                               *facebook-app-token*)
                                        '("scrape" . "true"))
                      :method :post)))))

  "Works the same as (update-facebook-object)"
  (when (= (second reply) 200)
    (decode-json-octets (first reply))))

(defun delete-facebook-action
  (facebook-action-id
   &aux (reply (multiple-value-list
                 (with-facebook-credentials
                   (http-request
                     (strcat "https://graph.facebook.com/" facebook-action-id)
                     :parameters (list (cons "access_token"
                                             *facebook-app-token*)
                                       '("method" . "DELETE")))))))
 (values
   (decode-json-octets (first reply))
   (second reply)
   )
  )

(defun get-facebook-app-token ()
    (string-left-trim "access_token="
      (http-request
        (url-compose "https://graph.facebook.com/oauth/access_token"
                     "client_id" *facebook-app-id*
                     "client_secret" *facebook-secret*
                     "grant_type" "client_credentials"))))

