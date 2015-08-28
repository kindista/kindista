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
           :content (or title (s+ "Kindista " (string-capitalize typestring))))
    (awhen description
      (htm (:meta :property "og:description"
                  :content it)))))

(defun decode-json-octets (octets)
  (json:decode-json-from-string (octets-to-string octets
                                                  :external-format :utf-8)))

(defun update-facebook-object
  (facebook-id
   typestring
   k-url
   &aux (reply (with-facebook-token
                 (multiple-value-list
                   (http-request
                     (url-compose (strcat "https://graph.facebook.com/"
                                          facebook-id)
                                  "access_token" *facebook-app-token*
                                  "method" "POST"
                                  typestring k-url))))))
  reply
  )

(defun scrape-facebook-item
  (url-or-fb-id
   &aux (reply  (multiple-value-list
                  (http-request
                    "https://graph.facebook.com/"
                    :parameters (list (cons "id"
                                            (if (integerp url-or-fb-id)
                                              (write-to-string url-or-fb-id)
                                              url-or-fb-id))
                                      '("scrape" . "true"))
                    :method :post))))
  (when (= (second reply) 200)
    (decode-json-octets (first reply))))

(defun delete-facebook-action 
  (url-or-fb-id
   &aux (reply  (multiple-value-list
                  (http-request
                    "https://graph.facebook.com/"
                    :parameters (list (cons "id"
                                            (if (integerp url-or-fb-id)
                                              (write-to-string url-or-fb-id)
                                              url-or-fb-id))
                                      '("scrape" . "true"))
                    :method :post))))
  (when (= (second reply) 200)
    (decode-json-octets (first reply))))

(defun get-facebook-app-token ()
 (string-left-trim (s+ *facebook-app-id* "|")
    (string-left-trim "access_token="
      (http-request
        (url-compose "https://graph.facebook.com/oauth/access_token"
                     "client_id" *facebook-app-id*
                     "client_secret" *facebook-secret*
                     "grant_type" "client_credentials")))
    
    )
  
  )

(defvar *facebook-app-token* nil)

(defmacro with-facebook-token (&body body)
  `(let ((*facebook-app-token* (or *facebook-app-token*
                                   (setf *facebook-app-token*
                                         (get-facebook-app-token)))))
     ,@body))
