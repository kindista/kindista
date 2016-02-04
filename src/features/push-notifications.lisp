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

(defun send-test-notification
  (&aux 
    (title "New Message")
    (body "new message recieved")
    (icon "kindista_favicon_180.png")
    (tag "new-message-tag")
    ;may need to send data in json later
    (raw-endpoint (getf (alist-plist (json:decode-json-from-string (raw-post-data :force-text t))) :endpoint))
    (registration-id (first (last (split "\\/" raw-endpoint))))
    ;check users message queue
    ;set title body etc to specific message
    ;dequeue that message from usere message queue
    (json-list ( list (cons "title"  title) (cons "body"  body) (cons "icon"  icon) (cons "tag"  tag)))
    )
  (pprint registration-id)
  (terpri)
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
