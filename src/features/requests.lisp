;;; Copyright 2012-2013 CommonGoods Network, Inc.
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
    (enter-inventory-text :title "Post a request"
                          :action "/requests/new"
                          :selected "requests")))
(defun post-requests-new ()
  (post-new-inventory-item "request" :url "/requests/new"))

(defun get-request (id)
  (setf id (parse-integer id))
  (aif (db id)
    (with-location
      (standard-page
        "Requests"
        (html
          (:div :class "activity"
            (str (inventory-activity-item "request" (gethash id *db-results*) :show-distance t))))
        :selected "requests"))
    (not-found)))

(defun post-request (id)
  (require-active-user
    (setf id (parse-integer id)) 
    (aif (db id)
      (cond
        ((and (post-parameter "love")
              (member (getf it :type) '(:gratitude :offer :request)))
         (love id)
         (see-other (or (post-parameter "next") (referer))))
        ((and (post-parameter "unlove")
              (member (getf it :type) '(:gratitude :offer :request)))
         (unlove id)
         (see-other (or (post-parameter "next") (referer)))))
      (not-found))))

(defun get-request-edit (id)
  (require-user
    (let* ((request (db (parse-integer id))))
      (require-test ((or (eql *userid* (getf request :by))
                         (getf *user* :admin))
                   "You can only edit your own requests.")
        (enter-inventory-tags :title "Edit your request"
                              :action (s+ "/requests/" id "/edit")
                              :text (getf request :text)
                              :tags (getf request :tags)
                              :button-text "Save request"
                              :selected "requests")))))

(defun post-request-edit (id)
  (post-existing-inventory-item "request" :id id
                                          :url (s+ "/requests/" id "/edit")))

(defun get-requests ()
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
         (inventory-body-html "request" :base base 
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
  (require-user
    (let ((base (iter (for tag in (split " " (get-parameter "kw")))
                      (when (scan *tag-scanner* tag)
                        (collect tag)))))
      (multiple-value-bind (tags items)
          (nearby-inventory-top-tags :request :count 10000 :subtag-count 10)
        (standard-page
         "Requests"
           (browse-all-inventory-tags "request" :base base :tags tags)
           :top (when (getf *user* :help)
                 (requests-help-text))
           :selected "requests")))))
