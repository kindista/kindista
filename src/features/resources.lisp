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

;bug list
;fix top tag display
 
(defun resources-help-text ()
  (welcome-bar
    (html
      (:h2 "Getting started with resources")
      (:p "Here are some things you can do to get started:")
      (:ul
        (:li (:a :href "/resources/new" "Post a resource") " you have that someone else in the community might be able to use.")
        (:li "Browse recently posted resources listed below.")
        (:li "Find specific resources by selecting keywords from the " (:strong "browse by keyword") " menu.")
        (:li "Search for resources using the search "
          (:span :class "menu-button" "button")
          (:span :class "menu-showing" "bar")
          " at the top of the screen.")))))

(defun get-resources-new ()
  (require-user
    (enter-inventory-text :title "Post a resource"
                          :action "/resources/new"
                          :selected "resources")))
(defun post-resources-new ()
  (post-new-inventory-item "resource" :url "/resources/new"))

(defun get-resource (id)
  (setf id (parse-integer id))
  (aif (db id)
    (with-user
      (standard-page
        "First few words... | Kindista"
        (html
          (:div :class "activity"
            (str (inventory-activity-item "resource" (gethash id *db-results*) :show-distance t))))
        :selected "resources"))
    (standard-page "Not found" "not found")))

(defun post-resource (id)
  (require-user
    (setf id (parse-integer id)) 
    (aif (db id)
      (cond
        ((and (post-parameter "love")
              (member (getf it :type) '(:gratitude :resource :resource)))
         (love id)
         (see-other (or (post-parameter "next") (referer))))
        ((and (post-parameter "unlove")
              (member (getf it :type) '(:gratitude :resource :resource)))
         (unlove id)
         (see-other (or (post-parameter "next") (referer)))))
      (standard-page "Not found" "not found"))))


(defun get-resource-edit (id)
  (require-user
    (let* ((resource (db (parse-integer id))))
      (require-test ((eql *userid* (getf resource :by))
                   "You can only edit resources you have posted.")
        (enter-inventory-tags :title "Edit your resource"
                              :action (s+ "/resources/" id "/edit")
                              :text (getf resource :text)
                              :tags (getf resource :tags)
                              :button-text "Save resource"
                              :selected "resources")))))
(defun post-resource-edit (id)
  (post-existing-inventory-item "resource" :id id
                                           :url (s+ "/resources/" id "/edit")))

(defun get-resources ()
  (with-user
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
            (nearby-inventory-top-tags :resource :base base :q q)
          (standard-page
           "resources"
           (inventory-body-html "resource" :base base 
                                           :q q 
                                           :items items 
                                           :start start 
                                           :page page)
          :top (when (getf *user* :help)
                 (resources-help-text))
          :search q
          :search-scope (if q "resources" "all")
          :right (browse-inventory-tags "resource" :q q :base base :tags tags)
          :selected "resources"))))))


(defun get-resources-all ()
  (require-user
    (let ((base (iter (for tag in (split " " (get-parameter "kw")))
                      (when (scan *tag-scanner* tag)
                        (collect tag)))))
      (multiple-value-bind (tags items)
          (nearby-inventory-top-tags :resource :count 10000 :subtag-count 10)
        (standard-page
         "resources"
           (browse-all-inventory-tags "resource" :base base :tags tags)
           :top (when (getf *user* :help)
                 (resources-help-text))
           :selected "resources")))))
