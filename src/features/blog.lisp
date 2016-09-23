;;; Copyright 2014-2015 CommonGoods Network, Inc.
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

(defun subscribe-kindista-update-subscribers-to-blog ()
  "Subscribe all users who are receiving updates about Kindista to also receive the new blog"
  (dolist (id (hash-table-keys *db*))
    (let ((data (db id)))
      (when (eq (getf data :type) :person)
        (if (getf data :notify-kindista)
          (modify-db id :notify-blog t)
          (unless (getf data :notify-blog)
            (modify-db id :notify-blog nil)))))))

(defun index-blog
  (id
   data
   &aux (author-id (getf data :author))
        (author (db author-id))
        (result (make-result :latitude (getf author :lat)
                             :longitude (getf author :long)
                             :id id
                             :type :blog
                             :people (list author-id)
                             :time (getf data :created)
                             :tags (getf data :tags))))

  (with-locked-hash-table (*db-results*)
    (setf (gethash id *db-results*) result))

  (with-mutex (*blog-mutex*)
    (setf *blog-index*
          (safe-sort (push result *blog-index*)
                     #'>
                     :key #'result-time)))

  (ensure-blog-file id :data data))

(defun ensure-blog-file
  (id
   &key (data (db id))
        update
   &aux (created (universal-to-timestamp (getf data :created)))
        (local-dir (with-output-to-string (str)
                     (format-timestring
                       str
                       created
                       :format '((:year 4) #\/ (:month 2) #\/ (:day 2) #\/))))
        (hyphenated-title (url-encode (hyphenate (getf data :title))))
        (blog-path (s+ *blog-path* local-dir hyphenated-title)))

  (unless (and (not update) (file-exists-p blog-path))
    (let* ((data-path (s+ *broadcast-path* (getf data :path)))
           (dirname (s+ *blog-path* local-dir))
           (markdown (markdown-file data-path)))

      (ensure-directories-exist dirname)
      (with-open-file (file blog-path :direction :output
                                      :if-exists :supersede)
        (with-standard-io-syntax
          (let ((*print-pretty* t))
            (prin1 (list id markdown) file)))))))

(defun update-blog-file (id)
  (ensure-blog-file id :update t))

(defun blog-post-html
  (result
   &key contents
        preview-paragraph-count
   &aux (id (result-id result))
        (data (db id))
        (hyphenated-title (url-encode (hyphenate (getf data :title))))
        (date-string (universal-to-datestring (getf data :created)))
        (url (s+ "/blog/" date-string hyphenated-title))
        (comments (gethash id *comment-index*))
        (see-more t)
        (path (merge-pathnames (s+ *blog-path*
                                   date-string)
                               hyphenated-title)))
  (html
    (:div :class "blog item" :id id
     (:div :class "blog-title"
      (:strong (str (humanize-exact-time (getf data :created)
                                         :detailed t)))
      (:h3 (:a :href url (str (getf data :title))))
      (:strong
        "By: " (str (person-link (getf data :author)))))
     (str
       (or contents
           (with-open-file (file path :direction :input :if-does-not-exist nil)
             (when file
               (let ((contents (cadr (read file))))
                 (aif preview-paragraph-count
                   (multiple-value-bind (text shorten-p)
                     (beginning-html-paragraphs contents :count it)
                     (setf see-more shorten-p)
                     text)

                   contents))))))

     (when (and preview-paragraph-count see-more)
       (htm (:p (:a :href url "...go to full article"))))

     ;; when we make tags searchable, use (display-tags)
     (awhen (and (getf *user* :admin) (getf data :tags))
       (htm
         (:p :class "tags"
           (:strong "tags: "
            (dolist (tag it)
              (str tag)
              (unless (eql tag (car (last it)))
                (htm " &middot; ")))))))

     (:div :class "comments"
      (when comments
        (dolist (comment-id comments)
          (str (comment-card comment-id))))

      (:form :method "post" :action (strcat "/blog/" id)
       (if *user*
         (htm (:label :for "new-comment" "Post a comment")
              (:textarea :name "comment-text" :id "new-comment")
              (:button :type "submit"
               :class "yes"
               :name "post-comment"
               "Post Reply"))
         (htm (:button :type "submit"
               :name "new-comment"
               :class "green link"
               "Post a comment"
               ))))))))

(defun blog-sidebar ()
  (html
    (:h3 "Do you have a story you would like to contribute to the Kindista blog?")
    (:p "Submissions can be made by emailing "
        (:a :href "mailto:blog@kindista.org" "blog@kindista.org"))))

(defun get-blog
  (&key (page (or (get-parameter "p") 0))
        (count 10)
   &aux (start (* page count))
        (posts (sublist *blog-index* start count)))
  (standard-page
    "Kindista Blog"
    (html
      (when (getf *user* :admin)
        (str (menu-horiz
               (html (:a :href "/blog/new" "sumbit new blog post")))))
      (dolist (post posts)
        (str (blog-post-html post :preview-paragraph-count 3))))
    :top (page-title-bar "Adventures in Gift &middot; the Kindista blog"
                         )
    :right (blog-sidebar)))

(defun get-blog-new ()
 (require-admin (new-broadcast-html "/blog/new" :blog-p t)))

(defun post-blog-new ()
  (if (or (getf *user* :admin)
          (getf *user* :blogger))
    (post-broadcast-new)
    (permission-denied)))

(defun get-blog-post
  (year
   month
   day
   title
   &aux (date-path (strcat *blog-path* year "/" month "/" day "/"))
        (path (merge-pathnames date-path (url-encode title))))

  (pprint title)
  (pprint (url-parts (script-name*)))
  (terpri)
  (with-open-file (file path :direction :input :if-does-not-exist nil)
    (if file
      (let* ((file-contents (read file))
             (id (car file-contents))
             (html (cadr file-contents))
             (data (db id)))
        (standard-page
          (getf data :title)
          (blog-post-html (gethash id *db-results*) :contents html)
          :right (blog-sidebar)))
      (not-found))))

 (defun post-blog-post
   (id
    &aux (blog-id (parse-integer id))
         (data (db blog-id)))

   (require-user ()
     (acond
       ((not data)
        (not-found))
       ((post-parameter-string "comment-text")
        (create-comment :on blog-id
                        :send-email-p nil
                        :text it
                        )
        (see-other "/blog"))
       (t (not-found)))))
