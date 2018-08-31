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


(defun broadcast-email-notice-handler ()
  (let ((notice (cddddr *notice*)))
    (send-broadcast-email (getf notice :broadcast-id)
                          (getf notice :user-id))))

(defun create-broadcast (&key path title author tags blog-p amazon-smile-p)
  (insert-db (list :type (if blog-p :blog :broadcast)
                   :created (get-universal-time)
                   :title title
                   :author author
                   :tags tags
                   :amazon-smile-p amazon-smile-p
                   :path path)))

(defun new-broadcast-html
  (action-url &key blog-p from author-id subject tags markdown error-fields)
  (flet ((error? (field)
           (when (find field error-fields :test #'string=) "error")))
   (standard-page
     (if blog-p "New Blog Entry" "Send Broadcast")
     (html
       (:div :class "item" :id "broadcast"
         (:h2 (str (if blog-p
                     "Submit new blog entry"
                     "Send broadcast email" )))
         (:form :action action-url
                :name "broadcast-form"
                :enctype "multipart/form-data"
                :method "post"
           (when blog-p
             (htm (:input :type "hidden" :name "blog-p" :value "t")))
           (:div
             (:label :for "from"
                     :class (error? "from")
                     (str (if blog-p "Author:" "From:")))
             (:input :type "text"
                     :name "from"
                     :id "from"
                     :placeholder "\"Benjamin Crandall\" <ben@kindista.org>"
                     :value from))
           (:div
             (:label :class (error? "author-id")
                     :for "author-id" "Kindista username or id:")
             (:input :type "text"
               :id "author-id"
               :name "author-id"
               :value (or author-id (username-or-id *userid*))))
           (:div
             (:label :class (error? "subject")
                     :for "subject" (str (if blog-p "Title:" "Subject:")))
             (:input :type "text"
                     :id "subject"
                     :name "subject"
                     :value subject))
           (:textarea :name "markdown"
                      :placeholder (if blog-p
                                     "Email teaser text..."
                                     "Use text field OR file upload.  Not Both.")
                      (if markdown (str markdown)))
           (when blog-p
             (htm
               (:p
                 (:label :for "tags" "Tags: (optional)")
                 (:input :type "text"
                         :id "tags"
                         :name "tags"
                         :value tags))))
           (:div
             (:span :class (error? "markdown-file")
               "Add a markdown file:")
             (:br)
             (:input :type "file"
                     :name "markdown-file"
                     :onchange (ps-inline (submit-markdown-form)))
             (:div :id "spinner" :class "spinner"))
           (:div
             (:input :type "checkbox"
                     :name "amazon-smile-p"
                     :value "checked")
             "Plug Kindista at AmazonSmile")
           (:button :type "submit" :name "test" :class "yes" "Send Test")
           (:button :type "submit" :class "yes" "Send to everyone")
           (unless blog-p
             (htm
               (:button :type "submit"
                        :name "eugene-only"
                        :class "yes"
                 "Send to Eugene Members")))
           ))))))

(defun send-broadcast-email (broadcast-id user-id &aux (user (db user-id)))
  (when (and (getf user :active)
             (or (getf user :notify-blog) (getf user :notify-kindista))
             ;; check which one before sending the message below
             )
    (let* ((name (getf user :name))
           (email (first (getf user :emails)))
           (unsubscribe (getf user :unsubscribe-key))
           (broadcast (db broadcast-id))
           (blog-p (eq (getf broadcast :type) :blog))
           (author (db (getf broadcast :author)))
           (author-email (car (getf author :emails)))
           (author-name (getf author :name))
           (amazon-smile-p (getf broadcast :amazon-smile-p))
           (broadcast-path (s+ *broadcast-path* (getf broadcast :path)))
           (latest-broadcast-p (eql (getf *latest-broadcast* :id)
                                    broadcast-id))
           (from-email (cond
                         (blog-p "\"Kindista Blog\" <blog@kindista.org>")
                         (latest-broadcast-p
                           (or (getf *latest-broadcast* :author-email)
                               author-email))
                         (t author-email)))
           (text-broadcast (if latest-broadcast-p
                             (getf *latest-broadcast* :text)
                             (read-file-into-string broadcast-path)))
           (html-broadcast (if latest-broadcast-p
                             (getf *latest-broadcast* :html)
                             (markdown-file broadcast-path)))
           (html-message (html-email-base
                           (strcat* (when blog-p
                                      (html
                                        (:h2 (str (getf broadcast :title)))
                                        (:p (:strong "By: " (str author-name)))))
                                    html-broadcast
                                    (when amazon-smile-p
                                      (amazon-smile-reminder :html))
                                    (unsubscribe-notice-ps-html
                                      unsubscribe
                                      email
                                      (if blog-p
                                        "emails for new articles from the Kindista blog"
                                        "updates from Kindista")
                                      :detailed-notification-description
                                        (unless blog-p "occasional updates like this from Kindista")
                                      :unsub-type "blog"))))
           (text-message (s+ (when blog-p
                               (strcat
                                 (getf broadcast :title)
                                 #\linefeed #\linefeed
                                 "By: " author-name
                                 #\linefeed #\linefeed))
                             text-broadcast
                             (when amazon-smile-p
                               (amazon-smile-reminder))
                             (unsubscribe-notice-ps-text
                               unsubscribe
                               email
                               (if blog-p
                                 "emails for new articles from the Kindista blog"
                                 "updates from Kindista")
                               :detailed-notification-description (unless blog-p "occasional updates like this from Kindista")
                               :unsub-type "blog"))))
      (when (and email
                 (if blog-p
                   (getf user :notify-blog)
                   (getf user :notify-kindista)))
        (cl-smtp:send-email +mail-server+
                            from-email
                            (format nil "\"~A\" <~A>" name email)
                            (if blog-p
                              (strcat "Kindista Blog: " (getf broadcast :title))
                              (getf broadcast :title))
                            text-message
                            :html-message html-message)))))

(defun save-broadcast
  (text
   title
   &aux (now (local-time:now))
        (hyphenated-title (url-encode (hyphenate title)))
        (local-dir (with-output-to-string (str)
                     (format-timestring
                       str
                       now
                       :format '((:year 4) #\/ (:month 2) #\/ (:day 2) #\/))))
        (dirname (strcat *broadcast-path* local-dir))
        (filename (s+ hyphenated-title ".md"))
        (new-file-path (merge-pathnames dirname filename)))

  (ensure-directories-exist dirname)
  (with-open-file (file new-file-path :direction :output
                                      :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*print-pretty* t))
        (princ text file))))
 ;(copy-file file new-file-path)
  (s+ local-dir hyphenated-title))

(defvar *last-broadcast-email-time* 0)

(defvar *latest-broadcast*)

(defun post-broadcast-new
  (&aux (author-post-id (post-parameter-string "author-id"))
        (author-id (when author-post-id
                     (aif (safe-parse-integer author-post-id)
                       it
                       (gethash author-post-id *username-index*))))
        (admin-email (first (getf *user* :emails)))
        (author-email (post-parameter-string "from"))
        (blog-p (when (post-parameter "blog-p") t))
        (subject (post-parameter-string "subject"))
        (missing-fields))

  (let* ((text (post-parameter-string "markdown"))
         (tags (tags-from-string (post-parameter "tags")))
         (markdown-upload (post-parameter "markdown-file"))
         (text-broadcast (or text
                             (read-file-into-string
                               (first markdown-upload))))
         (html-broadcast (if text
                           (markdown-text text)
                           (markdown-file (first markdown-upload))))
         (text-to-save (if markdown-upload
                           (read-file-into-string (first markdown-upload))
                           text))
         (amazon-smile-p (when (post-parameter "amazon-smile-p") t)))

    (dolist (item '("from" "subject" "author-id" "markdown"))
      (unless (post-parameter-string item)
        (push item missing-fields)))

    (unless markdown-upload (push "markdown-file" missing-fields))

     (flet ((try-again ()
              (new-broadcast-html (if blog-p "/blog/new" "/admin/sendmail")
                                  :blog-p blog-p
                                  :from author-email
                                  :author-id author-id
                                  :subject subject
                                  :tags (post-parameter "tags")
                                  :markdown text
                                  :error-fields missing-fields )))
      (cond
        ((notevery #'identity
                   (list author-id author-email subject (or text markdown-upload)))
         (try-again))

        ((post-parameter "test")
         (cl-smtp:send-email
           +mail-server+
           "Kindista <info@kindista.org>"
           admin-email
           subject
           (s+ text-broadcast
               (when amazon-smile-p
                 (amazon-smile-reminder))
               (unsubscribe-notice-ps-text
                 (getf *user* :unsubscribe-key)
                 admin-email
                 "updates from Kindista"
                 :detailed-notification-description "occasional updates like this from Kindista"
                 :unsub-type "kindista"))
           :html-message (html-email-base
                           (strcat* html-broadcast
                                    (when amazon-smile-p
                                      (amazon-smile-reminder :html))
                                    (unsubscribe-notice-ps-html
                                      (getf *user*
                                            :unsubscribe-key)
                                      admin-email
                                      "updates from Kindista"
                                      :detailed-notification-description "occasional updates like this from Kindista"
                                      :unsub-type "kindista"))))
         (flash "test email has been sent")
         (try-again))

        ((or (not *productionp*)
             (< *last-broadcast-email-time* (- (get-universal-time) 900)))
         (setf *last-broadcast-email-time* (get-universal-time))
         (let* ((saved-file (save-broadcast text-to-save subject))
                (saved-file-path (s+ saved-file ".md"))
                (blog-link (s+ +base-url+ "blog/" saved-file))
                (text-email (if blog-p
                              (strcat text-broadcast
                                      #\linefeed
                                      #\linefeed
                                      "Read the full essay here: "
                                      blog-link)
                              text-broadcast))
                (html-email (if blog-p
                              (html
                                (str html-broadcast)
                                (:div :style *style-p*
                                 (:a :style *style-a*
                                     :href blog-link
                                     "Read the full essay here")))
                              html-broadcast))
                (broadcast (create-broadcast :path saved-file-path
                                             :title subject
                                             :author author-id
                                             :amazon-smile-p amazon-smile-p
                                             :tags tags
                                             :blog-p blog-p)))
           (setf *latest-broadcast* (list :id broadcast
                                          :author-email author-email
                                          :text text-email
                                          :html html-email))
           (dolist (id (cond
                         ((not *productionp*)
                          (when (getf *user* :admin)
                            (list *userid*)))
                         ((post-parameter "unread-mail")
                          (users-with-new-mail))
                         ((post-parameter "eugene-only")
                          (remove-duplicates (local-members)))
                         (t (remove-duplicates *active-people-index*))))

             (notice :broadcast-email :time (get-universal-time)
                     :broadcast-id broadcast
                     :user-id id))
           (flash "your message has been sent")
           (see-other (if blog-p (strcat "/blog/" saved-file) "/admin"))))))))

