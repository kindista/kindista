;;; Copyright 2012-2014 CommonGoods Network, Inc.
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
  (insert-db `(:type ,(if blog-p :blog :broadcast)
               :created ,(get-universal-time)
               :title ,title
               :author ,author
               :tags ,tags
               :amazon-smile-p ,amazon-smile-p
               :path ,path
               )))

(defun new-broadcast-html (title action-url &key blog-p)
  (html
    (:div :class "item" :id "broadcast"
      (:h2 (str title))
      (:form :action action-url
             :name "broadcast-form"
             :enctype "multipart/form-data"
             :method "post"
        (when blog-p
          (htm (:input :type "hidden" :name "blog-p" :value "t")))
        (:p
          (:label (str (if blog-p "Author:" "From:"))
           (:input :type "text" :name "from" :value "\"Benjamin Crandall\" <ben@kindista.org>")))
        (:p
          (:label (str (if blog-p "Title:" "Subject:"))
           (:input :type "text" :name "subject")))
        (:textarea :name "markdown"
                   :placeholder (if blog-p
                                  "Email teaser text..."
                                  "Uset text field OR file upload.  Not Both."
                                  ))
        (when blog-p
          (htm
            (:p
              (:label "Tags: (optional)"
               (:input :type "text" :name "tags")))))
        (:div
          (:span "Add a markdown file:")
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
        ))))

(defun send-broadcast-email (broadcast-id user-id)
  (let ((data (db user-id)))
    (when (getf data :notify-kindista)
      (let* ((name (getf data :name))
             (email (first (getf data :emails)))
             (unsubscribe (getf data :unsubscribe-key))
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
                                          (unless blog-p "occasional updates like this from Kindista")))))
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
                                 :detailed-notification-description (unless blog-p "occasional updates like this from Kindista")))) )
        (when email
          (cl-smtp:send-email +mail-server+
                              from-email
                              (format nil "\"~A\" <~A>" name email)
                              (if blog-p
                                "The Kindista Blog: Adventures in Gift"
                                (getf broadcast :title))
                              text-message
                              :html-message html-message))))))

(defun save-broadcast
  (text
   title
   &aux (now (local-time:now))
        (hyphenated-title (hyphenate title))
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

(defun post-broadcast-new ()
  (if (and (or (post-parameter "markdown") (post-parameter "markdown-file"))
             (post-parameter "from")
             (post-parameter "subject"))
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
             (subject (post-parameter-string "subject"))
             (amazon-smile-p (when (post-parameter "amazon-smile-p") t))
             (from (post-parameter "from")))

        (cond
          ((post-parameter "test")
           (cl-smtp:send-email
             +mail-server+
             "Kindista <info@kindista.org>"
             from
             subject
             (s+ text-broadcast
                 (when amazon-smile-p
                   (amazon-smile-reminder))
                 (unsubscribe-notice-ps-text
                   (getf *user* :unsubscribe-key)
                   from
                   "updates from Kindista"
                   :detailed-notification-description "occasional updates like this from Kindista"))
             :html-message (html-email-base
                             (strcat* html-broadcast
                                      (when amazon-smile-p
                                        (amazon-smile-reminder :html))
                                      (unsubscribe-notice-ps-html
                                        (getf *user*
                                              :unsubscribe-key)
                                        from
                                        "updates from Kindista"
                                        :detailed-notification-description "occasional updates like this from Kindista")))))

          ((or (not *productionp*)
               (< *last-broadcast-email-time* (- (get-universal-time) 900)))
           (setf *last-broadcast-email-time* (get-universal-time))
           (let* ((saved-file (save-broadcast text-to-save subject))
                  (saved-file-path (s+ saved-file ".md"))
                  (blog-p (when (post-parameter "blog-p") t))
                  (blog-link (s+ +base-url+ "blog/" saved-file))
                  (text-email (if blog-p
                                (strcat text-broadcast
                                        #\newline
                                        #\newline
                                        "Read the full essay here: "
                                        blog-link)
                                text-broadcast))
                  (html-email (if blog-p
                                (html
                                  (str html-broadcast)
                                  (:div :class *style-p*
                                   (:a :href blog-link
                                     "Read the full essay here")))
                                html-broadcast))
                  (broadcast (create-broadcast :path saved-file-path
                                               :title subject
                                               :author *userid*
                                               :amazon-smile-p amazon-smile-p
                                               :tags tags
                                               :blog-p blog-p)))
             (setf *latest-broadcast* (list :id broadcast
                                            :author-email from
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
                                        :user-id id)))))
        (flash "your message has been sent"))

      (flash "specify everything please" :error t))
  )

