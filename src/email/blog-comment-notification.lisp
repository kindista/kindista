;;; Copyright 2017 CommonGoods Network, Inc.
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

(defun send-blog-comment-notification-email (id)
  (let* ((comment (db id))
         (by (db (car (getf comment :by)) :name))
         (blog-post-id (getf comment :on))
         (blog-post (db blog-post-id))
         (author (db (getf blog-post :author)))
         (text (getf comment :text))
         (blog-url (blog-url blog-post-id :include-domain t)))
    (dolist (email (list "blog@kindista.org"
                         (first (getf author :emails)) ))
      (cl-smtp:send-email +mail-server+
                        "Kindista <noreply@kindista.org>"
                        (if *productionp*
                          email
                          *error-message-email*)
                        (s+ "Kindista blog comment from " by)
                        (blog-comment-notification-email-text by
                                                              text
                                                              blog-url)
                        :html-message (blog-comment-notification-email-html
                                        by
                                        text
                                        (getf blog-post :title)
                                        blog-url)))))

(defun blog-comment-notification-email-text (by text blog-url)
  (strcat
"New Kindista Blog Comment"
#\linefeed #\linefeed
"Posted by: " by
#\linefeed #\linefeed
"Text: " text
#\linefeed #\linefeed
"Link: " blog-url))

(defun blog-comment-notification-email-html
  (by text blog-title blog-url)
  (html-email-base
    (html
      (:p :style *style-p*
       (:h2 "New Kindista Blog Comment"))

      (:p :style *style-p* "Blog post title: " (str blog-title))

      (:p :style *style-p* "Comment posted by: " (str by))

      (:p :style *style-p* "Text: " (str text))

      (str (email-action-button blog-url "Review and Respond"))
)))


