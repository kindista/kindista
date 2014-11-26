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

(defun send-broadcast-email (broadcast-id user-id)
  (let ((data (db user-id)))
    (when (getf data :notify-kindista)
      (let* ((name (getf data :name))
             (email (first (getf data :emails)))
             (unsubscribe (getf data :unsubscribe-key))
             (broadcast (db broadcast-id))
             (author-email (car (db (getf broadcast :author) :emails)))
             (latest-blog-p (eql (getf *latest-blog-post* :id) broadcast-id))
             (markdown (if latest-blog-p
                         (getf *latest-blog-post* :markdown)
                         (markdown-file (getf broadcast :path))))
             (html-message (html-email-base
                             (strcat markdown
                                     (unsubscribe-notice-ps-html
                                       unsubscribe
                                       email
                                       "updates from Kindista"
                                       :detailed-notification-description "occasional updates like this from Kindista"))))
             (text-message (s+ markdown (unsubscribe-notice-ps-text
                               unsubscribe
                               email
                               "updates from Kindista"
                               :detailed-notification-description "occasional updates like this from Kindista"))) )
        (when email
          (cl-smtp:send-email +mail-server+
                              (if latest-blog-p
                                (or (getf *latest-blog-post* :author-email)
                                    author-email)
                                author-email )
                              (format nil "\"~A\" <~A>" name email)
                              (getf broadcast :title)
                              text-message
                              :html-message html-message)))))

  )

(defun create-broadcast (&key path title author blog-p)
  (insert-db `(:type ,(if blog-p :blog :broadcast)
               :time ,(get-universal-time)
               :title ,title
               :author ,author
               :path ,path
               )))

(defun save-broadcast
  (file
   title
   &aux (now (local-time:now))
        (hyphenated-title (hyphenate title))
        (dirname (strcat
                   *blog-path*
                   (with-output-to-string (str)
                     (format-timestring str
                                        now
                                        :format '((:year 4) #\/ (:month 2) #\/)))))
        (new-file-path (merge-pathnames dirname
                                        (s+ hyphenated-title ".md"))))

  (ensure-directories-exist dirname)
  (copy-file file new-file-path)
  new-file-path)


