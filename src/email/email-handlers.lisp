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

(defvar *style-a* "color:#5c8a2f;
                   text-decoration:none;") 

(defvar *style-p* "margin-top:.9em;
                   margin-bottom:.9em;")

(defvar *style-quote-box* "border-collapse: collapse;
                           background: #ebf2e4;
                           margin: 8px;
                           border: thin solid #bac2b2;")

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun person-email-link (id)
  (html
    (:a :href (s+ "https://kindista.org/people/" (username-or-id id)) (str (getf (db id) :name)))))

(defun send-welcome-email (email token)
  (cl-smtp:send-email +mail-server+
                      "Kindista <noreply@kindista.org>"
                      email
                      "Welcome to Kindista!"
                      (welcome-email-text email token)
                      :html-message (welcome-email-html email token)))

(defun send-new-email (email token)
  (cl-smtp:send-email +mail-server+
                      "Kindista <noreply@kindista.org>"
                      email
                      "Verify your email address"
                      (new-email-text email token)
                      :html-message (new-email-html email token)))

(defun send-email-verification (invitation-id)
  (let* ((invitation (db invitation-id))
         (token (getf invitation :token))
         (expires (getf invitation :valid-until))
         (to (getf invitation :recipient-email)))
    (cl-smtp:send-email +mail-server+
                        "Kindista <noreply@kindista.org>"
                        to
                        "Please verify your email address."
                        (email-verification-text invitation-id 
                                                 to
                                                 token)
                        :html-message (email-verification-html invitation-id 
                                                               to 
                                                               token))))

(defun send-invitation-email (invitation-id)
  (let* ((invitation (db invitation-id))
         (token (getf invitation :token))
         (from (getf invitation :host))
         (text (getf invitation :text))
         (expires (getf invitation :valid-until))
         (to (getf invitation :recipient-email)))
    (cl-smtp:send-email +mail-server+
                        "Kindista <noreply@kindista.org>"
                        to
                        (s+ (getf (db from) :name) " has invited you to join Kindista!")
                        (invitation-email-text invitation-id 
                                               token 
                                               to
                                               from
                                               :text text)
                        :html-message (invitation-email-html invitation-id
                                                             token 
                                                             to 
                                                             from
                                                             :text text)
    )))

(defun send-gratitude-notification-email (gratitude-id)
  (let* ((gratitude (db (parse-integer gratitude-id)))
         (from (getf gratitude :author))
         (to-list (getf gratitude :subjects)))
    (dolist (to to-list)
      (cl-smtp:send-email +mail-server+
                          "Kindista <noreply@kindista.org>"
                          (car (getf (db to) :emails))
                          (s+ (getf (db from) :name) " has posted a statement of gratitude about you")
                          (gratitude-notification-email-text gratitude-id 
                                                             gratitude 
                                                             from)
                          :html-message (gratitude-notification-email-html gratitude-id gratitude from)))))

(defun send-message-notification-email (from to)
  (cl-smtp:send-email +mail-server+
                      "Kindista <noreply@kindista.org>"
                      to
                      (s+ "New message from " from)
                      (message-notification-email-text from)
                      :html-message (message-notification-email-html from)))

(defun send-comment-mine-notification-email (from to)
  (cl-smtp:send-email +mail-server+
                      "Kindista <noreply@kindista.org>"
                      to
                      (s+ from " commented on your gift")
                      (comment-mine-notification-email-text from)
                      :html-message (comment-mine-notification-email-html from)))

(defun send-comment-other-notification-email (from to)
  (cl-smtp:send-email +mail-server+
                      "Kindista <noreply@kindista.org>"
                      to
                      (s+ from " commented on a gift you received")
                      (comment-other-notification-email-text from)
                      :html-message (comment-other-notification-email-html from)))

(defun send-circle-notification-email (from to)
  (cl-smtp:send-email +mail-server+
                      "Kindista <noreply@kindista.org>"
                      to
                      (s+ from " added you to their circles on Kindista!")
                      (circle-notification-email-text from)
                      :html-message (circle-notification-email-html from)))

