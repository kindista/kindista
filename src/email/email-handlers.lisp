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
    (:a :href (strcat +base-url+ "/people/" (username-or-id id)) (str (getf (db id) :name)))))

(defun person-name (id)
  (db id :name))

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

(defun send-password-reset (user email)
  (let* ((name (getf user :name))
         (token (car (getf user :password-reset-token)))
         (expiration (humanize-future-time
                    (cdr (getf user :password-reset-token)))))
    (cl-smtp:send-email +mail-server+
                        "Kindista <noreply@kindista.org>"
                        email
                        "Forgotten Password"
                        (reset-password-text name
                                             token
                                             email
                                             expiration)
                        :html-message (reset-password-html name
                                                           token
                                                           email
                                                           expiration))))

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

(defun send-comment-notification-email (comment-id)
  (let* ((comment (db comment-id))
         (conversation-id (getf comment :on))
         (conversation (db conversation-id))
         (subject (getf conversation :subject))
         (text (getf comment :text))
         (sender-id (getf comment :by))
         (sender-name (db sender-id :name))
         (people (mapcar #'car (getf conversation :people))))
    (dolist (to (remove sender-id people))
      (cl-smtp:send-email
        +mail-server+
        "Kindista <noreply@kindista.org>"
        (car (db to :emails))
        (s+ "New message from " sender-name)
        (comment-notification-email-text conversation-id
                                         sender-name
                                         subject
                                         (name-list (remove to people)
                                                    :func #'person-name
                                                    :minimum-links 5)
                                         text)
        :html-message (comment-notification-email-html
                        conversation-id
                        (person-email-link sender-id)
                        subject
                        (name-list (remove to people)
                                   :func #'person-email-link
                                   :minimum-links 5)
                        text)))))

(defun send-circle-notification-email (from to)
  (cl-smtp:send-email +mail-server+
                      "Kindista <noreply@kindista.org>"
                      to
                      (s+ from " added you to their circles on Kindista!")
                      (circle-notification-email-text from)
                      :html-message (circle-notification-email-html from)))

