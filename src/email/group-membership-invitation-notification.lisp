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

(defun send-group-membership-invitation-notification (invitation-id)
  (let* ((invitation (db invitation-id))
         (personid (caaar (getf invitation :people)))
         (person (db personid))
         (groupid (getf invitation :group-id))
         (group (db groupid))
         (host-name (db (getf invitation :invited-by) :name)))
    (cl-smtp:send-email +mail-server+
                        "Kindista <noreply@kindista.org>"
                        (car (getf person :emails))
                        (s+ host-name " has invited you to join "
                            (if (eq +kindista-id+ groupid)
                              "Kindista's group account"
                              (s+ "the group " (getf group :name)))
                            " on Kindista")
                        "Forgotten Password"
                        (group-membership-invitation-notification-text
                          person
                          host-name
                          group)
                        :html-message (group-membership-invitation-notification-html
                                        person
                                        host-name
                                        group))))

(defun group-membership-invitation-notification-text (person host-name group)
  (s+
     (no-reply-notice)
     #\linefeed #\linefeed
     "Dear " (getf person :name) ","
     #\linefeed #\linefeed
     host-name
     " has invited you to join "
     (if (eq +kindista-id+ groupid)
       "Kindista's group account"
       (s+ "the group " (getf group :name)))
         " on Kindista"
     #\linefeed #\linefeed
"
To reset your password and access your account please click on
the following link or cut and paste it into the address bar of your browser:

"
(url-compose (s+ +base-url+ "reset")
             "token" token
             "email" email)
"

If you did not request this email, you can safely ignore it.
"
"
Your security code is " (write-to-string token) ".
This code will expire " expiration ". "

"
Thank you for sharing your gifts with us!
"
      "-The Kindista Team"))


(defun reset-password-html (name token email expiration)
  (html-email-base
    (html
      (:p :style *style-p* 
       "Dear " (str name) ",")
      (:p :style *style-p*
       "This email was sent automatically by Kindista in response to "
       "your request to reset your password.")
      (:p :style *style-p*
       "To reset your password and access your account please click "
       "on the following link or cut and paste it into the address bar of "
       "your browser:")

      (:p :style *style-p*
        (:a :href (url-compose (s+ +base-url+ "reset")
                               "token" token
                               "email" email)
                  (str (url-compose (s+ +base-url+ "reset")
                                    "token" token
                                    "email" email))))


      (:p :style *style-p*
       "If you did not request this email, you can safely ignore it.")

      (:p :style *style-p*
       "Your security code is " (:strong (str (write-to-string token)) ".")
       (:br)
       "This code will expire " (str expiration) ".")

      (:p :style *style-p*
        "Thank you for sharing your gifts with us! ")

      (:p "-The Kindista Team"))))
