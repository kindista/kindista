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

(defun send-group-membership-invitation-notification-email (invitation-id)
  (let* ((invitation (db invitation-id))
         (from (getf invitation :invited-by))
         (host-name (db from :name))
         (group-id (getf invitation :group-id))
         (group-name (db group-id :name))
         (recipient (caaar (getf invitation :people))))

     (cl-smtp:send-email +mail-server+
                         "DoNotReply <noreply@kindista.org>"
                         (car (db recipient :emails))
                         (s+ host-name
                             " has invited you to join their group, "
                             group-name
                             ", on Kindista")
                         (group-membership-invitation-notification-email-text
                           host-name
                           group-id
                           group-name)
                         :html-message (group-membership-invitation-notification-email-html
                                         from
                                         group-id
                                         group-name))))

(defun group-membership-invitation-notification-email-text (host-name group-id group-name)
  (strcat
    (no-reply-notice)
    #\linefeed
    host-name
    " invited you to join their group, "
    group-name
    ", on Kindista."
    #\linefeed #\linefeed
    "You can accept the invitation here:"
    #\linefeed
    +base-url+ "groups/" (username-or-id group-id)
    #\linefeed #\linefeed
    " If you no longer wish to receive notifications when people invite you to join groups on Kindista, please edit your settings:"
    #\linefeed
    +base-url+ "settings/communication"
    #\linefeed #\linefeed
    "Thank you for sharing your gifts with us!"
    #\linefeed
    "-The Kindista Team"))


(defun group-membership-invitation-notification-email-html (from group-id group-name)
  (html-email-base
    (html
      (:p :style *style-p* (:strong (str (no-reply-notice))))

      (:p :style *style-p* 
          (str (person-email-link from))
            " has invited you to join their group, "
            (str (person-email-link group-id))
                ", on Kindista.")

      (:p :style *style-p*
        "You can join " (str group-name) " here:"
        (:br)
        (:a :href (s+ +base-url+ "groups/" (username-or-id group-id))
            (str (s+ +base-url+ "groups/" (username-or-id group-id)))))

      (:p :style *style-p* 
          "If you no longer wish to receive notifications when people invite you to join groups on Kindista, please edit your settings:"
       (:br)
       (:a :href (s+ +base-url+
                     "settings/communication")
           (str (s+ +base-url+
                    "settings/communication"))))

      (:p :style *style-p* "Thank you for sharing your gifts with us!")
      (:p "-The Kindista Team"))))

