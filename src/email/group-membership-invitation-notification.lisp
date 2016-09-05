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

(defun send-group-membership-invitation-notification-email (invitation-id)
  (let* ((invitation (db invitation-id))
         (from (getf invitation :invited-by))
         (host-name (db from :name))
         (group-id (getf invitation :group-id))
         (group-name (db group-id :name))
         (group-url (strcat* *email-url* "groups/" (username-or-id group-id)))
         (recipient-id (caaar (getf invitation :people)))
         (recipient (db recipient-id))
         (email (car (getf recipient :emails)))
         (unsubscribe-key (getf recipient :unsubscribe-key)))

     (when (and (getf recipient :notify-group-membership-invites)
                (getf recipient :active))
       (send-push-through-chrome-api (list recipient-id)
                                     :message-title (s+ "Invitation to join "
                                                        group-name)
                                     :message-body (s+ host-name
                                                  " invited you to join their group")
                                     :message-tag "group-invite-tag"
                                     :message-url (strcat +base-url+
                                                         "groups/"
                                                         (username-or-id group-id))
                                     ;:message-type :group-invite
                                     )
       (cl-smtp:send-email +mail-server+
                           "Kindista <noreply@kindista.org>"
                           email
                           (s+ host-name
                               " has invited you to join their group, "
                               group-name
                               ", on Kindista")
                           (group-membership-invitation-notification-email-text
                             host-name
                             group-url
                             group-name
                             email
                             unsubscribe-key)
                           :html-message (group-membership-invitation-notification-email-html
                                           host-name
                                           group-url
                                           group-name
                                           email
                                           unsubscribe-key)))))

(defun group-membership-invitation-notification-email-text
  (host-name group-url group-name email unsubscribe-key)
  (strcat
    host-name
    " invited you to join their group, "
    group-name
    ", on Kindista."
    #\linefeed #\linefeed
    "You can accept the invitation here:"
    #\linefeed
    group-url
    #\linefeed #\linefeed
    "Thank you for sharing your gifts with us!"
    #\linefeed
    "-The Kindista Team"  
    #\linefeed #\linefeed
    (unsubscribe-notice-ps-text
      unsubscribe-key
      email
      "notifications when people invite you to join groups on Kindista"
      :detailed-notification-description "group invite notifications"
      :unsub-type "group-membership-invites")
    ))


(defun group-membership-invitation-notification-email-html
  (host-name group-url group-name email unsubscribe-key)
  (html-email-base
    (html

      (:p :style *style-p*
          (str host-name)
            " has invited you to join their group, "
            (str group-name)
            ", on Kindista.")

      (str (email-action-button group-url (s+ "Join " group-name)))

      (:p :style *style-p* "Thank you for sharing your gifts with us!")

      (:p "-The Kindista Team")

      (str (unsubscribe-notice-ps-html
             unsubscribe-key
             email
             "notifications when people invite you to join groups on Kindista"
             :detailed-notification-description "group invite notifications"
             :unsub-type "group-membership-invites")))))

