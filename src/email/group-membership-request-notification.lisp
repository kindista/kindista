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

(defun send-group-membership-request-notification-email (request-id)
  (let* ((request (db request-id))
         (from (getf request :requested-by))
         (requestor-name (db from :name))
         (group-id (getf request :group-id))
         (reply-url (strcat *email-url*
                            "groups/"
                            (username-or-id group-id)
                            "/members"))
         (group (db group-id))
         (push-recipients)
         (group-name (getf group :name))
         (admin-list (getf group :notify-membership-request)))

    (dolist (admin-id admin-list)
      (let* ((admin (db admin-id))
             (email (car (getf admin :emails)))
             (unsubscribe-key (getf admin :unsubscribe-key)))
        (push (list :id admin-id) push-recipients)
        (cl-smtp:send-email +mail-server+
                          "Kindista <noreply@kindista.org>"
                          email
                          (s+ requestor-name
                              " is requesting to join your group, "
                              group-name
                              ", on Kindista")
                          (group-membership-request-notification-email-text
                            requestor-name
                            reply-url
                            group-id
                            group-name
                            email
                            unsubscribe-key)
                          :html-message (group-membership-request-notification-email-html
                                          requestor-name
                                          reply-url
                                          group-id
                                          group-name
                                          email
                                          unsubscribe-key))))
    (send-push-through-chrome-api push-recipients
                                  :message-title (s+ "Request to join" group-name)
                                  :message-body (s+ requestor-name
                                         " is requesting to join your group, "
                                         group-name)
                                  :message-tag "group-request-tag"
                                  :message-url reply-url
                                  :message-type :group-request
                                  )))

(defun group-membership-request-notification-email-text
  (requestor-name reply-url group-id group-name email unsubscribe-key)
  (strcat
requestor-name
" is requesting to join your group, "
group-name
", on Kindista."
#\linefeed #\linefeed
#\linefeed #\linefeed
"You can accept the invitation here:"
#\linefeed
reply-url
#\linefeed #\linefeed
"Thank you for sharing your gifts with us!
-The Kindista Team"
#\linefeed #\linefeed
(unsubscribe-notice-ps-text
  unsubscribe-key
  email
  (s+ "notifications when people request to join " group-name)
  :groupid group-id)))


(defun group-membership-request-notification-email-html
  (requestor-name reply-url group-id group-name email unsubscribe-key)
  (html-email-base
    (html
      (:p :style *style-p*
          (str requestor-name)
          " is requesting to join your group, "
          (str group-name)
          ", on Kindista.")

      (:p :style *style-p*
        "You can approve or deny this request here:"
        (:br)
        (:a :href reply-url (str reply-url)))

      (:p :style *style-p* "Thank you for sharing your gifts with us!")

      (:p "-The Kindista Team")

      (str
        (unsubscribe-notice-ps-html
          unsubscribe-key
          email
          (s+ "notifications when people request to join " group-name)
          :groupid group-id)))))

