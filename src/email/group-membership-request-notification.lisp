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

(defun send-group-membership-request-notification-email (request-id)
  (let* ((request (db request-id))
         (from (getf request :requested-by))
         (requestor-name (db from :name))
         (group-id (getf request :group-id))
         (group (db group-id))
         (group-name (getf group :name))
         (admin-list (getf group :notify-membership-request)))

    (dolist (admin-id admin-list)
      (let* ((admin (db admin-id))
             (email (car (getf admin :emails)))
             (unsubscribe-key (getf admin :unsubscribe-key)))
        (cl-smtp:send-email +mail-server+
                          "DoNotReply <noreply@kindista.org>"
                          email
                          (s+ requestor-name
                              " is requesting to join your group, "
                              group-name
                              ", on Kindista")
                          (group-membership-request-notification-email-text
                            requestor-name
                            group-id
                            group-name
                            email
                            unsubscribe-key)
                          :html-message (group-membership-request-notification-email-html
                                          requestor-name
                                          group-id
                                          group-name
                                          email
                                          unsubscribe-key))))))

(defun group-membership-request-notification-email-text
  (requestor-name group-id group-name email unsubscribe-key)
  (strcat
(no-reply-notice)
#\linefeed #\linefeed
requestor-name
" is requesting to join your group, "
group-name
", on Kindista."
#\linefeed #\linefeed
#\linefeed #\linefeed
"You can accept the invitation here:"
#\linefeed
+base-url+ "groups/" (username-or-id group-id) "/members"
#\linefeed #\linefeed
(unsubscribe-notice-ps-text
  unsubscribe-key
  email
  (s+ "notifications when people request to join " group-name)
  :groupid group-id)
#\linefeed #\linefeed
"Thank you for sharing your gifts with us!
-The Kindista Team"))


(defun group-membership-request-notification-email-html
  (requestor-name group-id group-name email unsubscribe-key)
  (html-email-base
    (html
      (:p :style *style-p* (:strong (str (no-reply-notice))))

      (:p :style *style-p*
          (str requestor-name)
          " is requesting to join your group, "
          (str group-name)
          ", on Kindista.")

      (:p :style *style-p*
        "You can approve or deny this request here:"
        (:br)
        (:a :href (s+ +base-url+ (username-or-id group-id) "/members")
            (str (s+ +base-url+ (username-or-id group-id) "/members"))))

      (str
        (unsubscribe-notice-ps-text
          unsubscribe-key
          email
          (s+ "notifications when people request to join " group-name)
          :groupid group-id))

      (:p :style *style-p* "Thank you for sharing your gifts with us!")
      (:p "-The Kindista Team"))))

