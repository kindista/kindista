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

    (dolist (admin admin-list)
      (cl-smtp:send-email +mail-server+
                          "DoNotReply <noreply@kindista.org>"
                          (car (db admin :emails))
                          (s+ requestor-name
                              " is requesting to join your group, "
                              group-name
                              ", on Kindista")
                          (group-membership-request-notification-email-text
                            requestor-name
                            group-id
                            group-name)
                          :html-message (group-membership-request-notification-email-html
                                          from
                                          group-id)))))

(defun group-membership-request-notification-email-text (requestor-name group-id group-name)
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
" If you no longer wish to receive notifications when people request to join this group, please edit your settings:"
#\linefeed
+base-url+ "settings/communication" (strcat "?grouipid=" group-id)
#\linefeed #\linefeed
"Thank you for sharing your gifts with us!
-The Kindista Team"))


(defun group-membership-request-notification-email-html (from group-id)
  (html-email-base
    (html
      (:p :style *style-p* (:strong (str (no-reply-notice))))

      (:p :style *style-p* 
          (str (person-email-link from))
            " is requesting to join your group, "
            (str (person-email-link group-id))
                ", on Kindista.")

      (:p :style *style-p*
        "You can approve or deny this request here:"
        (:br)
        (:a :href (s+ +base-url+ (username-or-id group-id) "/members")
            (str (s+ +base-url+ (username-or-id group-id) "/members"))))


      (:p :style *style-p*
          "If you no longer wish to receive notifications when people request to join this group, please edit your settings:"
       (:br)
       (:a :href (s+ +base-url+
                     "settings/communication"
                     (strcat "?groupid=" group-id))
           (str (s+ +base-url+
                    "settings/communication"
                    (strcat "?groupid=" group-id)))))

      (:p :style *style-p* "Thank you for sharing your gifts with us!")
      (:p "-The Kindista Team"))))

