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

(defun updated-notifications-handler ()
  (send-updated-notifications-confirmation-email (getf (cddddr *notice*)
                                                       :userid)
                                                 (getf (cddddr *notice*)
                                                       :groupid)))

(defun send-updated-notifications-confirmation-email (userid &optional groupid)
  (let* ((user (db userid))
         (group (db groupid))
         (name (getf (or group user) :name))
         (preferences (user-communication-preferences userid
                                                      :user user
                                                      :groupid groupid)))

     (cl-smtp:send-email +mail-server+
                         "Kindista <info@kindista.org>"
                         (car (getf user :emails))
                         "You have successfully updated your Kindista Communication Preferences"
                         (unsubscribe-confirmation-email-text (getf user :name)
                                                              preferences
                                                              (when group name))
                         :html-message (unsubscribe-confirmation-email-html
                                         (getf user :name)
                                         preferences
                                         (when group name)))))

(defun user-communication-preferences
  (userid
   &key (user (db userid))
        (groupid)
        (group (db groupid))
        (entity (or (getf group :name) "you"))
   &aux (preferences))

  (flet ((record-option (option text)
           (if (getf (or group user) option)
             (asetf (getf preferences :subscribed)
                    (push text it))
             (asetf (getf preferences :unsubscribed)
                    (push text it)))))
    (record-option :notify-gratitude
                   (s+ "when someone posts gratitude about " entity))
    (record-option :notify-message
                   (s+ "when someone sends "
                       entity
                       " a message or responds to "
                       (if group "its" "your")
                       " offers/requests"))
    (unless group
      (record-option :notify-expired-invites
        "when invitatations you send for your friends to join Kindista expire" )

      (record-option :notify-new-contact
        "when someone sends you to their list of contacts")

      (record-option :notify-group-membership-invites
        "when someone invites you to join a group on Kindista (e.g. a business, non-profit, or other organization I belong to within my community)")

      (record-option :notify-reminders
        "occasional suggestions about how you can get the most out of Kindista")

      (record-option :notify-kindista
        "updates and information about Kindista"))

    (when group
      (record-option :notify-membership-request
                     (s+ "when someone wants to join "
                         entity 
                         " on Kindista"))))

  preferences)

(defun unsubscribe-confirmation-email-text (name preferences &optional groupname)
  (strcat*
    "Hi " name ","
    #\linefeed #\linefeed
    "You have successfully updated your Kindista communication preferences"
    (when groupname (strcat " for " groupname))
    "."
    #\linefeed #\linefeed
    (when (getf preferences :subscribed)
      (strcat*
        "You are currently subscribed to receive the following notifications from Kindista:"
        (apply #'strcat*
               (loop for item in (getf preferences :subscribed)
                     collect (strcat #\linefeed "- " item)))
        #\linefeed #\linefeed))
    (awhen (getf preferences :unsubscribed)
      (strcat*
        "You have opted out of receiving the following notifications from Kindista:"
        (apply #'strcat*
               (loop for item in (getf preferences :unsubscribed)
                     collect (strcat #\linefeed "- " item)))
        #\linefeed #\linefeed))
    "Please contact us immediately if you did not recently update your communication preferences "
    "on Kindista and you believe this email was sent in error."
    #\linefeed #\linefeed
    "Thank you for sharing your gifts with us!"
    #\linefeed
    "-The Kindista Team"))


(defun unsubscribe-confirmation-email-html (name preferences &optional groupname)
  (html-email-base
    (html
      (:p :style *style-p*
       "Hi " (str name) ",")

      (:p :style *style-p*
       "You have sucessfully updated your Kindista communication preferences"
       (when groupname
         (htm " for " (:strong (str groupname))))
       ".")

      (awhen (getf preferences :subscribed)
        (htm
          (:p :style *style-p*
           "You are currently "
           (:strong "subscribed")
           " to receive the following notifications from Kindista:")
          (:ul
            (dolist (item (getf preferences :subscribed))
              (htm (:li (str item)))))))

      (awhen (getf preferences :unsubscribed)
        (htm
          (:p :style *style-p*
           "You have "
           (:strong "opted out")
           " and "
           (:strong "will not")
           " be receiving the following notifications from Kindista:")
          (:ul
            (dolist (item (getf preferences :unsubscribed))
              (htm (:li (str item)))))))

      (:p :style *style-p*
         "Please contact us immediately if you did not recently update your communication preferences "
         "on Kindista and you believe this email was sent in error.")

      (:p :style *style-p* "Thank you for sharing your gifts with us!")
      (:p "-The Kindista Team"))))

