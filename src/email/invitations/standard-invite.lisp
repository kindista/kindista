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

(defun send-invitation-email (invitation-id &key auto-reminder)
  (let* ((invitation (db invitation-id))
         (token (getf invitation :token))
         (host (getf invitation :host))
         (host-name (db host :name))
         (text (getf invitation :text))
         ;; the most recent group invite sent by the host
         (group-id (car (getf invitation :groups)))
         (group (db group-id))
         (host-reminder (and (not auto-reminder)
                             (> (length (getf invitation :times-sent)) 1)))
         (to (getf invitation :recipient-email)))
    (cl-smtp:send-email
      +mail-server+
      "Kindista <noreply@kindista.org>"
      to
      (cond
        (auto-reminder
          (s+ "Reminder: your Kindista invitation from "
              host-name
              " is expiring soon"))
        (host-reminder
          (s+ host-name
              " is reminding you to join them on Kindista."))
        (group
          (s+ host-name
              " wants you to join the "
              (getf group :name)
              " sharing network on Kindista!"))
        (t (s+ host-name " wants you to join their sharing network!")))
      (invitation-email-text token
                             to
                             host
                             :text text
                             :host-reminder host-reminder
                             :group-name (getf group :name)
                             :auto-reminder auto-reminder)
      :html-message (invitation-email-html token
                                           to
                                           host
                                           :group-id group-id
                                           :text text
                                           :host-reminder host-reminder
                                           :auto-reminder auto-reminder))))

(defun invitation-email-text (token to from &key group-name text auto-reminder host-reminder)
  (let ((sender (getf (db from) :name)))
    (strcat*
(no-reply-notice)
#\linefeed #\linefeed
(when auto-reminder
"We are writing to let you know that your Kindista invitation will be expiring soon.")
#\linefeed #\linefeed
sender
" has invited you to join "
(when group-name (s+ "the " group-name " group on "))
"Kindista; the social network for building "
"community and sharing skills, tools, and other local resources."
#\linefeed #\linefeed
"Your invitation code is " (write-to-string token) ". "
#\linefeed #\linefeed
(when text
  (strcat* sender " says: \"" #\linefeed text "\""))
#\linefeed #\linefeed
sender
(if host-reminder
  " is reminding you to join"
  " sent you an invitation")
" because they think you would make a valuable addition to the"
" Kindista community."
#\linefeed #\linefeed
"Please click on this link or copy and paste it into your browser"
" to RSVP:"
#\linefeed
(url-compose (s+ +base-url+ "signup") "email" to "token" token)
#\linefeed #\linefeed
"-The Kindista Team"
#\linefeed #\linefeed
"Already on Kindista?"
"Please click on this link or copy and paste it into your browser"
" to add this email address (" to
") to your Kindista account:"
(s+ +base-url+ "settings/communication?edit=email#email"))))


(defun invitation-email-html (token to from &key group-id text auto-reminder host-reminder)
  (let ((sender (getf (db from) :name)))
    (html-email-base
      (html
        (:p :style *style-p* (str (no-reply-notice)))

        (when auto-reminder
          (htm
            (:p :style *style-p*
                "We are writing to let you know that your Kindista invitation will be expiring "
                "soon.")))

        (:p :style *style-p*
          (str (person-email-link from))
          " has invited you to join "

          (awhen group-id
            (htm "the " (str (group-link it)) " group on "))
          "Kindista, the social network for building "
          "community and sharing skills, tools and other local resources. ")

        (:p :style *style-p*
         "Your invitation code is " (:strong (str (write-to-string token)) "."))

        (when text
          (htm (:table :cellspacing 0
                       :cellpadding 0
                       :style *style-quote-box*
                 (:tr (:td :style "padding: 4px 12px;"

                        (str sender) " says:"
                          (:br)
                          "\"" (str (html-text text)) "\"")))))

        (:p :style *style-p*
          (str sender)
          (str (if host-reminder
                 " is reminding you to join"
                 " sent you an invitation"))
          " because they think you would make a valuable addition to the"
          " Kindista community.  ")

        (:p :style *style-p*
          "Please click on this link or copy and paste it into your browser"
          " to RSVP: "
          (:br)
          (:a :href (url-compose (s+ +base-url+ "signup")
                                 "email" to
                                 "token" token)
                    (str (url-compose (s+ +base-url+ "signup")
                                      "email" to
                                      "token" token))))

        (:p "-The Kindista Team")

        (:p :style *style-p*
          "Already on Kindista?"
          "Please click on this link or copy and paste it into your browser"
          " to add this email address (" to
          ") to your Kindista account:"
          (:a :href (s+ +base-url+ "settings/communication?edit=email#email")
              (str (s+ +base-url+ "settings/communication?edit=email#email"))))))))

