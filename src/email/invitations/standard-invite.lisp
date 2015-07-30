;;; Copyright 2012-2015 CommonGoods Network, Inc.
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
         (invitee-name (getf invitation :name))
         (text (getf invitation :text))
         ;; the most recent group invite sent by the host
         (group-id (car (getf invitation :groups)))
         (group (db group-id))
         ;; the most recent statement of gratitude from the host
         (gratitude-id (car (getf invitation :gratitudes)))
         (gratitude (db gratitude-id))
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
              " is expiring soon."))
        (host-reminder
          (s+ host-name
              " is reminding you to join them on Kindista."))
        (gratitude
          (s+ host-name " has shared gratitude for you on Kindista."))
        (group
          (s+ host-name
              " wants you to join the "
              (getf group :name)
              " sharing network on Kindista!"))
        (t (s+ host-name " wants you to join their sharing network!")))
      (invitation-email-text token
                             to
                             host
                             :invitee-name invitee-name
                             :text text
                             :host-reminder host-reminder
                             :group-name (getf group :name)
                             :gratitude gratitude
                             :auto-reminder auto-reminder)
      :html-message (invitation-email-html token
                                           to
                                           host
                                           :invitee-name invitee-name
                                           :text text
                                           :host-reminder host-reminder
                                           :group-id group-id
                                           :gratitude gratitude
                                           :auto-reminder auto-reminder))))

(defun invitation-email-text (token to from &key invitee-name group-name gratitude text auto-reminder host-reminder)
  (let ((sender (getf (db from) :name)))
    (strcat*
(no-reply-notice)
#\linefeed #\linefeed
(when invitee-name
  (s+ "Hi " invitee-name ","))
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
(acond
  ((getf gratitude :text)
    (strcat* sender
             " posted a statement of gratitude about you on Kindista: \""
             #\linefeed
             it
             "\""
             #\linefeed #\linefeed
             " On Kindista, gratitude lets people know how you are contributing to your community.  When you sign up, this statement of gratitude will appear on your profile. It is a form of currency (like money) that encourages others in the network to share resources with you. "
             #\linefeed #\linefeed))
  (text
    (strcat* sender
             " says: \"" #\linefeed it "\""
             #\linefeed #\linefeed)))

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
(url-compose (s+ *email-url* "signup") "email" to "token" token)
#\linefeed #\linefeed
"-The Kindista Team"
#\linefeed #\linefeed
"Already on Kindista?"
"Please click on this link or copy and paste it into your browser"
" to add this email address (" to
") to your Kindista account:"
(s+ *email-url* "settings/communication?edit=email#email"))))


(defun invitation-email-html (token to from &key invitee-name group-id text gratitude auto-reminder host-reminder)
  (let ((sender (getf (db from) :name)))
    (html-email-base
      (html
        (:p :style *style-p* (str (no-reply-notice)))

        (when invitee-name
          (htm (:p :style *style-p*
                "Hi " (str invitee-name) "," )) )

        (when auto-reminder
          (htm
            (:p :style *style-p*
                "We are writing to let you know that your Kindista invitation will be expiring "
                "soon.")))

        (:p :style *style-p*
          (str (person-email-link from))
          " has invited you to join "

          (awhen group-id
            (htm "the " (str (person-email-link it)) " group on "))
          "Kindista, the social network for building "
          "community and sharing skills, tools and other local resources. ")

        (acond
          ((getf gratitude :text)
           (htm (:table :cellspacing 0
                        :cellpadding 0
                        :style *style-quote-box*
                  (:tr (:td :style "padding: 4px 12px;"

                        (str sender) " shared a statement of gratitude about you:"
                        (:br)
                        (:strong "\"" (str (html-text it)) "\""))))
                (:p :style *style-p*
                  "On Kindista, gratitude lets people know how you are contributing to your community.  When you sign up, this statement of gratitude will appear on your profile. It is a form of currency (like money) that encourages others in the network to share resources with you. ")))
          (text
            (htm (:table :cellspacing 0
                         :cellpadding 0
                         :style *style-quote-box*
                   (:tr (:td :style "padding: 4px 12px;"

                         (str sender) " says:"
                         (:br)
                         "\"" (str (html-text it)) "\""))))))

        (:p :style *style-p*
          (str sender)
          (str (if host-reminder
                 " is reminding you to join"
                 " sent you an invitation"))
          " because they think you would make a valuable addition to the"
          " Kindista community.  ")

        (:p :style *style-p*
         (:strong "Please click on this link or copy and paste it into your browser to RSVP: ")
         (:br)
         (:strong (:a :href (url-compose (s+ *email-url* "signup")
                                 "email" to
                                 "token" token)
                    (str (url-compose (s+ *email-url* "signup")
                                      "email" to
                                      "token" token)))))

        (:p "-The Kindista Team")

        (:p :style *style-p*
          "Already on Kindista?"
          "Please click on this link or copy and paste it into your browser"
          " to add this email address (" (str to)
          ") to your Kindista account:"
          (:a :href (s+ *email-url* "settings/communication?edit=email#email")
              (str (s+ *email-url* "settings/communication?edit=email#email"))))))))

