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

(defun send-invitation-email (invitation-id &key reminder-type)
  (let* ((invitation (db invitation-id))
         (token (getf invitation :token))
         (from (getf invitation :host))
         (text (getf invitation :text))
         (reminder-type (or reminder-type
                            (when (> (length (getf invitation :times-sent)) 1) :host)))
         (to (getf invitation :recipient-email)))
    (cl-smtp:send-email +mail-server+
                        "Kindista <noreply@kindista.org>"
                        to
                        (case reminder-type
                          (:auto (s+ "Reminder: your Kindista invitation from "
                                     (getf (db from) :name)
                                     " is expiring soon"))
                          (:host (s+ (getf (db from) :name)
                                " is reminding you to join their sharing network on Kindista."))
                          (t (s+ (getf (db from) :name)
                                " wants you to join their sharing network!")))
                        (invitation-email-text token
                                               to
                                               from
                                               :text text
                                               :reminder-type reminder-type)
                        :html-message (invitation-email-html token
                                                             to
                                                             from
                                                             :text text
                                                             :reminder-type reminder-type))))

(defun invitation-email-text (token to from &key text reminder-type)
  (let ((sender (getf (db from) :name)))
    (s+ 
(no-reply-notice)

(when (eql reminder-type :auto)
"We are writing to let you know that your Kindista invitation will be expiring soon.")
"

"
sender
" has invited you to join Kindista, the social network for building "
"community and sharing skills, tools, and other local resources.

Your invitation code is " (write-to-string token) ".

"

(when text
(s+ sender " says:
\""
text "\"

"))

sender
" sent you an invitation"
" because they think you would make a valuable addition to the"
" Kindista community.

"

"Please click on this link or copy and paste it into your browser"
" to RSVP:
"
(url-compose (s+ +base-url+ "signup")
             "email" to
             "token" token)

"

"
"-The Kindista Team")))


(defun invitation-email-html (token to from &key text reminder-type)
  (let ((sender (getf (db from) :name)))
    (html-email-base
      (html
        (:p :style *style-p* (str (no-reply-notice)))

        (when (eql reminder-type :auto)
          (htm
            (:p :style *style-p*
                "We are writing to let you know that your Kindista invitation will be expiring "
                "soon.")))

        (:p :style *style-p*
          (str (person-email-link from))
          " has invited you to join Kindista, the social network for building "
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
          " sent you an invitation"
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

        (:p "-The Kindista Team")))))

