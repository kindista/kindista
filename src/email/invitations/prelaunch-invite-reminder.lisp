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

(defun send-prelaunch-invite-reminder (invitation-id)
  (let* ((invitation (db invitation-id))
         (token (getf invitation :token))
         (text (getf invitation :text))
         (email (getf invitation :recipient-email)))
    (cl-smtp:send-email +mail-server+
                        "Kindista <noreply@kindista.org>"
                        email
                        "Final Reminder: your invitation to join Kindista is expiring soon"
                        (prelaunch-invite-reminder-email-text token
                                                              email
                                                              :text text)
                        :html-message (prelaunch-invite-reminder-email-html token
                                                                            email
                                                                            :text text))))

(defun prelaunch-invite-reminder-email-text (token email &key text)
(s+ "Greetings,
"
"
At some point last year, likely at a talk given by Charles Eisenstein, you signed up to be invited to join Kindista to get more of your needs met through the gift economy.  We sent you an invitation a couple of months ago, but in case you missed it we want to let you know that your invitation will be expiring soon.
"

"
You may be one of the first people to sign up in your local area. If that is the case, please start by inviting your friends to join and please feel free to contact us for suggestions about how to jump start a new sharing network in your local community."

"

Your invitation code is " (write-to-string token) ".

"

(when text
(s+ "Personal message from the Kindista crew: 
\""
text "\"

"))

"We are excited for you to join the Kindista community!

"

"Please click on this link or copy and paste it into your browser"
" to create your account:
"
(url-compose (s+ +base-url+ "signup")
             "email" email
             "token" token)

"
Thanks for sharing your gifts with us!
"
"-The Kindista Team"))


(defun prelaunch-invite-reminder-email-html (token email &key text)
(html-email-base
  (html
    (:p :style *style-p*
     "Greetings,")

    (:p :style *style-p*
"At some point last year, likely at a talk given by Charles Eisenstein, you signed up to be invited to join Kindista to get more of your needs met through the gift economy.  We sent you an invitation a couple of months ago, but in case you missed it we want to let you know that your invitation will be expiring soon.")


    (:p :style *style-p*
     " You may be one of the first people to sign up in your local area. If that is the case, please start by inviting your friends to join and please feel free to contact us for suggestions about how to jump start a new sharing network in your local community. ")

    (:p :style *style-p*
     "Your invitation code is " (:strong (str (write-to-string token)) "."))

    (when text
      (htm (:table :cellspacing 0
                   :cellpadding 0
                   :style *style-quote-box*
             (:tr (:td :style "padding: 4px 12px;"
                     "Personal message from the Kindista crew: "
                      (:br)
                      "\"" (str text) "\"")))))

    (:p :style *style-p*
      "We are excited for you to join the Kindista community!")

    (:p :style *style-p*
      "Please click on this link or copy and paste it into your browser"
      " to create your account: "
      (:br)
      (:a :href (url-compose (s+ +base-url+ "signup")
                             "email" email
                             "token" token)
                (str (url-compose (s+ +base-url+ "signup")
                                  "email" email
                                  "token" token))))

    (:p :style *style-p*
      "Thanks for sharing your gifts with us!")

    (:p "-The Kindista Team"))))
