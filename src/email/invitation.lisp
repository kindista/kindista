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

(defun send-invitation-email (invitation-id)
  (let* ((invitation (db invitation-id))
         (token (getf invitation :token))
         (from (getf invitation :host))
         (text (getf invitation :text))
         (to (getf invitation :recipient-email)))
    (cl-smtp:send-email +mail-server+
                        "Kindista <noreply@kindista.org>"
                        to
                        (s+ (getf (db from) :name) " has invited you to join Kindista!")
                        (invitation-email-text token
                                               to
                                               from
                                               :text text)
                        :html-message (invitation-email-html token
                                                             to
                                                             from
                                                             :text text))))

(defun invitation-email-text (token to from &key text)
  (let ((sender (getf (db from) :name)))
    (s+ sender
" has invited you to join Kindista, the social network for building "
"community and sharing local resources.

Your invitation code is " (write-to-string token) ".
This invitation will expire in 30 days.

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


(defun invitation-email-html (token to from &key text)
  (let ((sender (getf (db from) :name)))
    (html-email-base
      (html
        (:p :style *style-p*
          (str (person-email-link from))
          " has invited you to join Kindista, the social network for building "
          "community and sharing local resources. ")

        (:p :style *style-p*
         "Your invitation code is " (:strong (str (write-to-string token)) ".")
         (:br)
         "This invitation will expire in 30 days.")

        (when text
          (htm (:table :cellspacing 0
                       :cellpadding 0
                       :style *style-quote-box*
                 (:tr (:td :style "padding: 4px 12px;"

                        (str sender) " says:"
                          (:br)
                          "\"" (str text) "\"")))))

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

(defun send-requested-invite-email (invitation-id)
  (let* ((invitation (db invitation-id))
         (token (getf invitation :token))
         (text (getf invitation :text))
         (name (getf invitation :name))
         (email (getf invitation :recipient-email)))
    (cl-smtp:send-email +mail-server+
                        "Kindista <noreply@kindista.org>"
                        email
                        "Here's the invitation you requested to join Kindista!"
                        (requested-invite-email-text token
                                                     name
                                                     email
                                                     :text text)
                        :html-message (requested-invite-email-html token
                                                                   name
                                                                   email
                                                                   :text text))))

(defun requested-invite-email-text (token name email &key text)
(s+ "Greetings,
"
"
At some point last year, likely at a talk given by Charles Eisenstein, you signed up to be notified when Kindista.org was available in your area.  Although it has taken longer than we had hoped, we are excited to invite you to join our gift-economy network!
"

"
You may be one of the first people to sign up in your local area. If that is the case, please start by inviting your friends to join and please feel free to contact us for suggestions about how to jump start a new sharing network in your local community."

"

Your invitation code is " (write-to-string token) ".
This invitation will expire in 60 days.

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


(defun requested-invite-email-html (token name email &key text)
(html-email-base
  (html
    (:p :style *style-p*
     "Greetings,")
    
    (:p :style *style-p*
"At some point last year, likely at a talk given by Charles Eisenstein, you signed up to be notified when Kindista.org was available in your area.  Although it has taken longer than we had hoped, we are excited to invite you to join our gift-economy network!")

    (:p :style *style-p*
     " You may be one of the first people to sign up in your local area. If that is the case, please start by inviting your friends to join and please feel free to contact us for suggestions about how to jump start a new sharing network in your local community. ")
     
    (:p :style *style-p*
     "Your invitation code is " (:strong (str (write-to-string token)) ".")
     (:br)
     "This invitation will expire in 60 days.")

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
#|
(defun requested-invite-email-text (token name email &key text)
(s+ "Hi " name ",
"
"
Here is the invitation you requested to join Kindista!"
"

Your invitation code is " (write-to-string token) ".
This invitation will expire in 60 days.

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


(defun requested-invite-email-html (token name email &key text)
(html-email-base
  (html
    (:p :style *style-p*
      "Hi " (str name) ","
      "Here is the invitation you requested to join Kindista!")

    (:p :style *style-p*
     "Your invitation code is " (:strong (str (write-to-string token)) ".")
     (:br)
     "This invitation will expire in 60 days.")

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
|#
