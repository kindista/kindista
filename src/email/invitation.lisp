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
         (expires (getf invitation :valid-until))
         (to (getf invitation :recipient-email)))
    (cl-smtp:send-email +mail-server+
                        "Kindista <noreply@kindista.org>"
                        to
                        (s+ (getf (db from) :name) " has invited you to join Kindista!")
                        (invitation-email-text invitation-id
                                               token
                                               to
                                               from
                                               :text text)
                        :html-message (invitation-email-html invitation-id
                                                             token
                                                             to
                                                             from
                                                             :text text)
    )))

(defun invitation-email-text (invitation-id token to from &key text)
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
             "id" invitation-id
             "email" to
             "token" token
             "from" sender)

"

You will then have the option to join Kindista, or decline the "
      " invitation so " sender " can give it to someone else.

"
      "-The Kindista Team")))


(defun invitation-email-html (invitation-id token to from &key text)
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
                                 "id" invitation-id
                                 "email" to
                                 "from" sender
                                 "token" token
                                 )
                    (str (url-compose (s+ +base-url+ "signup")
                                      "id" invitation-id
                                      "email" to
                                      "from" sender
                                      "token" token
                                      ))))


        (:p :style *style-p* 
          "You will then have the option to join Kindista, or decline the "
          " invitation so " (str sender) " can give it to someone else.  ")

        (:p "-The Kindista Team")))))
