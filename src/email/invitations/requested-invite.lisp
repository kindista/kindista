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

(defun send-requested-invite-email (invitation-id &key auto-reminder)
  (let* ((invitation (db invitation-id))
         (token (getf invitation :token))
         (text (getf invitation :text))
         (name (getf invitation :name))
         (email (getf invitation :recipient-email)))
    (cl-smtp:send-email
       +mail-server+
       "Kindista <noreply@kindista.org>"
      email
      (if auto-reminder
        "Reminder: The Kindista invitation you requested will be expiring soon!"
        "Here's the invitation you requested to join Kindista!"
)                        (requested-invite-email-text token
                                   name
                                   email
                                   :auto-reminder auto-reminder
                                   :text text)
      :html-message (requested-invite-email-html token
                                                 name
                                                 email
                                                 :auto-reminder auto-reminder
                                                 :text text))))

(defun requested-invite-email-text (token name email &key text auto-reminder)
(s+ "Hi " name ",

"
(if auto-reminder
"We are writing to let you know that the Kindista invitation you requested will  be expiring soon."
"Here is the invitation you requested to join Kindista!")
"

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
(url-compose (s+ *email-url* "signup")
             "email" email
             "token" token)

"
Please note: some features on Kindista will not be "
"available until you "
"post your first offer and we have time to review it.
"
"
Thanks for sharing your gifts with us!
"
"-The Kindista Team"))


(defun requested-invite-email-html (token name email &key text auto-reminder)
(html-email-base
  (html
    (:p :style *style-p*
      "Hi " (str name) ","
    (:p :style *style-p*
      (str
        (if auto-reminder
          "We are writing to let you know that the Kindista invitation you requested will  be expiring soon."
          "Here is the invitation you requested to join Kindista!"))))

    (when text
      (htm (:table :cellspacing 0
                   :cellpadding 0
                   :style *style-quote-box*
             (:tr (:td :style "padding: 4px 12px;"
                     "Personal message from the Kindista crew: "
                      (:br)
                      "\"" (str (email-text text)) "\"")))))

    (:p :style *style-p*
      "We are excited for you to join the Kindista community!")

    (:p :style *style-p*
      "Please click on this link or copy and paste it into your browser"
      " to create your account: "
      (:br)
      (:a :href (url-compose (s+ *email-url* "signup")
                             "email" email
                             "token" token)
                (str (url-compose (s+ *email-url* "signup")
                                  "email" email
                                  "token" token))))
    (:p :style *style-p*
      "Please note: some features on Kindista will not be "
      "available until you "
      "post your first offer and we have time to review it." )

    (:p :style *style-p*
      "Thanks for sharing your gifts with us!")

    (:p "-The Kindista Team"))))
