(in-package :kindista)

(defun send-email-verification (invitation-id)
  (let* ((invitation (db invitation-id))
         (token (getf invitation :token))
         (to (getf invitation :recipient-email)))
    (cl-smtp:send-email +mail-server+
                        "Kindista <noreply@kindista.org>"
                        to
                        "Please verify your email address."
                        (email-verification-text invitation-id
                                                 to
                                                 token)
                        :html-message (email-verification-html invitation-id
                                                               to
                                                               token))))

(defun email-verification-text (id to token)
  (strcat*
"To validate this email address, click on this link or
 copy and paste it into your browser: "
#\linefeed
(url-compose (s+ +base-url+ "settings/communication")
             "token" token
             "invitation-id" id
             "email" to)
#\linefeed #\linefeed
"If you did not request this email, you can safely ignore it."
#\linefeed #\linefeed
"If you are unable to click on the above link, go to your
communications settings, click on the \"activate\" link
next to this email address (" to
") and enter your activation code."
#\linefeed #\linefeed
"Your activation code is " (write-to-string token) "."
#\linefeed
"This activation code will expire in 30 days."
#\linefeed #\linefeed
"Thank you for sharing your gifts with us!"
#\linefeed #\linefeed
"-The Kindista Team"))


(defun email-verification-html (id to token)
  (html-email-base
    (html
      (:p :style *style-p*
       "To validate this email address, click on this link or copy and paste "
       "it into your browser:")
      (:p :style *style-p*
        (:a :href (url-compose (s+ +base-url+ "settings/communication")
                               "invitation-id" id
                               "token" token
                               "email" to)
                  (str (url-compose (s+ +base-url+ "settings/communication")
                                    "invitation-id" id
                                    "token" token
                                    "email" to))))


      (:p :style *style-p*
       "If you did not request this email, you can safely ignore it.")

      (:p :style *style-p*
       "If you are unable to click on the above link, go to your "
       (:a :href (s+ +base-url+ "settings/communication")
           "communications settings")
       ", click on the \"activate\" link next to this email address("
       (str to)
       ") and enter your activation code.")

      (:p :style *style-p*
       "Your activation code is " (:strong (str (write-to-string token)) ".")
       (:br)
       "This activation code will expire in 30 days.")

      (:p :style *style-p*
        "Thank you for sharing your gifts with us! ")

      (:p "-The Kindista Team"))))
