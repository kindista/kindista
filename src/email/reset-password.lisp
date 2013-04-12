(in-package :kindista)

(defun reset-password-text (name token email expiration)
  (s+ 
"Dear " name ",
"
" 
This email was sent automatically by Kindista in response to
your request to reset your password. 
"
"
To reset your password and access your account please click on
the following link or cut and paste it into the address bar of your browser:

"
(url-compose (s+ +base-url+ "/reset")
             "token" token 
             "email" email)
"

If you did not request this email, you can safely ignore it.
"
"
Your security code is " (write-to-string token) ".
This code will expire " expiration ". " 

"
Thank you for sharing your gifts with us!
"
      "-The Kindista Team"))


(defun reset-password-html (name token email expiration)
  (html-email-base
    (html
      (:p :style *style-p* 
       "Dear " (str name) ",")
      (:p :style *style-p*
       "This email was sent automatically by Kindista in response to "
       "your request to reset your password.")
      (:p :style *style-p*
       "To reset your password and access your account please click "
       "on the following link or cut and paste it into the address bar of "
       "your browser:")

      (:p :style *style-p*
        (:a :href (url-compose (s+ +base-url+ "/reset")
                               "token" token 
                               "email" email)
                  (str (url-compose (s+ +base-url+ "/reset")
                                    "token" token 
                                    "email" email))))  


      (:p :style *style-p* 
       "If you did not request this email, you can safely ignore it.")
      
      (:p :style *style-p*
       "Your security code is " (:strong (str (write-to-string token)) ".")         
       (:br)
       "This code will expire " (str expiration) ".")

      (:p :style *style-p* 
        "Thank you for sharing your gifts with us! ")

      (:p "-The Kindista Team"))))
