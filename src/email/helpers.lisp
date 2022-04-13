;;; Copyright 2012-2021 CommonGoods Network, Inc.
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

(defparameter *style-a* "color:#5c8a2f;
                         font-weight:bold;
                         text-decoration:none;")

(defparameter *style-p* "max-width:70em;
                         margin-top:.9em;
                         margin-bottom:.9em;")

(defparameter *style-button*
  "font-size: 1.2em;
   background: #3c6dc8 !important;
   border: 1px solid #474747;
   padding: 0.5em 0.7em;
   vertical-align: middle;
   color: #ffffff !important;
   border-radius: 0.25em;
   text-decoration: none;"
  ;background: -moz-linear-gradient( top, #3c6dc8 0%, #29519c);
  ;background: -ms-linear-gradient( top, #3c6dc8 0%, #29519c);
  ;background: -o-linear-gradient( top, #3c6dc8 0%, #29519c);
  ;background: -webkit-linear-gradient( top, #3c6dc8 0%, #29519c);
  ;background: -webkit-gradient( linear, left top, left bottom, from(#3c6dc8), to(#29519c));
  )

(defparameter *small-style-button*
  "font-size: 1em;
   background: #3c6dc8 !important;
   border: 1px solid #474747;
   padding: 0.3em 0.4em;
   vertical-align: middle;
   color: #ffffff !important;
   border-radius: 0.25em;
   text-decoration: none;"
  )

(defparameter *style-button-link*
  "cursor: pointer;
   color: #ffffff !important;
   text-decoration: none;
   display: block; ")

(defparameter *style-quote-box* "border-collapse: collapse;
                                 background: #ebf2e4;
                                 margin: 8px 8px 8px 0;
                                 border: thin solid #bac2b2;")

(defparameter *email-url* (or (awhen *test-email-ip*
                                (s+ it "/"))
                              +base-url+))

(defun person-email-link (id)
  (awhen (db id)
    (html
      (:a :href (strcat *email-url*
                        (if (eql (getf it :type) :person)
                          "people/"
                          "groups/")
                        (username-or-id id))
          (str (getf it :name))))))

(defun email-text (string)
  (if string
    (regex-replace-all "\\n" string "<br>")
    ""))

(defparameter *notification-types*
  '(:notify-blog
    :notify-expired-invites
    :notify-gratitude
    :notify-group-membership-invites
    :notify-inventory-expiration
    :notify-inventory-digest
    :notify-kindista
    :notify-membership-request
    :notify-message
    :notify-new-contact
    :notify-reminders))

(defun unsubscribe-emails-from-all-messages
  (email-addresses
   &optional (unsubscribe-reason "User has complained about Kindista spam.")
   &aux (now (get-universal-time)))
  (dolist (email email-addresses)
    (when (and (validate-email email) (gethash email *email-index*))
      (let* ((user-id (gethash email *email-index*)))
        (dolist (notification-type *notification-types*)
          (modify-db user-id notification-type nil))
        (modify-db user-id
                   :email-suppression-notes
                   (strcat unsubscribe-reason " Suppressing all further emails to this user as of " (universal-to-datestring now) "."))))))

(defun person-name (id)
  (db id :name))

(defun email-action-button (url message &key image (style *style-button*))
  (html
    (:table :cellspacing "0" :cellpadding "0"
     (:td :class "button" :style style
       (:a :href url
           :style *style-button-link*
        (:span :style "color:#ffffff!important;
                       font-weight: bold;
                       cursor: pointer;
                       text-shadow: 1px 1px 2px rgba(0,0,0,0.4); "
         (awhen image (str it))
         (str message)))))))

(defun no-reply-notice
  (&optional (instructions "do so from their profile on Kindista.org"))
  (s+ "PLEASE DO NOT REPLY TO THIS EMAIL, IT WILL NOT BE DELIVERED TO THE SENDER. If you want to contact the sender, please " instructions ". "))

(defun amazon-smile-reminder (&optional html)
  (if html
    (html
      (:div :class *style-p*
         "------------------------------------"
         (:br)
         "Do you shop at Amazon.com? If so, please "
         (:a :href *amazon-smile-link*
             :style *style-a*
          "click here")
         " and Amazon will donate a portion of your purchases to Kindista through our parent organization, CommonGoods Network."))
    (strcat
      #\linefeed #\linefeed
      "------------------------------------"
      #\linefeed
      "Do you shop at Amazon.com? If so, please click here and Amazon will donate a portion of your purchases to Kindista through our parent organization, CommonGoods Network:"
      #\linefeed
      *amazon-smile-link*)))

(defun email-blockquote (text)
 (html (:table :cellspacing 0
               :cellpadding 0
               :style *style-quote-box*
         (:tr (:td :style "padding: 4px 12px;"
                  "\"" (str (email-text text)) "\"")))
       (:br)))

(defun unsubscribe-notice-ps-text
  (unsubscribe-code
   email-address
   notification-description
   &key detailed-notification-description
        groupid
        unsub-type)

(strcat*
#\linefeed #\linefeed
"------------------------------------"
#\linefeed
"Why am I receiving this? "
"In your Kindista communications settings, you are subscribed to receive "
notification-description
". "
"If you no longer wish to receive "
(or detailed-notification-description notification-description)
", you may unsubscribe: "
#\linefeed
(unsubscribe-url email-address unsubscribe-code groupid unsub-type)))

(defun unsubscribe-notice-ps-html
  (unsubscribe-code
   email-address
   notification-description
   &key detailed-notification-description
        groupid
        unsub-type)
(html
  (:p :style (s+ *style-p* " font-size: 0.85em;")
    "Why am I receiving this? "
    "In your Kindista communications settings, you are subscribed to receive "
    (str notification-description)
    ". "
    "If you no longer wish to receive "
    (str (or detailed-notification-description notification-description))
    ", you may "
    (:a :href (unsubscribe-url email-address unsubscribe-code groupid unsub-type)
        :style *style-a*
        "unsubscribe")
    ".")))

(defun unsubscribe-url (email-address unsubscribe-code &optional groupid unsub-type)
  (url-compose (strcat *email-url* "settings/communication")
               "groupid" groupid
               "email" email-address
               "k" unsubscribe-code
               "type" unsub-type))

(defun html-email-base (content)
  (html
    (:html
      (:head
        (:style :type "text/css"
                      "a:hover {text-decoration:underline;}
                       a {color: #5C8A2F !important;}
                       td.button > a {color: #ffffff !important; text-decoration: none;}")
        (:title "Kindista"))

      (:body :style "font-family: Ubuntu, Roboto, \"Segoe UI\", \"Helvetica Neue\", Tahoma, sans-serif;"
        (:table :cellspacing 0
                :cellpadding 0
                :style "border-collapse: collapse; width: 98%;"
                :border 0

          (:tr (:td :style "background: #fafafa;
                            border-bottom: 1px solid #eeeeee;
                            padding: 6px 6px 3px;"

                 (:a :href "http://kindista.org/"
                     :style "text-decoration: none;
                             color: #799f56;
                             font-size: 22px;
                             font-weight: 500;"
                     (:img :src (s+ *email-url* "media/logo.png") :width 136 :height 26))))

          (:tr (:td :style "padding: 10px;
                            color: #000000;
                            background: #ffffff;"
                 (str content))))))))
