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

(defun comment-notification-email-text (conversation-id from subject people text)
  (strcat "A conversation with " people
"

Subject: " subject
"

"
from " says:

"
"\"" text "\" "
"

You can see the conversation on Kindista here:
"
(strcat +base-url+ "/conversation/" conversation-id)

"

If you no longer wish to receive notifications when people send you messages, please edit your settings:
"
(strcat +base-url+ "/settings/communication")

"

Thank you for sharing your gifts with us!
-The Kindista Team"))


(defun comment-notification-email-html (conversation-id from subject people text)
  (html-email-base
    (html
      (:p :style *style-p*
        "A conversation with " (str people))

      (:p :style *style-p*
        (:strong "Subject: " (str subject)))

      (:p :style *style-p*
        (str from) " says:")

      (:table :cellspacing 0
              :cellpadding 0
              :style *style-quote-box*
        (:tr (:td :style "padding: 4px 12px;"
                 "\"" (str text) "\"")))

      (:p :style *style-p*
       "You can see the conversation on Kindista here: "
       (:a :href (strcat +base-url+ "/conversation/" conversation-id)
                 (str (strcat +base-url+ "/conversation/" conversation-id))))

      (:p :style *style-p*
          "If you no long wish to receive notifications when people send you messages, please edit your settings:"
       (:br)
       (:a :href (strcat +base-url+ "/settings/communication")
                 (str (strcat +base-url+ "/settings/communication"))))

      (:p :style *style-p* "Thank you for sharing your gifts with us!")
      (:p "-The Kindista Team"))))

