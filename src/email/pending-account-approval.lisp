;;; Copyright 2012-2016 CommonGoods Network, Inc.
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

(defun send-account-approval-email
  (id
   &key text
   &aux (user (db id))
        (name (getf user :name))
        (email (first (getf user :emails)))
        (message (unless (string= text "")
                   text)))
  (cl-smtp:send-email +mail-server+
                      "Kindista <noreply@kindista.org>"
                      email
                      "Your Kindista account has been approved!"
                      (account-approval-text name :text message)
                      :html-message (account-approval-html name :text message)))

(defun account-approval-text (name &key text)
(s+ "Hi " name ",
"

"
We have approved your Kindista account. Any items you have posted should now be visible on the site. You may now use it to contact other Kindista members. "
"We trust you; please be considerate and respectful of our community."
"

"
(when text
(s+ "Personal message from the Kindista crew: 
\""
text "\"

"))


"
Thanks for sharing your gifts with us!
"
"-The Kindista Team"))


(defun account-approval-html (name &key text)
(html-email-base
  (html
    (:p :style *style-p*
      "Hi " (str name) ","
      (:br)
      "We have approved your Kindista account. Any items you have posted should now be visible on the site. You may now use it to contact other Kindista members. "
      "We trust you; please be considerate and respectful of our community.")

    (when text
      (htm (:table :cellspacing 0
                   :cellpadding 0
                   :style *style-quote-box*
             (:tr (:td :style "padding: 4px 12px;"
                     "Personal message from the Kindista crew: "
                      (:br)
                      "\"" (str (email-text text)) "\"")))))

    (:p :style *style-p*
      "Thanks for sharing your gifts with us!")

    (:p "-The Kindista Team"))))
