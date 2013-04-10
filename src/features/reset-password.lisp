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

(defun get-reset-password ()
  (with-user 
    (standard-page 
      "Reset your password"
      (acond
        ((get-parameter "token")
         (html
           (:form :method "post" :class "password" :autocomplete "off" :action "/reset"
            (:div :class "submit-settings"
              (:button :class "yes" :type "submit" :class "submit" "Change password"))
            (:div
              (:label "Security code:")
              (:input :type "text"
                      :name "token"
                      :value it
                      :placeholder "enter security code here"))
            (:div
              (:label "New password:")
              (:input :type "password"
                      :name "new-password-1"
                      :placeholder "new password: at least 8 characters"))   
            (:div
              (:label "Confirm your new password:")
              (:input :type "password"
                      :name "new-password-2"
                      :placeholder "please retype your new password"))))   

            )
        (t
         (html 
           (:div :class "item"
             (:form :method "post" :action "/reset")))))
      (:p :class "help-text"
        "Minimum of 8 characters. "
        "We strongly recommend using either a mix of upper- and "
        "lower-case letters, numbers, and symbols; or a sentance "
        "of at least 8 words.")
      :right (html
               (str (donate-sidebar))))))


