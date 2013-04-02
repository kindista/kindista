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

(defun gratitude-notification-email-text (gratitude-id gratitude from)
  (s+ (getf (db from) :name)
    " has shared a statement of gratitude about you on Kindista.

    "
    (getf (db gratitude) :text)
    "
    
    You can see the statement on Kindista here:
    https://kindista.org/gratitude/" gratitude-id

    "If you no longer wish to receive notifications, please edit your settings:
    https://kindista.org/settings/email

    Thank you for sharing your gifts with us!
    -The Kindista Team"))


(defun gratitude-notification-email-html (gratitude-id gratitude from)
  (html-email-base
    (html
      (:p :style *style-p* 
          (str (person-email-link from)) 
            " has shared a "
            (:a :href (s+ "https://kindista.org/gratitude/" gratitude-id) 
                          "statement of gratitude") 
                " about you on Kindista.")

      (:table :cellspacing 0
              :cellpadding 0
              :style *style-quote-box*
        (:tr (:td :style "padding: 4px 12px;"

               (str (getf gratitude :text)))))

      (:p :style *style-p* 
          "If you no long wish to receive notifications, please edit your settings:"
       (:br)
       (:a :href "https://kindista.org/settings/email" "https://kindista.org/settings/email"))

      (:p :style *style-p* "Thank you for sharing your gifts with us!")
      (:p "-The Kindista Team"))))

