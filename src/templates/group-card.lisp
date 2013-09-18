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

(defun group-card (id)
  (let* ((group (db id))
         (members (length (getf group :members))) 
         (name (getf group :name))
         (link (s+ "/groups/" (username-or-id id))))
    (html
      (:div :class "card"
        (:div :class "image" (:a :href link (:img :src (strcat "/media/avatar/" id ".jpg"))))
        (:h3 (:a :href link
               (str name)))

        (awhen (getf group :city) 
          (htm (:p "In " (str it))))

        (htm (:p (:a :href (s+ link "/members" ) 
                 (str (strcat members " member")) 
                 (str (when (or (eql members 0) (> members 1)) "s")))))))))
