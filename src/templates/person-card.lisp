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


(defun person-card (id name &rest items)
  (let ((mutuals (mutual-connections id)))
    (html
      (:div :class "card"
        (:img :src (strcat "/media/avatar/" id ".jpg"))
        (:h3 (:a :href (strcat "/people/" (username-or-id id))
               (str name)))
        (:p "Lives in " (str (getf *user* :city)) ", " (str (getf *user* :state)))    
        (when mutuals
          (htm (:p (:a :href (strcat "/people/" id "/connections" ) 
                   (str (strcat (length mutuals) " mutual connection")) 
                   (str (when (> (length mutuals) 1) "s"))))))))))
