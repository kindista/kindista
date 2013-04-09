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

(defun person-button (id alias button-name)
  (let* ((mutuals (mutual-connections id))
         (person (db id))
         (name (getf person :name))) 
    (html
      (:button :class "card" :value id :name button-name
        (:img :src (strcat "/media/avatar/" id ".jpg"))
        (:h3 (str name))
        (unless (string= name alias)
          (htm (:p "nickname: " (str alias))))
        (awhen (getf person :city) 
          (htm (:p "Lives in " (str it))))    
        (when mutuals
          (htm (:p (str (strcat (length mutuals) " mutual connection")) 
                   (str (when (> (length mutuals) 1) "s")))))))))

(defun person-card (id alias)
  (let* ((mutuals (mutual-connections id))
         (person (db id))
         (name (getf person :name)))
    (html
      (:div :class "card"
        (:div :class "image" (:img :src (strcat "/media/avatar/" id ".jpg")))
        (:h3 (:a :href (strcat "/people/" (username-or-id id))
               (str name)))
        (unless (string= name alias)
          (htm (:p "nickname: " (str alias))))

        (awhen (getf person :city) 
          (htm (:p "Lives in " (str it))))
        (when mutuals
          (htm (:p (:a :href (strcat "/people/" id "/connections" ) 
                   (str (strcat (length mutuals) " mutual connection")) 
                   (str (when (> (length mutuals) 1) "s"))))))))))
