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

(defun person-card (id &key alias button suggestion-reasons remove)
  (let* ((mutuals (mutual-connections id))
         (person (db id))
         (name (getf person :name))
         (link (s+ "/people/" (username-or-id id)))
         (city (awhen (getf person :city) it)))

    (html

      (:div :class "person card"

       (awhen remove
         (htm
           (str it)))

       (:div :class "image"
        (:a :href link (:img :src (get-avatar-thumbnail id 100 100))))

        (awhen button
          (htm
            (:div :class "card-button"
              (str (v-align-middle (str it))))))

        (:div :class "card-details"
         (:h3 (:a :href link
               (str name)))


         (when alias
           (unless (string= name alias)
             (htm (:p "nickname: " (str alias)))))

         (aif suggestion-reasons (str it)

           ; below is "default" content
           (progn
             (awhen city (htm (:p "Lives in " (str it))))
             (when mutuals
               (htm (:p (:a :href (s+ link "/connections" )
                         (str (pluralize mutuals
                                         " mutual connection")))))))))))))
