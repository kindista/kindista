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
         (members (length (group-members id)))
         (name (getf group :name))
         (link (s+ "/groups/" (username-or-id id)))
         (distance (when (and (getf *user* :lat)
                              (getf *user* :long)
                              (getf group :lat)
                              (getf group :long))
                     (air-distance (getf *user* :lat) (getf *user* :long)
                                   (getf group :lat) (getf group :long)))))
    (labels ((show-different-location (place)
               (unless (and (getf *user* place)
                            (string= (getf group place)
                                     (getf *user* place)))
                 (s+ (unless (eql place :city) ", ")
                     (getf group place)))))
     (html
      (:div :class "card"
        (:div :class "image" (:a :href link (:img :alt id :src (get-avatar-thumbnail id 300 300))))
        (:div :class "card-details"
          (:h3 (:a :href link
                 (str name)))

          (aif (show-different-location :city)
            (htm
              (:p "In "
                  (str (s+ it
                         (show-different-location :state)
                         (show-different-location :country)))))
            (awhen distance
              (htm
                (:p "within " (str (distance-string it))))))

          (htm (:p (:a :href (s+ link "/members" )
                   (str (strcat members " member"))
                   (str (when (or (eql members 0) (> members 1)) "s")))))))))))
