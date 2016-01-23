;;; Copyright 2016 CommonGoods Network, Inc.
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

(defun register-new-location (lat long &key street city state zip country)
  (when (equalp street "nil nil") (setf street nil))
  ;; "address" is not returned consistently for a lat/long pair
  ;; so we are storing that on the person/group/event instead of the location
  (let ((id (insert-db (remove-nil-plist-pairs (list :lat lat
                                                     :long long
                                                     :street street
                                                     :city city
                                                     :state state
                                                     :zip zip
                                                     :country country)))))
    (index-location id (db id))
    id))

(defun index-location (id data)
  (with-locked-hash-table (*location-latitude-index*)
    (pushnew id (gethash (getf data :lat) *location-latitude-index*)))
  (with-locked-hash-table (*location-longitude-index*)
    (pushnew id (gethash (getf data :long) *location-longitude-index*))))

(defun ensure-address-string (string)
  (when (and (stringp string)
             (not (string= string ""))
             (not (equalp string "nil nil")))
    string))

(defun existing-locations (lat long)
  (intersection (gethash lat *location-latitude-index*)
                (gethash long *location-longitude-index*)))

(defun existing-address (lat long)
  (car (existing-locations lat long)))

(defun find-multiple-similar-locations
  (&optional (axis :long) &aux (matches))
  (dolist (list (hash-table-values (case axis
                                     (:long *location-longitude-index*)
                                     (:lat *location-latitude-index*))))
    (when (< 1 (length list))
      (asetf matches (append list it))))
  matches)

(defun register-or-update-location
  (lat long
   &key street city state zip country
   &aux (location-id (existing-location lat long))
        (location (db location-id))
        (data (list :street street :city city :state state :zip zip :country country))
        (new-data))
   (aif location-id
     (progn
       (dolist (param (list :street :city :state :zip :country))
         (when (and (getf data param)
                    (not (getf location param)))
           (push (list param (getf data param)) new-data)))
       (apply #'modify-db new-data))
      (register-new-location lat long
                             :street street
                             :city city
                             :state state
                             :zip zip
                             :country country)))

(defun create-location-items-from-current-db
  (&aux (unique-locations 0))
  (dolist (id (hash-table-keys *db*))
    (let* ((data (db id))
           (lat (awhen (getf data :lat) (round-to it 5)))
           (long (awhen (getf data :long) (round-to it 5)))
           (address (ensure-address-string (getf data :address)))
           (street (ensure-address-string (getf data :street)))
           (city (getf data :city))
           (state (getf data :state))
           (zip (getf data :zip))
           (country (getf data :country))
           (location-id)
           )
      (when (and (floatp lat)
                 (floatp long))
        (aif (existing-address lat long)
          (let ((location (db it)))
            (flet ((update-location-data (parameter)
                     (when (and (getf data parameter)
                                (not (getf location parameter)))
                       (modify-db it parameter (getf data parameter)))))
              (mapcar #'update-location-data (list :street :city :state :zip :country)))
            (setf location-id it))
          (progn (setf location-id (register-new-location lat long
                                                          :street street
                                                          :city city
                                                          :state state
                                                          :zip zip
                                                          :country country))
                 (incf unique-locations)))

        (if (find (getf data :type) (list :group :person))
          (modify-db id
                     :locations (list
                                  (list :id location-id
                                        :address address
                                        :nickname (if (eq (getf data :type)
                                                          :person)
                                                    "home"
                                                    "main")))
                     :location nil
                     :lat nil
                     :long nil
                     :address nil
                     :street nil
                     :city nil
                     :state nil
                     :zip nil
                     :country nil)
          (modify-db id
                     :location (list :id location-id :address address)
                     :lat nil
                     :long nil
                     :address nil
                     :street nil
                     :city nil
                     :state nil
                     :zip nil)))))
  unique-locations)
