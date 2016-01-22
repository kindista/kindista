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

(defun register-new-location (lat long &key address street city state zip country)
  (when (equalp street "nil nil") (setf street nil))
  (let ((id (insert-db (remove-nil-plist-pairs (list :lat lat
                                                     :long long
                                                     :address address
                                                     :street street
                                                     :city city
                                                     :state state
                                                     :zip zip
                                                     :country country)))))
    (index-location id (db id))))

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

(defun existing-address
  (lat long
   &key address street city state zip country)
  (dolist (location-id (existing-locations lat long))
    (let ((location (db location-id)))
      (when (and (string= (ensure-address-string street)
                          (getf location :street))
                 (or (aand (ensure-address-string address)
                           (string= it (getf location :address)))
                     (and (string= city (getf location :city))
                          (string= state (getf location :state))
                          (string= zip (getf location :zip))
                          (string= country (getf location :country)))))
        (return location-id)))))

(defun create-location-items-from-current-db (&aux (unique-locations 0))
  (dolist (id (hash-table-keys *db*))
    (let* ((data (db id))
           (lat (getf data :lat))
           (long (getf data :long))
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
        (aif (existing-address lat long
                               :address address
                               :street street
                               :city city
                               :state state
                               :zip zip
                               :country country)
          (let ((location (db it)))
            (flet ((update-location-data (parameter)
                     (when (and (getf data parameter)
                                (not (getf location parameter)))
                       (pprint (list it parameter (getf data parameter)))
                       (terpri)
                       (modify-db it parameter (getf data parameter)))))
              (mapcar #'update-location-data (list :city :state :zip :country)))
            (setf location-id it))
          (progn (setf location-id (register-new-location lat long
                                                          :address address
                                                          :street street
                                                          :city city
                                                          :state state
                                                          :zip zip
                                                          :country country))
                 (incf unique-locations)))

        (modify-db id
                   (if (find (getf data :type)
                             (list :group :person))
                     :locations
                     :location)
                   (case (getf data :type)
                     (:group (cons location-id "main"))
                     (:person (cons location-id "home"))
                     (t location-id))
                   :lat nil
                   :long nil
                   :address nil
                   :street nil
                   :city nil
                   :state nil
                   :zip nil
                   :country nil))))
  unique-locations)
