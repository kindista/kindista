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

(defun geocode-address (address)
  (let* ((results (first
                    (cdr
                      (assoc
                        :results
                        (json:decode-json-from-string
                          (octets-to-string
                            (http-request
                              "http://maps.googleapis.com/maps/api/geocode/json"
                              :parameters `(("address" . ,address)
                                            ("sensor" . "false")))
                            :external-format :utf-8))))))

         (location (cdr (assoc :location (cdr (assoc :geometry results)))))
         city
         country
         number
         street
         state
         zip)

    (iter (for component in (cdr (assoc :address--components results)))
          (until (and city state country number street zip))
          (let ((types (cdr (assoc :types component))))
            (cond
              ((member "street_number" types :test #'string=)
               (setf number (cdr (assoc :short--name component))))

              ((member "route" types :test #'string=)
               (setf street (cdr (assoc :short--name component))))

              ((member "country" types :test #'string=)
               (setf country (cdr (assoc :short--name component))))

              ((member "administrative_area_level_1" types :test #'string=)
               (setf state (cdr (assoc :short--name component))))

              ((member "postal_code" types :test #'string=)
               (setf zip (cdr (assoc :short--name component))))

              ((member "locality" types :test #'string=)
               (setf city (cdr (assoc :long--name component)))))))


    (values (cdr (assoc :lat location))
            (cdr (assoc :lng location))
            (cdr (assoc :formatted--address results))
            city
            state
            country
            (format nil "~a ~a" number street)
            zip)))

(defun static-google-map (&key (size "400x400") lat long (marker t) (zoom 13) (scale 2))
  (strcat "<img src=\"http://maps.googleapis.com/maps/api/staticmap?size="
           size
           "&scale="
           scale
           "&maptype=roadmap&center="
           lat
           ","
           long
           "&sensor=false&zoom="
           zoom
           (if marker
             (strcat "&markers=color:0x90B04B|" lat "," long)
             "")
           "\">"))

