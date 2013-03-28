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

(defun get-settings ()
  (standard-page
    "Settings"
    (html
      (:h1 "Settings"))))

(defun post-settings ()
  
  (when (post-parameter "bio-doing")
    (unless (getf *user* :bio)
      (modify-db *userid* :bio t))
    (modify-db *userid* :bio-doing (post-parameter "bio-doing"))
    (see-other (or (post-parameter "next") "/home")))

  (when (post-parameter "bio-summary")
    (unless (getf *user* :bio)
      (modify-db *userid* :bio t))
    (modify-db *userid* :bio-summary (post-parameter "bio-summary"))
    (see-other (or (post-parameter "next") "/home")))

  (when (post-parameter "bio-into")
    (unless (getf *user* :bio)
      (modify-db *userid* :bio t))
    (modify-db *userid* :bio-into (post-parameter "bio-into"))
    (see-other (or (post-parameter "next") "/home")))

  (when (post-parameter "bio-contact")
    (unless (getf *user* :bio)
      (modify-db *userid* :bio t)) (modify-db *userid* :bio-contact (post-parameter "bio-contact"))
    (see-other (or (post-parameter "next") "/home")))

  (when (post-parameter "bio-skills")
    (unless (getf *user* :bio)
      (modify-db *userid* :bio t))
    (modify-db *userid* :bio-skills (post-parameter "bio-skills"))
    (see-other (or (post-parameter "next") "/home")))

  (when (post-parameter "address")
    (multiple-value-bind (lat long address city state country street zip)
        (geocode-address (post-parameter "address"))
      (declare (ignore country))
      (modify-db *userid* :lat lat :long long :address address :city city :state state :street street :zip zip)
      (see-other (or (post-parameter "next") "/home"))))

  (when (post-parameter "reset-location")
    (modify-db *userid* :lat nil :long nil :address nil :location nil)
    (see-other (or (post-parameter "next") "/home")))

  (when (scan +number-scanner+ (post-parameter "rdist"))
    (modify-db *userid* :rdist (parse-integer (post-parameter "rdist")))
    (flash "Your search distance for resources and requests has been changed!")
    (see-other (or (post-parameter "next") "/requests")))

  (when (scan +number-scanner+ (post-parameter "sdist"))
    (modify-db *userid* :rdist (parse-integer (post-parameter "sdist")))
    (flash "Your default search distance has been changed!")
    (see-other (or (post-parameter "next") "/requests")))

  (when (scan +number-scanner+ (post-parameter "distance"))
    (modify-db *userid* :distance (parse-integer (post-parameter "distance")))
    (flash (format nil "Now showing activity within ~a miles." (post-parameter "distance")))
    (see-other (or (post-parameter "next") "/home")))

  (when (equalp (post-parameter "help") "0")
    (modify-db *userid* :help nil)
    (see-other (or (referer) "/home")))

  (when (equalp (post-parameter "help") "1")
    (modify-db *userid* :help t)
    (see-other (or (referer) "/home")))

  (when (and (post-parameter "confirm-location")
        (getf *user* :lat)
        (getf *user* :long))
    (modify-db *userid* :location t)
    (see-other (or (post-parameter "next") "/home")))

  )
