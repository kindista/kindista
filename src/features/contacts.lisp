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

(defun post-contacts ()
  (require-user
    (let ((contacts (getf *user* :following)))
      (cond
        ((scan +number-scanner+ (post-parameter "add"))
         (let ((id (parse-integer (post-parameter "add"))))
           (unless (member id contacts)
             (modify-db *userid* :following (cons id contacts))))
         (see-other (or (post-parameter "next") "/home")))

        ((scan +number-scanner+ (post-parameter "remove"))
         (let ((id (parse-integer (post-parameter "remove"))))
           (when (member id contacts)
             (modify-db *userid* :following (remove id contacts))))
         (see-other (or (post-parameter "next") "/home")))

        (t
         (flash "Sorry, couldn't make sense of that request.")
         (see-other "/home"))))))
