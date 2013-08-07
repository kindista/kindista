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

(defun create-image (path content-type)
  (let* ((suffix (cond
                  ((string= content-type "image/jpeg")
                   "jpg")
                  ((string= content-type "image/png")
                   "png")
                  ((string= content-type "image/gif")
                   "gif")
                  (t
                   (error "~S is not a supported content type" content-type))))
         (image (insert-db `(:type :image
                             :suffix ,suffix
                             :content-type ,content-type)))
         (filename (strcat image "." suffix)))
   (copy-file path (merge-pathnames *original-images* filename))
   (modify-db image :filename filename)
   (values image)))

(defun get-image-thumbnail (id maxwidth maxheight)
  (let* ((image (db id))
         (filename (format nil "~d-~d-~d.~a" id maxwidth maxheight (getf image :suffix)))
         (filepath (merge-pathnames *images-path* filename)))
    (assert image)
    (unless (file-exists-p filepath)
      (run-program *convert-path*
                   (list (strcat *original-images* (getf image :filename))
                         "-scale"
                         (strcat maxwidth "x" maxheight)
                         (native-namestring filepath))))
    (strcat *images-base* filename)))

(defun get-avatar-thumbnail (userid maxwidth maxheight)
  (aif (db userid :avatar)
    (get-image-thumbnail it maxwidth maxheight)
    *avatar-not-found*))
