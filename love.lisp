; Copyright 2013 CommonGoods Network, Inc.
;
; This file is part of Kindista.
;
; Kindista is free software: you can redistribute it and/or modify
; it under the terms of the GNU Affero General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; Kindista is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU Affero General Public License for more details.
;
; You should have received a copy of the GNU Affero General Public License
; along with Kindista.  If not, see <http://www.gnu.org/licenses/>.

(in-package :kindista)

(defun love (id &key (userid *userid*))
  (with-locked-hash-table (*db*)
    (let* ((user (db userid))
           (loves (getf user :loves)))
      (unless (member id loves)
        (setf (getf user :loves) (cons id loves))
        (update-db userid user))))

  (with-locked-hash-table (*love-index*)
    (let* ((loves (gethash id *love-index*)))
      (unless (member userid loves)
        (setf (gethash id *love-index*) (cons userid loves))))))

(defun unlove (id &key (userid *userid*))
  (with-locked-hash-table (*db*)
    (let* ((user (db userid))
           (loves (getf user :loves)))
      (when (member id loves)
        (setf (getf user :loves) (remove id loves))
        (update-db userid user))))

  (with-locked-hash-table (*love-index*)
    (let* ((loves (gethash id *love-index*)))
      (when (member userid loves)
        (setf (gethash id *love-index*) (remove userid loves))))))

(defun loves (id)
  (gethash id *love-index*))

