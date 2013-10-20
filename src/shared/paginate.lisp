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

(defun paginate-links (page more &optional (url (script-name*)))
  (when (or (> page 0) more)
    (html
      (:div :class "item"
       (when (> page 1)
         (htm
           (:a :href (strcat url "?p=0") "&larr; first page ")))
       (when (> page 0)
         (htm
           (:a :href (strcat url "?p=" (- page 1)) "&larr; previous page")))
       "&nbsp;"
       (when more
         (htm
           (:a :style "float: right;" :href (url-compose url "p" (+ page 1)) "next page &rarr;")))))))
