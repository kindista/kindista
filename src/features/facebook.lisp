;;; Copyright 2012-2015 CommonGoods Network, Inc.
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

(defun facebook-item-meta-content (typestring title description)
  (html
    (:meta :property "og:type"
           :content (s+ "kindistadotorg:" typestring))
    (:meta :property "og:title"
           :content (or title (s+ "Kindista " (string-capitalize typestring))))
    (:meta :property "og:description"
           :content (strcat* (when description
                               (strcat (ellipsis description)
                                       #\linefeed #\linefeed))
                             (string-case typestring
                               ("offer" "Kindista is for sharing resources freely.  Please do not use Kindista for buying, selling, or renting.")
                               ("request" "Kindista is for sharing resources freely.  Please do not use Kindista for buying, selling, or renting."))))))
