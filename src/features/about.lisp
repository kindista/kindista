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

(defparameter *about-html* (markdown-file (s+ +markdown-path+ "about.md")))

(defun go-about ()
  (moved-permanently "/about"))

(defun get-about ()
  (with-user
    (standard-page
      "About"
      (html
        (str (help-tabs-html :tab :about))
        (str *about-html*))
      :selected "faq"
      :right (html
               (str (donate-sidebar))
               (str (invite-sidebar))))))
