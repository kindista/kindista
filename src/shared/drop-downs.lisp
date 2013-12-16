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

(defun privacy-selection-html (item-type selected groups &key (class "identity") onchange)
"Groups should be an a-list of (groupid . group-name)"
  (html
    (:h2  "Who can see this " (str item-type) "?")
    (:select :name "privacy-selection" :class class :onchange onchange
      (:option :value ""
               :selected (when (null selected) "")
               "Anyone")
      (dolist (group (sort groups #'string< :key #'cdr))
        (htm (:option :value (car group)
                      :selected (when (eql selected (car group)) "")
                      (str (cdr group))" group members "))))))

(defun identity-selection-html (selected groups &key (class "identity") onchange)
"Groups should be an a-list of (groupid . group-name)"
  (html
    (:select :name "identity-selection" :class class :onchange onchange
      (:option :value *userid*
               :selected (when (eql selected *userid*) "")
               (str (getf *user* :name))" ")
      (dolist (group (sort groups #'string< :key #'cdr))
        (htm (:option :value (car group)
                      :selected (when (eql selected (car group)) "")
                      (str (cdr group))" "))))))

