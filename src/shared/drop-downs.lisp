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

(defun privacy-selection-html (item-type restrictedp groups groups-selected &key (class "privacy-selection") onchange)
"Groups should be an a-list of (groupid . group-name)"
  (html
    (:h2  "Who can see this " (str item-type) "?")
    (:div :class (s+ class (when (and restrictedp (> (length groups) 1))
                             " privacy-selection-details"))
      (:select :name "privacy-selection" :class class :onchange onchange
        (:option :value "public"
                 :selected (unless restrictedp "")
                 "Anyone")
        (:option :value "restricted"
                 :selected (when restrictedp "")
                 (str
                   (if (= 1 (length groups))
                     (s+ (cdar groups)
                         (when (= (caar groups) +kindista-id+)
                           " account group")
                          " members ")
                     "People in my groups"))))
      (when restrictedp
        (if (> (length groups) 1)
          (dolist (group (sort groups #'string-lessp :key #'cdr))
            (htm
              (:br)
              (:div :class "item-group-privacy"
                (:input :type "checkbox"
                        :name "groups-selected"
                        :checked (when (or (not groups-selected)
                                           (member (car group) groups-selected))
                                   "checked")
                        :value (car group)
                        (str (cdr group))
                        (str (when (= (car group) +kindista-id+)
                             " group account "))
                        " members"))))
          (htm (:input :type "hidden" :name "groups-selected" :value (caar groups))))))))

