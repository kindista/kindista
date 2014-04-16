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

(defun verify-location (url page-title lat long
                        &rest parameters)
"Parameters is a list of string-pairs: post-parameter-name post-parameter-value which will be included with the post requst. The post-parameter-value can be either a single string or a list of strings."
  (standard-page
    page-title
    (html
      (:div :class "item"
        (:h2 (str page-title))
        (str (static-google-map :size "280x150" :zoom 12 :lat lat :long long))
        (:form :method "post" :action url
          (:h3 "Is this location correct?")
          (:input :type "hidden" :name "lat" :value lat)
          (:input :type "hidden" :name "long" :value long)
          (do ((params parameters (cddr params)))
              ((not params))
              (flet ((hidden-input (value)
                       (html (:input :type "hidden"
                                     :name (car params)
                                     :value (if (stringp value)
                                              (escape-for-html value)
                                              value)))))
                (if (listp (cadr params))
                  (dolist (value (cadr params))
                    (str (hidden-input value)))
                  (str (hidden-input (cadr params))))))
          (:button :class "cancel"
                   :type "submit"
                   :name "reset-location"
                   "No, go back")
          (:button :class "yes"
                   :type "submit" :name "confirm-location" "Yes, this is correct"))))))
