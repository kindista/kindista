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

;;;; package.lisp

;(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defpackage :kindista
            (:use :cl
                  :sb-thread
                  :sb-ext
                  :stem
                  :alexandria
                  :anaphora-basic
                  :hunchentoot
                  :cl-ppcre
                  :iterate)
            (:import-from :parenscript :ps :ps-inline :@ :f :create)
            (:import-from :ps-dom1-symbols :document :get-element-by-id :submit :set-attribute
                                           :remove-attribute)
            (:import-from :drakma :http-request)
            (:import-from :double-metaphone :double-metaphone)
            (:import-from :cl-markdown :markdown)
            (:import-from :cl-who :with-html-output-to-string :str :htm :fmt)
            (:export :run-server :stop-server))

