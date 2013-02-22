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

(asdf:defsystem #:kindista
  :serial t
  :depends-on (:alexandria
               :anaphora
               :cl-gd
               :cl-json
               :cl-markdown
               :cl-ppcre
               :cl-smtp
               :cl-who
               :cl-stripe
               :parenscript
               :css-lite
               :vecto
               :double-metaphone
               :drakma
               :flexi-streams
               :hunchentoot
               :ironclad
               :iterate
               :levenshtein
               :sb-concurrency
               :stem)
  :components ((:file "package")
               (:file "settings")
               (:file "helpers")
               (:file "events")
               (:file "scanners")
               (:file "db")
               (:file "metrics")
               (:file "donations")
               (:file "http")
               (:file "tags")
               (:file "search")
               (:file "people")
               (:file "activity")
               (:file "timeline")
               (:file "gratitude")
               (:file "requests")
               (:file "resources")
               (:file "inventory")
               (:file "messages")
               (:file "comments")
               (:file "love")
               (:file "admin")
               (:file "time")
               (:file "geocode")
               (:file "home")
               (:file "routes")
               (:file "main")
               (:file "log")))

