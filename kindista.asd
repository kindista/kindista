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
  :name "Kindista"
  :description "A social network for local sharing"
  :license "GNU Affero General Public License Version 3 (see file COPYING)"
  :maintainer "Nicholas E. Walker"
  :serial t
  :depends-on (:alexandria
               :anaphora
               ;:cl-gd
               :cl-json
               :cl-markdown
               :cl-ppcre
               :cl-smtp
               :cl-who
               :cl-stripe
               :parenscript
               ;:css-lite
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
  :components ((:file "settings")
               (:module src
                :serial t
                :components ((:file "package")
                             (:file "helpers")
                             (:module db
                              :serial t
                              :components ((:file "indexes")
                                           (:file "main")))
                             (:module log
                              :serial t
                              :components ((:file "main")
                                           (:file "events")))
                             (:module http
                              :serial t
                              :components ( (:file "main")))
                             (:module shared
                              :serial t
                              :components ((:file "inventory")
                                           (:file "activity")
                                           (:file "geo")
                                           (:file "tags")
                                           (:file "timeline")
                                           (:file "time")))
                             (:module templates
                              :serial t
                              :components ((:file "sidebar")))
                             (:module features
                              :serial t
                              :components ((:file "about")
                                           (:file "admin")
                                           (:file "comments")
                                           (:file "donate")
                                           (:file "events")
                                           (:file "gratitude")
                                           (:file "groups")
                                           (:file "help")
                                           (:file "home")
                                           (:file "love")
                                           (:file "messages")
                                           (:file "people")
                                           (:file "requests")
                                           (:file "resources")
                                           (:file "search")))
                             (:file "routes")
                             (:file "main")))))

