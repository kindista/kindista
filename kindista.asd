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
  :components ((:module src
                :serial t
                :components ((:file "package")
                             (:file "helpers")
                             (:file "settings")
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
                              :components ((:file "sidebar")
                                           (:file "timestamp")
                                           (:file "menu-horiz")
                                           (:file "card")
                                           (:file "person-card")))
                             (:module features
                              :serial t
                              :components ((:file "about")
                                           (:file "admin")
                                           (:file "comments")
                                           (:file "discussions")
                                           (:file "invitations")
                                           (:file "donate")
                                           (:file "events")
                                           (:file "gratitude")
                                           (:file "groups")
                                           (:file "help")
                                           (:file "home")
                                           (:file "legacy")
                                           (:file "love")
                                           (:file "messages")
                                           (:file "people")
                                           (:file "requests")
                                           (:file "resources")
                                           (:file "root")
                                           (:file "settings")
                                           (:file "signup")
                                           (:file "search")))
                             (:file "routes")
                             (:file "main")
                             (:module email
                                      :serial t
                                      :components ((:file "base")
                                                   (:file "invitation")
                                                   (:file "gratitude-notification")
                                                   (:file "email-handlers")))))))
