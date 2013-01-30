;;;; kindista.asd

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
               :stem)
  :components ((:file "package")
               (:file "settings")
               (:file "helpers")
               (:file "scanners")
               (:file "db")
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
               (:file "messages")
               (:file "comments")
               (:file "love")
               (:file "admin")
               (:file "time")
               (:file "geocode")
               (:file "home")
               (:file "routes")))

