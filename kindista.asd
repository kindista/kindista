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
               :css-lite
               :double-metaphone
               :drakma
               :flexi-streams
               :hunchentoot
               :ironclad
               :iterate
               :levenshtein
               :stem)
  :components ((:file "package")
               (:file "helpers")
               (:file "scanners")
               (:file "db")
               (:file "http")
               (:file "tags")
               (:file "search")
               (:file "people")
               (:file "activity")
               (:file "timeline")
               (:file "gratitude")
               (:file "requests")
               (:file "resources")
               (:file "comments")
               (:file "love")
               (:file "admin")
               (:file "time")
               (:file "geocode")
               (:file "settings")
               (:file "routes")))

