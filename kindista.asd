;;;; kindista.asd

(asdf:defsystem #:kindista
  :serial t
  :depends-on (:alexandria
               :anaphora
               :bordeaux-threads
               :ironclad
               :cl-gd
               :cl-json
               :cl-ppcre
               :cl-smtp
               :cl-who
               :cl-markdown
               :drakma
               :flexi-streams
               :stem
               :hunchentoot
               :levenshtein
               :iterate
               :css-lite
               :double-metaphone)
  :components ((:file "package")
               (:file "macros")
               (:file "scanners")
               (:file "db")
               (:file "people")
               (:file "activity")
               (:file "resources")
               (:file "comments")
               (:file "time")
               (:file "settings")
               (:file "http")))

