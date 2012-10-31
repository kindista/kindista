;;;; kindista.asd

(asdf:defsystem #:kindista
  :serial t
  :depends-on (:alexandria
               :anaphora
               :cl-gd
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
               (:file "people")
               (:file "activity")
               (:file "timeline")
               (:file "testimonials")
               (:file "resources")
               (:file "comments")
               (:file "time")
               (:file "settings")
               (:file "routes")))

