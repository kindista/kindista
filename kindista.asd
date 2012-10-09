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
               (:file "time")
               (:file "settings")
               (:file "http")))

