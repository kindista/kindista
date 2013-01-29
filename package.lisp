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

