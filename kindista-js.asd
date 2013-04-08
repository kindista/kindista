(defpackage #:kindista-js-system
  (:use #:cl #:asdf))

(in-package :kindista-js-system)

(operate 'load-op :paren-files)

(defsystem #:kindista-js
  :name "Kindista JavaScript"
  :description "A social network for local sharing"
  :license "GNU Affero General Public License Version 3 (see file COPYING)"
  :maintainer "Nicholas E. Walker"
  :serial t
  :depends-on (:parenscript
               :paren-files)
  :components ((:module js
                :serial t
                :components ((:file "package")
                             (:parenscript-file "main")))))
