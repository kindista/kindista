(defpackage #:kindista-js-system
  (:use #:cl #:asdf))

;; no longer compiling with (paren-files:compile-script-system) 
;; becase paren-files' ability to interface with asdf is currently busted

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
