(in-package :cl-user)

(defpackage :kindista-js
  (:use :common-lisp
        :parenscript)
  (:export :set-display
           :submit-image-form))

(setf (ps::ps-package-prefix :kindista-js) "K")
