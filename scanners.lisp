(in-package :kindista)

(defvar +number-scanner+ (ppcre:create-scanner "^\\d+$"))
(defvar +email-scanner+ (ppcre:create-scanner
                         "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,6}$"))
