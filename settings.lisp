(in-package :kindista)

(define-constant +db-path+ "/home/walker/k2/data/" :test #'string=)
(define-constant +avatar-path+ "/home/walker/k2/media/avatars/"; <- trailing slash
                 :test #'string=)
(define-constant +mail-server+ "localhost" :test #'string=)


(setf stripe:*default-api-key* "sk_test_j8V6OYQsrVoEPBNvsdSqgbJI")

