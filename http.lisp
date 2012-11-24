(in-package :kindista)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(setf (cl-who:html-mode) :sgml)

(defvar *flashes* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))

(defun flash (message &key (id *userid*) error)
  (with-locked-hash-table (*flashes*)
    (push
      (format nil
              (if error
                "<div class=\"flash err\">~a</div>"
                "<div class=\"flash\">~a</div>")
              message)
      (gethash id *flashes*))))

(defun flashes (&optional (id *userid*))
  (with-locked-hash-table (*flashes*)
    (prog1
      (gethash id *flashes*)
      (setf (gethash id *flashes*) nil))))

(defun not-found ()
  (flash "The page you asked for could not be found :-( Here's the home page instead." :error t)
  (see-other "/"))

;;; routing and acceptor {{{

(setf *methods-for-post-parameters* '(:post :put))

(defvar *routes* ())

(defparameter *route-param-scanner*
  (create-scanner "<((\\w+):)?([-\\w]+)>"))

(defun make-path-scanner (path)

  (let ((names ())
        (newpath path))

    (do-scans (s e reg-starts reg-ends *route-param-scanner* path)
      (let* ((values (map 'vector
                          (lambda (rs re)
                            (if rs
                              (subseq path rs re)
                              nil))
                          reg-starts
                          reg-ends))
             (pattern nil)
             (param-type (elt values 1)))

        (setf names (append names (list (make-keyword
                                          (string-upcase
                                           (elt values 2))))))

        (cond
          ((string= param-type "int")
           (setf pattern "(\\d+)"))
          ((string= param-type "email")
           (setf pattern "(^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,6})"))
          (t
           (setf pattern "([^\/]+)")))

        (setf newpath (regex-replace *route-param-scanner* newpath pattern))))

    (create-scanner (s+ "^" newpath "$"))))

(defmacro defroute (path params &body body)
  `(setf *routes* (cons (cons (make-path-scanner ,path)
                              (lambda ,params
                                (case (request-method*)
                                  ,@body
                                  (t (setf (return-code*) +http-method-not-allowed+) ""))))
                        *routes*)))


; }}}

;; globals


;; special variables

(defvar *user* nil) ; current user
(defvar *userid* nil) ; current user

(defun random-password (length)
  (declare (type fixnum length))
  (string-downcase
    (with-output-to-string (stream)
      (let ((*print-base* 36))
        (loop repeat length do (princ (random 36) stream))))))

(defun crypt-password (password salt)
  (s+ salt ":"
    (ironclad:byte-array-to-hex-string
      (ironclad:digest-sequence :sha256 (ironclad:ascii-string-to-byte-array
                                          (s+ salt password))))))

(defun new-password (password)
  (let ((salt (random-password 10)))
    (crypt-password password salt)))

(defun password= (password crypted-password)
  (let* ((salt (first (ppcre:split ":" crypted-password :limit 2)))
         (crypted-password2 (crypt-password password salt)))
    (string= crypted-password crypted-password2)))

(defun set-password (username password)
  (with-file-lock ((s+ +db-path+ "users/" username))
    (let ((user (load-user username)))
      (setf (getf user :pass) (new-password password))
      (save-user username user))))

(defun password-match-p (id password)
  (let ((crypted-password (getf (db id) :pass)))
    (if crypted-password
      (password= password crypted-password)
      nil)))

(defun check-token-cookie ()
  (let ((token (cookie-in "token")))
    (when token
      (let ((userid (check-token token)))
        (when (and (not userid) token)
          (delete-token-cookie))
        userid))))

(defun authorization-required ()
  (setf (return-code*) +http-see-other+)
  (setf (header-out :location) (if (string= (script-name*) "/")
                                 "/"
                                 (s+ "/?next=" (script-name*))))
  "")

(defmacro with-user (&body body)
  `(let* ((*userid* (or *userid* (check-token-cookie)))
          (*user* (or *user* (db *userid*))))
     ,@body))

(defmacro require-user (&body body)
  `(with-user
     (if *userid*
       (progn ,@body)
       (authorization-required))))

(defmacro require-admin (&body body)
  `(with-user
     (if (getf *user* :admin)
       (progn ,@body)
       (not-found))))

;; tokens

(defun make-token (id)
  (with-mutex (*make-token-lock*)
    (do (token)
      ((not (gethash (setf token (random-password 30)) *auth-tokens*))
      (prog1 token
        (setf (gethash token *auth-tokens*) (list id (get-universal-time))))))))

(defun check-token (token)
  (let ((value (gethash token *auth-tokens*)))
    (when (and value (< (get-universal-time) (+ (cadr value) 2592000)))
      (car value))))

(defun delete-token-cookie ()
  (awhen (cookie-in "token")
    (awhen (gethash it *auth-tokens*)
      (setf (cadr it) 0))
    (set-cookie "token" :value ""
                :http-only t
                :expires 0
                :secure nil)))

(defun see-other (url)
  (setf (return-code*) +http-see-other+)
  (setf (header-out :location) url)
  "")

(defclass k-acceptor (acceptor) ())

(defmethod acceptor-dispatch-request ((acceptor k-acceptor) request)
  (dolist (rule *routes*)
    (multiple-value-bind (match results)
        (scan-to-strings (car rule) (script-name*))
      (when match
        (return-from acceptor-dispatch-request
          (progn
            (apply (cdr rule) (coerce results 'list)))))))
  (with-user
    (if *userid*
      (not-found)
      "not found")))

#|(defmethod acceptor-status-message ((acceptor k-acceptor)
                                    http-status-code
                                    &rest properties
                                    &key &allow-other-keys)
  "Disable automatic error pages."
  (declare (ignore http-status-code properties))
  nil)|#

(defvar *acceptor* (make-instance 'k-acceptor
                                  :port 5000))

(defun page-header (&optional extra)
  (html
    (:div :id "header"
     (:a :href "/" (:img :src "/media/logo.png" :alt "kindista"))
     ;(:h1 "kindista")
     (when extra
       (str extra)))))

(defun icon (type)
  (html
    (:img :class "icon" :src (s+ "/media/icons/" type ".png") :alt " ")))

(defun menu-item (title slug icon &optional selected)
  (html
    (:li :class (when selected "selected") (:a :href (s+ "/" icon) (str (icon slug)) (str title)))))

(defun menu (items &optional selected)
  (html
    (:menu
      (iter (for item in items) 
            (when item
              (str (menu-item (car item)
                              (cadr item)
                              (or (caddr item) (cadr item))
                              (string= selected (cadr item)))))))))
(defun welcome-bar (content)
  (html
    (:div :class "welcome item"
      (:form :method "post" :action "/settings"
        (:button :class "corner" :type "submit" :name "help" :value "0" "[ hide this help text ]"))
      (str content))))

(defun base-page (title body &key class)
  (html
    "<!DOCTYPE html>"
    (:html
      (:head
        (:title (if title (str (s+ title " | Kindista")) "Kindista"))
        (:meta :name "viewport" :content "width=device-width,initial-scale=1.0,maximum-scale=1.0")
        (:meta :name "HandheldFriendly" :content "True")
        ;(:meta :name "apple-mobile-web-app-status-bar-style" :content "black")
        (:link :rel "stylesheet" :href "/media/style.css")
        (str "<!--[if lt IE 9]>")
        (:link :rel "stylesheet" :href "/media/ie.css" :type "text/css")
        (str "<![endif]-->"))
      (:body :class class :onload "if(!location.hash){window.scrollTo(0,0);}"
        (:a :name "top")
        (str body)))))

(defun header-page (title header-extra body &key class)
  (base-page title
             (html
               (str (page-header header-extra))
               (str body))
             :class class))

(defun standard-page (title body &key selected top right)
  (header-page title
               (html
                 (:div
                   (:a :href "#search" (:img :alt "Search" :src "/media/icons/search.png"))  
                   " "
                   (:a :href "#menu" (:img :alt "Menu" :src "/media/icons/menu.png"))))
               (html
                 (dolist (flash (flashes))
                   (str flash))
                 (when top
                   (htm
                     (:div :id "full"
                      (str top))))
                 (:div :id "body"
                   (when right
                     (htm
                       (:div :id "right"
                        (str right))))
                   (str body))

                 (:form :action "/search" :method "POST" :id "search"
                   (:table
                    (:tr
                      (:td (:input :type "text" :name "q"))
                      (:td (:input :type "submit" :value "Search")))))

                 (:div :id "menu"
                   (:table
                     (:tr
                       (:td :rowspan "2"
                        (:img :src (format nil "/media/avatar/~A.jpg" *userid*)))
                       (:td (:a :href (s+ "/people/" (username-or-id)) (str (getf *user* :name)))))
                     (:tr
                       (:td (:a :href "/logout" "Log out"))))

                   (str (menu (list '("Home" "home" "")
                                    '("Messages" "messages")
                                    '("People" "people")
                                    '("Offers" "offers")
                                    '("Requests" "requests")
                                    ;("Events" "events") 
                                    (when (getf *user* :admin)
                                      '("Admin" "admin")))
                              selected))

                   (:p :id "copyright"
                     "Kindista &copy; 2012"
                     (:br)
                     "Programmed in Common Lisp")

                   (:a :href "#top"
                       "Back to the top")
                   )
                 )
               :class (when right "right")))


(defun run ()
  (load-db)
  (load-tokens)
  (start *acceptor*))

