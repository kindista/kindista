;;; Copyright 2012-2013 CommonGoods Network, Inc.
;;;
;;; This file is part of Kindista.
;;;
;;; Kindista is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Affero General Public License as published
;;; by the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Kindista is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public
;;; License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with Kindista.  If not, see <http://www.gnu.org/licenses/>.

(in-package :kindista)

;(declaim (optimize (speed 0) (safety 3) (debug 3)))

(setf (cl-who:html-mode) :sgml)

(defvar *flashes* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))

(defvar *base-url* "/")

(defun flash (message &key error)
  (with-locked-hash-table (*flashes*)
    (push
      (format nil
              (if error
                "<div class=\"flash err\">~a</div>"
                "<div class=\"flash\">~a</div>")
              message)
      (gethash *token* *flashes*))))

(defun flashes ()
  (with-locked-hash-table (*flashes*)
    (prog1
      (gethash *token* *flashes*)
      (remhash *token* *flashes*))))

(defun not-found ()
  (flash "The page you requested could not be found. Here's the home page instead." :error t)
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

(defvar *token* nil) ; current token (session)
(defvar *donate-info* nil) ; current donation page data
(defvar *user* nil) ; current user
(defvar *userid* nil) ; current user
(defvar *latitude* 0.0)
(defvar *longitude* 0.0)

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
  (let ((token-id (cookie-in "token")))
    (when token-id
      (let ((token (check-token token-id)))
        (when (and (not token) token-id)
          (delete-token-cookie))
        token))))

(defmacro with-token (&body body)
  `(let ((*token* (or *token* (check-token-cookie) (start-token))))
     ,@body))

(defmacro with-user (&body body)
  `(with-token
    (let* ((*userid* (or *userid* (token-userid *token*)))
           (*user* (or *user* (db *userid*))))
       ,@body)))

(defmacro with-location (&body body)
  `(let ((*latitude* (or (getf *user* :lat) 44.028297))
         (*longitude* (or (getf *user* :long) -123.076065)))
     ,@body))

(defmacro with-donate-info (&body body)
  `(with-user
     (let ((*donate-info* (or (token-donate-info *token*)
                              (setf (token-donate-info *token*)
                                    (make-donate-info :address (getf *user* :street)
                                                      :city (getf *user* :city)
                                                      :state (getf *user* :state)
                                                      :email (getf *user* :email)
                                                      :zip (getf *user* :zip)
                                                      :name (getf *user* :name))))))
       ,@body)))

(defmacro require-user (&body body)
  `(with-user
     (if *userid*
       (progn ,@body)
       (see-other "/home"))))

(defmacro require-test ((test &optional message) &body body)
  `(if ,test
     (progn ,@body)
     ,(if message
        `(progn
           (flash ,message)
           (see-other "/")) 
        `(see-other "/"))))

;; tokens

(defun start-token ()
  (with-locked-hash-table (*tokens*)
    (do (token)
      ((not (gethash (setf token (random-password 30)) *tokens*))
      (prog1
        (setf (gethash token *tokens*)
              (make-token :created (get-universal-time)))
        (set-cookie "token" :value token
                            :http-only t
                            :path "/"
                            :expires (+ (get-universal-time) 2592000)
                            :secure nil))))))

(defun check-token (cookie-value)
  (let ((token (gethash cookie-value *tokens*)))
    (if (and token (< (get-universal-time) (+ (token-created token) 2592000)))
      token
      (progn (remhash cookie-value *tokens*) nil))))

(defun delete-token-cookie ()
  (awhen (cookie-in "token")
    (awhen (gethash it *tokens*)
      (remhash it *tokens*))
    (set-cookie "token" :value ""
                :http-only t
                :expires 0
                :secure nil)))

(defun reset-token-cookie ()
  (awhen (cookie-in "token")
    (awhen (gethash it *tokens*)
      (remhash it *tokens*))
    (start-token))) 

(defun markdown-file (path)
  (multiple-value-bind (doc text)
      (markdown (pathname path) :stream nil)
    (declare (ignore doc))
    text))

(defun see-other (url)
  (setf (return-code*) +http-see-other+)
  (setf (header-out :location) url)
  "")

(defun moved-permanently (url)
  (setf (return-code*) +http-moved-permanently+)
  (setf (header-out :location) url)
  "")

(defclass k-acceptor (acceptor) ())

(defmethod acceptor-dispatch-request ((acceptor k-acceptor) request)
  (with-token
    (dolist (rule *routes*)
      (multiple-value-bind (match results)
          (scan-to-strings (car rule) (script-name*))
        (when match
          (return-from acceptor-dispatch-request
            (progn
              (apply (cdr rule) (coerce results 'list)))))))
    (not-found)))

#|(defmethod acceptor-status-message ((acceptor k-acceptor)
                                    http-status-code
                                    &rest properties
                                    &key &allow-other-keys)
  "Disable automatic error pages."
  (declare (ignore http-status-code properties))
  nil)|#

(defvar *acceptor* (make-instance 'k-acceptor
                                  :port 5000
                                  :access-log-destination nil
                                  :message-log-destination nil))

(defun page-header (&optional extra)
  (html
    (:div :id "header"
     (:a :id "logo" :href "/home" (:img :id "symbol" :src "/media/logo.png" :alt "kindista"))
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

(defun welcome-bar (content &optional (hide t))
  (html
    (:div :class "welcome"
      (when hide
        (htm
          (:form :method "post" :action "/settings"
            (:button :class "corner" :type "submit" :name "help" :value "0" "[ hide help text ]"))))
      (str content))))

(defun base-page (title body &key class)
  (html
    "<!DOCTYPE html>"
    (:html
      (:head
        (:title (str (if title (s+ title " | Kindista") "Kindista")))
        (:meta :charset "utf-8")
        (:meta :name "viewport" :content "width=device-width,initial-scale=1.0,maximum-scale=1.0")
        (:meta :name "HandheldFriendly" :content "True")
        ;(:meta :name "apple-mobile-web-app-status-bar-style" :content "black")
        (:link :rel "stylesheet" :href "/media/style.css")
        ;(str "<!--[if lt IE 9]>")
        ;(:link :rel "stylesheet" :href "/media/ie.css" :type "text/css")
        ;(str "<![endif]-->")
        )
      (:body :class class :onload "if(!location.hash){window.scrollTo(0,0);};document.body.className+=\" js\";"
        (str body)))))

(defun header-page (title header-extra body &key class)
  (base-page title
             (html
               (:a :id "top")
               (str (page-header header-extra))
               (str body))
             :class class))

(defun standard-page (title body &key selected top right search search-scope class)
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
                 (when right
                   (htm
                     (:div :id "right"
                      (str right))))
                 (:div :id "body"
                   (str body))

                 (:form :action "/search" :method "GET" :id "search"
                   (:strong "Search ")
                   (:select :name "scope"
                     (:option :value "all" :selected (when (or (not search-scope)
                                                                     (string= search-scope "all"))
                                                             "selected") "All")
                     (:option :value "resources" :selected (when (equalp search-scope "resources")
                                                                       "selected") "Resources")
                     (:option :value "requests" :selected (when (equalp search-scope "requests")
                                                                       "selected") "Requests")
                     (:option :value "people" :selected (when (equalp search-scope "people")
                                                                   "selected") "People"))
                   (:input :type "text" :name "q" :value search)
                   (:input :type "submit" :value "Search"))

                 (:div :id "menu"
                   (if *user*
                     (htm
                       (:table
                         (:tr
                           (:td :rowspan "2"
                            (:img :src (format nil "/media/avatar/~A.jpg" *userid*)))
                           (:td (:a :href (s+ "/people/" (username-or-id)) (str (getf *user* :name)))))
                         (:tr
                           (:td
                             (:a :href "/settings" "Settings")
                             " &nbsp;&middot;&nbsp; "
                             (:a :href "/logout" "Log out")))))
                     )

                   (str (menu (list '("Activity" "home")
                                    '("Resources" "resources")
                                    '("Requests" "requests")
                                    '("People" "people")
                                    '("Discussions" "discuss")
                                    ;'("Events" "events") 
                                    '("Help & Feedback" "help")
                                    (when (getf *user* :admin)
                                      '("Admin" "admin")))
                              selected))

                   (:div :id "fine-print-links"
                    (:a :href "/about" "about")
                    " &middot; "
                    (:a :href "/terms" "terms")
                    " &middot; "
                    (:a :href "/privacy" "privacy"))

                   (:a :class "dark" :href "#top"
                       "Back to the top")))
               :class (cond
                        ((and right class) (s+ "right " class))
                        (right "right")
                        (class class))))

(defun confirm-delete (&key url next-url (type "item") class text)
  (standard-page
    "Confirm Delete"
    (html
      (:h1 "Are you sure you want to delete this " (str type) "?")
      (when text
        (htm (:p (cl-who:esc text))))
      (:form :method "post" :action url
        (when next-url
          (htm (:input :type "hidden" :name "next" :value next-url)))
        (:button :class "yes" :type "submit" :class "submit" :name "really-delete" "Yes")      
        (:a :href next-url "No, I didn't mean it!")))
    :class class))

