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

(setf hunchentoot:*show-lisp-backtraces-p* nil)
(setf hunchentoot:*show-lisp-errors-p* nil)

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
      (delete-duplicates (gethash *token* *flashes*) :test #'string=)
      (remhash *token* *flashes*))))

(defun not-found ()
  (flash "The page you requested could not be found." :error t)
  (if (equal (fourth (split "/" (referer) :limit 4)) (subseq (script-name*) 1))
    (see-other "/home")
    (see-other (or (referer) "/home"))))

(defun permission-denied ()
  (flash "The page you requested is private." :error t)
  (if (equal (fourth (split "/" (referer) :limit 4)) (subseq (script-name*) 1))
    (see-other "/home")
    (see-other (or (referer) "/home"))))

(defun login-required ()
  (flash "The page you requested is only available when you are logged in to Kindista." :error t)
  (see-other (script-name*)))

(defun active-status-required ()
  (flash "Sorry, you must reactivate your account to perform that action." :error t)
  (if (equal (fourth (split "/" (referer) :limit 4)) (subseq (script-name*) 1))
    (see-other "/home")
    (see-other (or (referer) "/home"))))

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


(defmacro routes (&body routes)
  `(setf *routes* (list ,@(iter (for route in routes)
                                (collect `(list (make-path-scanner ,(first route))
                                                (quote ,(rest route))))))))

; }}}

;; globals


;; special variables

(defvar *token* nil) ; current token (session)
(defvar *donate-info* nil) ; current donation page data
(defvar *user* nil) ; current user
(defvar *userid* nil) ; current user
(defvar *user-group-privileges* nil) ; current user's group privileges
(defvar *user-mailbox* nil) ;current user's mailbox
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

(defun password-match-p (id password)
  (let ((crypted-password (db id :pass)))
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
           (*user* (or *user* (db *userid*)))
           (*user-group-privileges* (or *user-group-privileges*
                                         (gethash *userid* *group-privileges-index*)))
           (*user-mailbox* (or *user-mailbox*
                               (gethash *userid* *person-mailbox-index*))))
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
                                                      :email (car (getf *user* :emails))
                                                      :zip (getf *user* :zip)
                                                      :name (getf *user* :name))))))
       ,@body)))

(defmacro require-user (&body body)
  `(with-user
     (if *userid*
       (if (getf *user* :banned)
         (progn
           (flash "This account has been suspended for posting inappropriate content or otherwise violating Kindista's Terms of Use.  If you believe this to be an error please email us so we can resolve this issue." :error t)
           (get-logout))
         (progn ,@body))
       (login-required))))

(defmacro require-active-user (&body body)
  `(with-user
     (if *userid*
       (if (eq (getf *user* :active) t) 
         (progn ,@body)
         (active-status-required))
       (login-required))))

(defmacro require-admin (&body body)
  `(with-user
     (if (getf *user* :admin)
       (progn ,@body)
       (not-found))))

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
  (nth-value 1 (markdown (pathname path) :stream nil)))

(defun see-other (url)
  (setf (return-code*) +http-see-other+)
  (setf (header-out :location) url)
  "")

(defun moved-permanently (url)
  (setf (return-code*) +http-moved-permanently+)
  (setf (header-out :location) url)
  "")

(defclass k-acceptor (acceptor)
  ((metric-system :initform (make-instance 'metric-system)
                  :reader acceptor-metric-system)))

(defmethod acceptor-dispatch-request ((acceptor k-acceptor) request)
  (with-token
    (dolist (rule *routes*)
      (multiple-value-bind (match results)
          (scan-to-strings (car rule) (script-name*))
        (when match
          (return-from acceptor-dispatch-request
            (let ((method (request-method*)))
              (iter (for rule-method in (cadr rule) by #'cddr)
                    (for rule-function in (cdadr rule) by #'cddr)
                    (when (eq method rule-method)
                      (leave (with-user
                               (when *userid*
                                 (send-metric (acceptor-metric-system acceptor) :active *userid*))
                               (apply (fdefinition rule-function) (coerce results 'list)))))
                    (finally
                      (setf (return-code*) +http-method-not-allowed+)
                      "that method is not permitted on this URL")))))))
    (not-found)))

(defmethod acceptor-status-message ((acceptor k-acceptor)
                                    http-status-code
                                    &rest properties
                                    &key &allow-other-keys)
  "Disable automatic error pages."
  (declare (ignore http-status-code properties))
  (if *show-errors*
    (call-next-method)
    (base-page
      "Error"
      (html
        (str (page-header))
        (:div :id "site-error"
          (:h1 "Something terrible happened")
          (:p "Humans have been notified!"))))))

(defmethod acceptor-log-message ((acceptor k-acceptor) (log-level (eql :error)) format-string &rest format-arguments)
  "Print extra information when logging an error. Sends a formatted
  message to the destination denoted by (ACCEPTOR-MESSAGE-LOG-DESTINATION
  ACCEPTOR).  FORMAT and ARGS are as in FORMAT.  LOG-LEVEL is a keyword
  denoting the log level or NIL in which case it is ignored."
  (hunchentoot::with-log-stream (stream (acceptor-message-log-destination acceptor) hunchentoot::*message-log-lock*)
    (handler-case
      (flet ((error-message (destination)
               (format destination "[~A~@[ [~A]~]] ~A ~A ~:S ~:S ~?~%"
                       (hunchentoot::iso-time)
                       log-level
                       *userid*
                       (script-name*)
                       (get-parameters*)
                       (post-parameters*)
                       format-string
                       format-arguments)))
        (send-error-notification-email (error-message nil))
        (error-message stream))
      (error (e)
        (ignore-errors
         (format *trace-output* "error ~A while writing to error log, error not logged~%" e))))))

(defun send-error-notification-email (message)
  (cl-smtp:send-email +mail-server+
                      "Kindista <noreply@kindista.org>"
                      *error-message-email*
                      "Kindista Error - Notifying Humans"
                      message))

(defmethod acceptor-log-access ((acceptor k-acceptor) &key return-code)
  (unless (scan +bot-scanner+ (user-agent))
    (hunchentoot::with-log-stream (stream (acceptor-access-log-destination acceptor) hunchentoot::*access-log-lock*)
      (format stream "~:[-~@[ (~A)~]~;~:*~A~@[ (~A)~]~] ~:[-~;~:*~A~] [~A] \"~A ~A~@[?~A~] ~
                      ~A\" ~D ~:[-~;~:*~D~] \"~:[-~;~:*~A~]\" \"~:[-~;~:*~A~]\"~%"
              (header-in* :x-real-ip)
              (header-in* :x-forwarded-for)
              (authorization)
              (hunchentoot::iso-time)
              (request-method*)
              (script-name*)
              (query-string*)
              (server-protocol*)
              return-code
              (content-length*)
              (referer)
              (user-agent)))))

(defvar *acceptor* (make-instance 'k-acceptor
                                  :port 5000
                                  :address "127.0.0.1"
                                  :taskmaster (make-instance 'single-threaded-taskmaster)
                                  :access-log-destination (s+ +db-path+ "access")
                                  :message-log-destination (s+ +db-path+ "errors")))

(defun page-header (&optional extra)
  (html
    (:div :id "header"
     (:a :id "logo" :href (if *user* "/home" "/") (:img :id "symbol" :src "/media/logo.png" :alt "kindista"))
     ;(:h1 "kindista")
     (when extra
       (str extra)))))

(defun icon (type)
  (html
    (:img :class "icon" :src (s+ "/media/icons/" type ".png") :alt " ")))

(defun menu-item (title slug &optional selected count)
  (html
    (:li :class (when selected "selected")
      (:a :href (s+ "/" slug)
          (str (icon slug))
          (str title)
          (when count
            (htm (:span :class "newcount" (str count))))))))

(defun menu (items &optional selected)
  (html
    (:menu :type "toolbar"
      (iter (for (title slug count) in items)
            (when (and title slug)
              (str (menu-item title
                              slug
                              (string= selected slug)
                              count)))))))

(defun welcome-bar (content &optional (hide t))
  (html
    (:div :class "welcome"
      (when hide
        (htm
          (:form :method "post" :action "/settings"
            (:button :class "corner" :type "submit" :name "help" :value "0" "[ hide help text ]"))))
      (str content))))

(defun base-page (title body &key class)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
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
        (:script :type "text/javascript" :src "/kindista.js")
        ;(str "<!--[if lt IE 9]>")
        ;(:link :rel "stylesheet" :href "/media/ie.css" :type "text/css")
        ;(str "<![endif]-->")
        )
      (:body :class class :onload "if(!location.hash){window.scrollTo(0,0);};document.body.className+=\" js\";"
        (str body)))))

(defun login-box (&optional (mobile t))
  (html
    (:div :class (s+ (when mobile "mobile ") "login item")
      (:form :method "POST" :action "/login" :id "login"
        (:label :for "username" "Email")
        (:input :type "text"
                :id "username"
                :name "username"
                :value (get-parameter "retry"))
        (:label :for "password" "Password")
        (:input :type "password"
                :id "password"
                :name "password")
        (:button :type "submit" :class "yes" "Log in")
        (:a :href "/reset" "Forgot your password?")))))

(defun header-page (title header-extra body &key class)
  (base-page title
             (html
               (:a :id "top")
               (str (page-header header-extra))
               (str body))
             :class class))

(defun standard-page (title body &key selected top right search search-scope class)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (header-page title
               (html
                 (:div
                   (:a :href "#search" (:img :alt "Search" :src "/media/icons/search.png"))
                   (:div :class "message-icon"
                    (:a :href "/messages"
                     (:img :alt "Messages" :src "/media/icons/messages.png")
                      (let ((inbox-count (new-inbox-items)))
                        (when (> inbox-count 0)
                         (htm
                           (:span :class "newcount" (str inbox-count)))))))

                   (:a :href "#menu" (:img :alt "Menu" :src "/media/icons/menu.png"))))
               (html
                 (dolist (flash (flashes))
                   (str flash))
                 (unless *user*
                   (str (login-box t)))
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
                   ;(:strong "Search ")
                   (:select :name "scope"
                     (:option :value "all" 
                              :selected (when (or (not search-scope)
                                                  (string= search-scope "all"))
                                          "selected")
                              "All")
                     (:option :value "offers"
                              :selected (when (equalp search-scope "offers") "selected")
                              "Offers")
                     (:option :value "requests"
                              :selected (when (equalp search-scope "requests") "selected")
                              "Requests")
                     (:option :value "events"
                              :selected (when (equalp search-scope "events") "selected")
                              "Events")
                     (:option :value "people"
                              :selected (when (equalp search-scope "people") "selected")
                              "People")
                     (:option :value "groups"
                              :selected (when (equalp search-scope "groups") "selected")
                              "Groups"))
                   (:input :type "text" :size "14" :name "q" :value (awhen search
                                                                      (escape-for-html it)))
                   (:input :type "submit" :value "Search"))

                 (:div :id "menu"
                   (if *user*
                     (let ((link (s+ "/people/" (username-or-id))))
                       (htm
                         (:table
                           (:tr
                             (:td :rowspan "2"
                               (:a :href link (:img :src (get-avatar-thumbnail *userid* 100 100 :filetype "png")
                                                    :alt "")))
                             (:td (:a :href link (str (getf *user* :name)))))
                           (:tr
                             (:td
                               (:a :class (when (eq selected :settings) "selected") :href "/settings" "Settings")
                               " &middot; "
                               (:a :href "/logout" "Log&nbsp;out"))))))
                     (str (login-box nil)))

                   (when *user*
                     (let ((inbox-count (new-inbox-items)))
                       (str (menu (list `("Messages" "messages" ,(when (> inbox-count 0) inbox-count)))
                                  selected))))

                   (str (menu (list '("News" "home")
                                    '("Offers" "offers")
                                    '("Requests" "requests")
                                    '("People" "people") 
                                    '("Groups" "groups") 
                                    '("Events" "events") 
                                    '("Help & Feedback" "faq")
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

(defun confirm-delete (&key url next-url (type "item") class text image-id item-id inappropriate-item)
  (standard-page
    "Confirm Delete"
    (html
      (:h2 "Are you sure you want to delete this " (str type) "?")
      (when text
        (htm (:p
               (:blockquote :class "review-text " (cl-who:esc text)))))
      (when image-id
        (htm (:img :class "activity-image"
                   :src (get-image-thumbnail image-id 300 300)
                   :alt type)))
      (:form :method "post" :action url :class "item confirm-delete"
        (awhen item-id
          (htm (:input :type "hidden" :name "item-id" :value it)))
        (awhen next-url
          (htm (:input :type "hidden" :name "next" :value it)))
        (when inappropriate-item
          (htm
            (:input :type "hidden" :name "delete-inappropriate-item")
            (:label :for "explanation"
             "Do you want to include a comment or explanation about deleting this item (optional)?")
            (:textarea :cols 300 :rows 5 :name "explanation" :id "explanation")))
        (:a :href next-url "No, I didn't mean it!")
        (:button :class "yes" :type "submit" :class "submit" :name "really-delete" "Yes")))
    :class class))

(defun confirm-action (title text &key url next-url class item-id details (post-parameter "confirm-action"))
  (standard-page
    title
    (html
      (:div :class class
        (:h2 (str text))
        (when details
          (htm (:div (str details))))
        (:form :method "post" :action url :class "item confirm-delete"
          (awhen item-id
            (htm (:input :type "hidden" :name "item-id" :value it)))
          (awhen next-url
            (htm (:input :type "hidden" :name "next" :value it)))
          (:a :href next-url "No, I didn't mean it!")
          (:button :class "yes" :type "submit" :class "submit" :name post-parameter "Yes"))))))

