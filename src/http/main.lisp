;;; Copyright 2012-2016 CommonGoods Network, Inc.
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

(setf hunchentoot:*show-lisp-backtraces-p* t)
(setf hunchentoot:*show-lisp-errors-p* t)

(defvar *client-errors-log-lock* (make-mutex :name "client errors log"))
(defvar *flashes* (make-hash-table :synchronized t :size 500 :rehash-size 1.25))

(defvar *base-url* "/")

(defun flash (message &key error)
  (with-locked-hash-table (*flashes*)
    (push
      (format nil
              (if error
                "<div class=\"flash err\"><span>~a</span></div>"
                "<div class=\"flash\"><span>~a</span></div>")
              message)
      (gethash *token* *flashes*))))

(defun flashes ()
  (with-locked-hash-table (*flashes*)
    (prog1
      (setf (gethash *token* *flashes*)
            (delete-duplicates (gethash *token* *flashes*) :test #'string=))
      (remhash *token* *flashes*))))

(defun not-found ()
  (flash "The page you requested could not be found." :error t)
  (if (equal (fourth (split "/" (referer) :limit 4)) (subseq (script-name*) 1))
    (see-other "/home")
    (see-other (or (referer) "/home"))))

(defun permission-denied (&optional next)
  (flash "The page you requested is private." :error t)
  (if (equal (fourth (split "/" (referer) :limit 4)) (subseq (script-name*) 1))
    (see-other "/home")
    (see-other (or next (referer) "/home"))))

(defun item-violates-terms ()
  (flash "This item violated Kindista's Terms of Use and has been deactivated." :error t)
  (if (equal (fourth (split "/" (referer) :limit 4)) (subseq (script-name*) 1))
    (see-other "/home")
    (see-other (or (referer) "/home"))))

(defun login-required ()
  (flash "The page you requested is only available when you are logged in to Kindista." :error t)
  (see-other (url-compose "/login" "next" (url-encode (request-uri*)))))

(defun test-users-prohibited ()
  (flash "Sorry, this functionality is not available for test users."
         :error t)
  (see-other "/home"))

(defun disregard-test-data ()
  (flash "Sorry, the data requested is for testing purposes only."
         :error t)
  (see-other "/home"))

(defun active-status-required ()
  (flash "Sorry, you must reactivate your account to perform that action." :error t)
  (if (equal (fourth (split "/" (referer) :limit 4)) (subseq (script-name*) 1))
    (see-other "/home")
    (see-other (or (referer) "/home"))))

(defun email-required ()
  (flash (html "You must have an email associated with your account to perform this action. "
               "Add an email address on your "
               (:a :href "/settings/communication#email" "settings page")
               " so that you can be notified when people respond to your post.")
         :error t)
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
          ((string= param-type "str")
           (setf pattern  "([a-zA-Z0-9_%+-=]+)"))
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

;; tokens

(defmacro with-token (&body body)
;;If the token was created less then 30 days ago, get the token, otherwise remove the token and start a new one.
;;But what happens to the cookie?
  `(let ((*token* (or *token* (check-token-cookie) (start-token))))
     ,@body))

(defun start-token (&aux (now (get-universal-time)))
  (with-locked-hash-table (*tokens*)
    (do (cookie-value)
      ((not (gethash (setf cookie-value (random-password 30)) *tokens*))
      (prog1
        (setf (gethash cookie-value *tokens*)
              (make-token :created now
                          :last-seen now))
        (set-cookie "token" :value cookie-value
                            :http-only t
                            :path "/"
                            :expires (+ now (* 12 +week-in-seconds+))
                            :secure nil))))))

(defun check-token (cookie-value &aux (now (get-universal-time)))
  (let ((token (gethash cookie-value *tokens*)))
    (if (and token (< now
                      (+ (or (token-last-seen token)
                             (token-created token))
                         (* 26 +week-in-seconds+))))
      (progn
        (setf (token-last-seen token) now)
        token)
      (progn (remhash cookie-value *tokens*) nil))))

(defun check-token-cookie ()
  (first (remove nil
                 (mapcar #'check-token
                         (mapcar #'cdr
                                 (remove "token" (cookies-in*)
                                          :key #'car
                                          :test #'string-not-equal))))))

(defun delete-token-cookie
  (&key (userid *userid*)
        (cookie (when *token* (cookie-in "token")))
        (token (or *token* (gethash cookie *tokens*))))

  (when cookie
    (when (gethash cookie *tokens*)
      (with-locked-hash-table (*user-tokens-index*)
        (asetf (gethash userid *user-tokens-index*)
               (remove token it :key #'cdr)))
      (remhash cookie *tokens*))
    (when (eql *token* token)
      (set-cookie "token" :value ""
                          :http-only t
                          :expires 0
                          :path "/"
                          :secure nil))))

(defun delete-all-but-current-token-cookie
  (&optional (userid *userid*)
             (token *token*))
  (dolist (pair (gethash userid *user-tokens-index*))
    (unless (eql (cdr pair) token)
      (delete-token-cookie :userid userid
                           :cookie (car pair)
                           :token (cdr pair)))))

(defun reset-token-cookie ()
  (awhen (cookie-in "token")
    (awhen (gethash it *tokens*)
      (remhash it *tokens*))
    (start-token)))

(defun too-many-cookies-p ()
  (let ((cookies (cookies-in*)))
    (> (length (remove "token" cookies :key #'car :test #'string-not-equal)) 1)))

(defmacro with-user (&body body)
  `(with-token
    (let* ((*userid* (or *userid* (token-userid *token*)))
           (*user* (or *user* (db *userid*)))
           (*user-group-privileges* (or *user-group-privileges*
                                         (gethash *userid* *group-privileges-index*)))
           (*user-mailbox* (or *user-mailbox*
                               (gethash *userid* *person-mailbox-index*)))
           (*fb-id* (getf *user* :fb-id))
           (*facebook-user-token* (getf *user* :fb-token))
           (*facebook-user-token-expiration* (getf *user* :fb-expires)))
      ,@body)))

(defmacro with-location (&body body)
  `(let ((*latitude* (or (getf *user* :lat) 44.028297))
         (*longitude* (or (getf *user* :long) -123.076065)))
     ,@body))

(defmacro with-donate-info (&body body)
  `(with-user
     (let ((*donate-info* (or (getf (token-session-data *token*) :donate-info)
                              (setf (getf (token-session-data *token*) :donate-info)
                                    (make-donate-info :address (getf *user* :street)
                                                      :city (getf *user* :city)
                                                      :state (getf *user* :state)
                                                      :email (car (getf *user* :emails))
                                                      :zip (getf *user* :zip)
                                                      :name (getf *user* :name))))))
       ,@body)))

(defmacro require-user
  ((&key require-active-user
         require-email
         allow-test-user)
   &body body)
  `(with-user
     (if *userid*
       (cond
         ((getf *user* :banned)
          (progn
            (flash "This account has been suspended for posting inappropriate content or otherwise violating Kindista's Terms of Use.  If you believe this to be an error please email us so we can resolve this issue." :error t)
            (get-logout)))

         ((and ,require-email
               (not (car (getf *user* :emails))))
          (email-required))

         ((and (getf *user* :test-user)
               (not ,allow-test-user))
          (test-users-prohibited))

         ((and ,require-active-user
               (not (getf *user* :active)))
          (active-status-required))

         (t (progn ,@body)))

       (login-required))))

(defmacro require-active-user (&body body)
  `(require-user (:require-active-user t)
     (progn ,@body)))

(defmacro require-admin (&body body)
  `(with-user
     (if (getf *user* :admin)
       (progn ,@body)
       (not-found))))

(defun server-side-request-p ()
  (or (string= (header-in* :x-real-ip) *local-ipv4-address*)
      (string= (header-in* :x-real-ip) *local-ipv6-address*)
      (string= (header-in* :x-real-ip) "127.0.0.1")))

(defmacro require-test ((test &optional message) &body body)
  `(if ,test
     (progn ,@body)
     ,(if message
        `(progn
           (flash ,message)
           (see-other "/")) 
        `(see-other "/"))))

(defun new-error-notice-handler ()
  (let ((data (cddddr *notice*)))
   (send-error-notification-email :on (getf data :on)
                                  :url (getf data :url)
                                  :data (getf data :data)
                                  :userid (getf data :userid))))
(defun client-side-error-logger
  (&aux
   (errorJSON (json:decode-json-from-string (raw-post-data :force-text t))))
  (require-user ()
    (with-mutex (*client-errors-log-lock*)
      (with-open-file (s (s+ +db-path+ "client-side-errors") :direction :output
                                                             :if-exists :append
                                                             :if-does-not-exist :create)
        (let ((*print-readably* nil))
          (format s "~{~S~%~}" errorJSON))))))

(defun markdown-file (path)
  (nth-value 1 (markdown (pathname path) :stream nil)))

(defun markdown-text (string)
  (nth-value 1 (markdown string :stream nil)))

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
            (let ((method (request-method*))
                  (timer (make-timer (lambda ()
                                       (error "timeout"))
                                     :thread *current-thread*)))
              (iter (for rule-method in (cadr rule) by #'cddr)
                    (for rule-function in (cdadr rule) by #'cddr)
                    (when (eq method rule-method)
                      (leave (with-user
                               (when (and (eq method :get) (too-many-cookies-p))
                                 ;; earlier code erroneously set multiple tokens that might need to be cleared
                                 (set-cookie "token" :value "" :expires 0 :path (script-name*)) )
                               (when *userid*
                                 (send-metric (acceptor-metric-system acceptor) :active *userid*))
                               (schedule-timer
                                 timer
                                 (cond
                                   ((and (or (server-side-request-p)
                                             (not *productionp*))
                                         (or (string= (script-name*) "/send-all-reminders")
                                             (string= (script-name*) "/inventory-refresh")
                                             (string= (script-name*) "/inventory-expiration-reminders")
                                             (string= (script-name*) "/send-inventory-digest")))
                                    60)
                                   ((and (getf *user* :admin)
                                         (string= (script-name*) "/admin/sendmail")) 600)
                                   ((string= (script-name*)
                                             "/settings/social")
                                    15)
                                   (t 10)))
                               (unwind-protect
                                 (apply (fdefinition rule-function) (coerce results 'list))
                                 (unschedule-timer timer)))))
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
          (:h1 "Something unexpected has happened")
          (:p "If this problem persists, please "
           (:a :href "mailto:errors@kindista.org" "email our development team")
           "."))))))

(defmethod acceptor-log-message ((acceptor k-acceptor) (log-level (eql :error)) format-string &rest format-arguments)
  "Print extra information when logging an error. Sends a formatted
  message to the destination denoted by (ACCEPTOR-MESSAGE-LOG-DESTINATION
  ACCEPTOR).  FORMAT and ARGS are as in FORMAT.  LOG-LEVEL is a keyword
  denoting the log level or NIL in which case it is ignored."
  (hunchentoot::with-log-stream (stream (acceptor-message-log-destination acceptor) hunchentoot::*message-log-lock*)
    (handler-case
      (flet ((error-message (destination)
               (format destination "~A [~A~@[ [~A]~]] USERID: ~A ~A ~:S ~:S ~?~%"
                       (get-universal-time)
                       (hunchentoot::iso-time)
                       log-level
                       (with-user *userid*)
                       (script-name*)
                       (get-parameters*)
                       (post-parameters*)
                       format-string
                       format-arguments)))
        (send-hunchentoot-error-notification-email (error-message nil))
        (error-message stream))
      (error (e)
        (ignore-errors
         (format *trace-output* "error ~A while writing to error log, error not logged~%" e))))))

(defun send-hunchentoot-error-notification-email (message)
  "Another function exists for sending specific errors within the codebase. See send-error-notification-email in email/error-notifications.lisp"
  (cl-smtp:send-email +mail-server+
                      "Kindista <noreply@kindista.org>"
                      *error-message-email*
                      "Kindista Error - Notifying Humans"
                      message))

(defmethod acceptor-log-access
  ((acceptor k-acceptor)
   &key return-code
   &aux (now (local-time:now))
        (today (local-time:timestamp-day now))
        (local-dir (with-output-to-string (str)
                     (format-timestring
                       str
                       now
                       :format '((:year 4) #\/ (:month 2) #\/))))
        (log-directory (ensure-directories-exist
                           (strcat +db-path+ "access-log/" local-dir)))
        (log-file (strcat log-directory today)))
  (unless (scan +bot-scanner+ (user-agent))
    (unless (string= (acceptor-access-log-destination acceptor) log-file)
      (setf (acceptor-access-log-destination acceptor) log-file))
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

(defun get-item-by-id
  (item-id
   &aux (id (parse-integer item-id))
        (item (db id)))
  (flet ((redirect (typestring &optional subpage)
           (see-other (strcat* "/" typestring "/" id subpage))))
    (case (getf item :type)
      (:person (redirect "people" "/reputation"))
      (:offer (redirect "offers"))
      (:request (redirect "requests"))
      (:event (redirect "events"))
      (:group (redirect "groups"))
      (:conversation (redirect "conversations"))
      (:transaction (redirect "transactions"))
      (:gratitude (redirect "gratitude"))
      (t (not-found)))))

(defun page-header (&optional extra)
  (html
    (:div :id "header"
     (:a :id "logo" :href (if *user* "/home" "/") (:img :id "symbol" :src "/media/logo.png" :alt "kindista"))
     ;(:h1 "kindista")
     (when extra
       (str extra)))))

(defun icon (type &optional alt)
  (html
    (:img :class "icon"
          :src (s+ "/media/icons/" type ".png")
          :alt (or alt " "))))

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

(defun base-page (title body &key class extra-head)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (html
    "<!DOCTYPE html>"
    (:html
      (:head
        (:title (str (if title (s+ title " | Kindista") "Kindista")))
        (:meta :charset "utf-8")
        (:meta :name "viewport" :content "width=device-width,initial-scale=1.0,maximum-scale=1.0")
        (:meta :name "HandheldFriendly" :content "True")
        (:meta :property "og:site_name" :content "Kindista Generosity Network")
        (:meta :property "og:locale" :content "en_US")
        (:meta :name "apple-mobile-web-app-capable" :content "yes") ;hide safari user interface
        ;(:meta :name "apple-mobile-web-app-status-bar-style" :content "black")
        (:link :rel "stylesheet" :href "/media/style.css")
        (:link :href "//fonts.googleapis.com/css?family=Varela+Round"
               :rel "stylesheet"
               :type "text/css")
        (:link :rel "manifest" :href "/manifest.json") ;link for android integration
        (:link :rel "apple-touch-icon" :sizes "76x76" :href "/media/icons/kindista_favicon_76.png")
        (:link :rel "apple-touch-icon" :sizes "120x120" :href "/media/icons/kindista_favicon_120.png")
        (:link :rel "apple-touch-icon" :sizes "152x152" :href "/media/icons/kindista_favicon_152.png")
        (:link :rel "apple-touch-icon" :sizes "180x180" :href  "/media/icons/kindista_favicon_180.png")
        (:script :type "text/javascript" :src "/kindista.js")
        (:script :type "text/javascript" :src "/service-worker-registration.js?v=1.0") ;add query version number to ensure cache busting
        (when (and *userid* (or (string= (referer) (s+ +base-url+ "login"))
                                (string= (referer) "http://localhost/login")))
          (htm (:script :type "text/javascript"
                        :src "/update-push-registration.js")))
        ;; if serviceworker js, inline login subscription here

        ;(str "<!--[if lt IE 9]>")
        ;(:link :rel "stylesheet" :href "/media/ie.css" :type "text/css")
        ;(str "<![endif]-->")
        (str extra-head))
      (:body :class class :onload "if(!location.hash){window.scrollTo(0,0);};document.body.className+=\" js\";"
        (str body)))))

(defun login-box (&optional (mobile t))
  (html
    (:div :class (s+ (when mobile "mobile ") "login item")
      (:form :method "POST" :action "/login"
        (:label "Email"
          (:input :type "text"
                  :class "username"
                  :name "username"
                  :value (get-parameter "retry")))
        (:label "Password"
          (:input :type "password"
                  :class "password"
                  :name "password"))
         (awhen (get-parameter-string "next")
          (htm (:input :type "hidden" :name "next" :value it)))
        (:button :type "submit" :class "yes" "Log in")
        (:a :href "/reset" :class "reset" "Forgot your password?")))))

(defun header-page (title header-extra body &key class hide-menu extra-head)
  (base-page title
             (html
               (unless hide-menu (htm (:a :id "top")))
               (str (page-header header-extra))
               (str body))
             :class (s+ class (when hide-menu " hide-menu"))
             :extra-head extra-head))

(defun standard-page (title body &key selected top right search search-scope class extra-head)
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
                   (:input :type "submit" :class "blue" :value "Search"))

                 (:div :id "menu"
                   (if *user*
                     (let ((link (s+ "/people/" (username-or-id))))
                       (htm
                         (:table
                           (:tr
                             (:td :rowspan "2"
                               (if (getf *user* :avatar)
                                 (htm
                                   (:a :href link
                                     (:img :src (get-avatar-thumbnail *userid* 100 100 :filetype "png") :alt "")))
                                 (htm
                                   (:div :class "profile-pic small"
                                     (:img :src (get-avatar-thumbnail *userid* 100 100 :filetype "png") :alt "")
                                     (str (add-profile-picture-prompt))))))
                             (:td (:a :href link (str (getf *user* :name)))))
                           (:tr
                             (:td
                               (:a :class (when (eq selected :settings) "selected") :href "/settings" "Settings")
                               " &middot; "
                               (:a :href "/logout" :id "logout" "Log&nbsp;out"))))))
                     (str (login-box nil)))

                   (when *user*
                     (let ((inbox-count (new-inbox-items)))
                       (str (menu (list `("Messages" "messages" ,(when (> inbox-count 0) inbox-count)))
                                  selected))))

                   (str (menu (list '("Community" "home")
                                    '("Offers" "offers")
                                    '("Requests" "requests")
                                    '("People" "people")
                                    '("Groups" "groups")
                                    '("Events" "events")
                                    '("Help & Feedback" "faq")
                                    (when (getf *user* :admin)
                                      '("Admin" "admin")))
                              selected))

                   (:menu :id "fine-print-links"
                          :type "toolbar"
                     (:li (:a :href "/blog" "blog"))
                     (:li (:a :href "/about" "about"))
                     (:li (:a :href "/terms" "terms"))
                     (:li (:a :href "/privacy" "privacy")))

                   (:a :class "dark" :href "#top"
                       "Back to the top")))
               :extra-head extra-head
               :class (cond
                        ((and right class) (s+ "right " class))
                        (right "right")
                        (class class))))

(defun confirm-delete (&key url next-url (type "item") confirmation-question class text image-id item-id inappropriate-item)
  (standard-page
    "Confirm Delete"
    (html
      (:h2
        (str
          (or confirmation-question
              (s+ "Are you sure you want to delete this " type "?"))))
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
            (:textarea :cols 300 :rows 5 :name "explanation" :id "explanation")
            (:h3 "The automatically generated response is shown below. "
                 "If you include an explanation, it will be inserted after the first paragraph.")
            (:div
              (str (html-text (deleted-invalid-item-reply-text (db (db item-id :by) :name)
                                                               (getf *user* :name)
                                                               type))))))
        (:a :href next-url "No, I didn't mean it!")
        (:button :class "yes" :type "submit" :class "submit" :name "really-delete" "Yes")))
    :class class))

(defun confirm-action (title text &key url next-url class item-id details (post-parameter "confirm-action") fine-print button-text)
  (standard-page
    title
    (html
      (:div :class class
        (:h2 (str text))
        (when details
          (htm (:div :class "details" (str details))))
        (when fine-print
          (htm (:div :class "fine-print" (str fine-print))))
        (:form :method "post" :action url :class "item confirm-delete"
          (awhen item-id
            (htm (:input :type "hidden" :name "item-id" :value it)))
          (awhen next-url
            (htm (:input :type "hidden" :name "next" :value it)))
          (:a :href next-url "No, I didn't mean it!")
          (:button :class "yes" :type "submit" :class "submit" :name post-parameter (str (or button-text "Yes"))))))))

