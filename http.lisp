(in-package :kindista)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(setf (cl-who:html-mode) :sgml)

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

(defclass k-acceptor (acceptor) ())

(defmethod acceptor-dispatch-request ((acceptor k-acceptor) request)
  (dolist (rule *routes*)
    (multiple-value-bind (match results)
        (scan-to-strings (car rule) (script-name*))
      (when match
        (return-from acceptor-dispatch-request
          (progn
            (apply (cdr rule) (coerce results 'list)))))))

  (setf (return-code*) +http-not-found+)
  "<h1>Not found</h1>")

#|(defmethod acceptor-status-message ((acceptor k-acceptor)
                                    http-status-code
                                    &rest properties
                                    &key &allow-other-keys)
  "Disable automatic error pages."
  (declare (ignore http-status-code properties))
  nil)|#

(defvar *acceptor* (make-instance 'k-acceptor
                                  :port 5000))

; }}}

;; globals


;; special variables

(defvar *user* nil) ; current user
(defvar *userid* nil) ; current user

(defun run ()
  (load-db)
  (load-tokens)
  (start *acceptor*))

(defmacro json (&rest properties)
  `(progn
     (setf (content-type*) "application/json")
     (json:encode-json-plist-to-string (list ,@properties))))

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
      (loop for item in items
            do (str (menu-item (car item)
                               (cadr item)
                               (or (caddr item) (cadr item))
                               (string= selected (cadr item))))))))

(defun base-page (title body &key class)
  (html
    "<!DOCTYPE html>"
    (:html
      (:head
        (:title (if title (str (s+ title " | Kindista")) "Kindista"))
        (:meta :name "viewport" :content "width=device-width,initial-scale=1.0,maximum-scale=1.0")
        ;(:meta :name "apple-mobile-web-app-status-bar-style" :content "black")
        (:link :rel "stylesheet" :href "/media/style.css")
        (str "<!--[if lt IE 9]>")
        (:link :rel "stylesheet" :href "/media/ie.css" :type "text/css")
        (str "<![endif]-->"))
      (:body :class class :onload "window.scrollTo(0,0);"
        (:a :name "top")
        (str body)))))

(defun header-page (title header-extra body &key class)
  (base-page title
             (html
               (str (page-header header-extra))
               (str body))
             :class class))

(defun standard-page (title body &key (selected "news") top right)
  (header-page title
               (html
                 (:div
                   (:a :href "#search" (:img :alt "Search" :src "/media/icons/search.png"))  
                   " "
                   (:a :href "#menu" (:img :alt "Menu" :src "/media/icons/menu.png"))))
               (html
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

                   (str (menu '(("Home" "news" "")
                                ("Messages" "messages")
                                ("People" "people")
                                ("Offers" "offers")
                                ("Requests" "requests")
                                ("Events" "events")
                               )
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


;;; routes {{{

(defroute "/" ()
  (:get
    (with-user
      (if *user*
        (see-other "/home")
        (base-page nil
                 (html
                   (:img :id "biglogo" :src "/media/biglogo.png")
                   (:form :method "POST" :action "/login" :id "login"
                     (awhen (get-parameter "retry")
                       (htm (:p :class "error" "The username or password was incorrect.")))
                     (awhen (get-parameter "next")
                       (htm (:input :type "hidden" :name "next" :value it)))
                     (:label :for "username" "Username or email")
                     (:input :type "text" :name "username")
                     (:label :for "password" "Password")
                     (:input :type "password" :name "password")
                     (:input :type "submit" :value "Log in")
                     (:p (:a :href "/reset" "Forgot your password?"))
                     (:p "New to Kindista?"
                      (:br)
                      (:a :href "/signup" "Create an account")))
                   (:div :id "about"
                    (:h2 "Co-creating a more beautiful world.")
                    (:p :class "big"
                      "Kindista is a new social network for seeing and appreciating the
                       creative potential in all people and supporting each other
                       in building the more beautiful world our hearts know is possible."))
                  (:p "Kindista &copy; 2012 &middot; "
                      (:a :href "/help" "Help") " &middot; "
                      (:a :href "/about" "About") " &middot; "
                      (:a :href "/blog" "Blog")
                      " &middot; Programmed in Common Lisp"))
                 :class "landing")))))

(defun signup-page (&key error name email password)
  (header-page
    "Sign up"
    nil
    (html
      (:h1 "Create an account")
      (when error
        (htm (:p :class "error" (str error))))
      (:form :method "POST" :action "/signup"
        (:div :id "login"
          (:label :for "name" "Full name")
          (:input :type "text" :name "name" :value name)
          (:label :for "email" "Email")
          (:input :type "email" :name "email" :value email)
          (:label :for "name" "Password")
          (:input :type "password" :name "password" :value password))
        (:input :type "submit" :value "Sign up"))

      (:p "Have an account? " (:a :href "/" "Sign in"))

      (:p :class "fineprint" "By creating an account, you are agreeing to our "
        (:a :href "/terms" "Terms of Service") " and " (:a :href "/privacy" "Privacy Policy"))

      (:menu :id "footer"
        (:li (:a :href "/help" "Help"))
        (:li (:a :href "/about" "About"))
        (:li (:a :href "/blog" "Blog"))
       )
      (:br)
      (:br)
      " ")
    :class "landing"))

(defroute "/signup" ()
  (:get
    (with-user
      (if *user*
        (see-other "/home")
        (signup-page))))
  (:post
    (with-user
      (if *user*
        (see-other "/home")
        (let ((userid (gethash (post-parameter "email") *email-index*)))
          (cond
            ((password-match-p userid (post-parameter "password"))

             (set-cookie "token" :value (make-token username)
                         :http-only t
                         :expires (+ (get-universal-time) 2592000)
                         :secure nil)
             (see-other "/home"))

            (userid
             (see-other (s+ "/forgot?email=" (post-parameter "email"))))

            ((not (and (< 0 (length (post-parameter "name")))
                       (< 0 (length (post-parameter "email")))
                       (< 0 (length (post-parameter "password")))))

             (signup-page :error "All fields are required"
                          :name (post-parameter "name")
                          :email (post-parameter "email")
                          :password (post-parameter "password")))

            ((> 7 (length (post-parameter "password")))

             (signup-page :error "Please use a strong password"
                          :name (post-parameter "name")
                          :email (post-parameter "email")))

            ((not (find #\Space (post-parameter "name")))

             (signup-page :error "Please use your full name"
                          :name (post-parameter "name")
                          :email (post-parameter "email")
                          :password (post-parameter "password")))

            (t
             (let ((id (create-person :name (post-parameter "name")
                                      :email (post-parameter "email")
                                      :password (post-parameter "password"))))
               (set-cookie "token"
                           :value (make-token id)
                           :http-only t
                           :expires (+ (get-universal-time) 2592000)
                           :secure nil))
             (see-other "/home"))))))))

(defroute "/logout" ()
  (:get
    (delete-token-cookie)
    (see-other "/")))

(defroute "/login" ()
  (:get
    (see-other "/"))
  (:post
    (let ((user (post-parameter "username"))
          (next (post-parameter "next")))
      (if (find #\@ user :test #'equal)
        (setf user (gethash user *email-index*))
        (setf user (gethash user *username-index*)))
      (cond
        ((password-match-p user (post-parameter "password"))
         (set-cookie "token"
                     :value (make-token user)
                     :http-only t
                     :expires (+ (get-universal-time) 2592000)
                     :secure nil)
         (setf (return-code*) +http-see-other+)
         (setf (header-out :location) (if (and (< 0 (length next))
                                               (equal #\/ (elt next 0)))
                                        next
                                        "/home"))
         "")
        (t
         (setf (return-code*) +http-see-other+)
         (setf (header-out :location) (if (and (< 0 (length next))
                                               (equal #\/ (elt next 0)))
                                        (s+ "/?retry&next=" (url-encode next))
                                        "/?retry"))
         "")))))


(defun timestamp (time)
  (html (:h3 :class "timestamp" :data-time time (str (humanize-universal-time time)))))

(defun love-button (url)
  (html
    (:form :method "POST" :action url
      (:input :type "submit" :name "like" :value "Love"))))

(defun comment-button (url)
  (html
    (:form :method "GET" :action (s+ url "/comments")
      (:input :type "submit" :value "Discuss"))))

(defun flag-button (url &optional next-url)
  (html
    (:form :method "GET" :action (s+ url "/flag")
      (:input :type "hidden" :name "next" :value next-url)
      (:input :type "submit" :value "Flag"))))


(defroute "/home" ()
  (:get
    (require-user
      (standard-page
        "Home"
        (html
          (:div :class "activity"
            (:div :class "item"
              (:menu :class "horiz"
                (:strong "create")
                (:li (:a :href "/testimonials/compose" "testimonial"))
                (:li (:a :href "/offers/compose" "offer"))
                (:li (:a :href "/requests/compose" "request"))
                (:li (:a :href "/events/compose" "event"))
                (:li (:a :href "/announcements/compose" "announcement"))
                )
              )
            (str (activity-items))))
         :right (html
                  (:div :class "item"
                    (:h2 "Upcoming Events")
                    (:menu
                      (:li "10/20 7:00PM " (:a :href "x" "East Eugene Gift Circle"))
                      (:li "10/24 7:00PM " (:a :href "x" "West Eugene Gift Circle")))))))))

(defroute "/messages" ()
  (:get
    (require-user
      (standard-page
        "Messages"
        (html
          (:h1 "Messages")
          (:h2 "Ideas")
          (:ul
            (:li "separate pages for message-only inbox and notification-only inbox?")
            ))
        :selected "messages"))))

(defroute "/people" ()
  (:get
    (require-user
      (standard-page
        "People"
        (html
          (:h1 "People")
          (:h2 "Ideas")
          (:ul
            (:li "suggested friends")
            (:li "nearby people, weight given to people w/ mutual friends")
            ))
        :selected "people"))))

(defroute "/offers" ()
  (:get
    (require-user
      (standard-page
        "Offers"
        (html
          (:h1 "Offers")
          (:h2 "Ideas")
          (:ul
            ))
        :selected "offers"))))

(defroute "/requests" ()
  (:get
    (require-user
      (standard-page
        "Requests"
        (html
          (:h1 "Requests")
          (:h2 "Ideas")
          (:ul
            ))
        :selected "requests"))))

(defroute "/events" ()
  (:get
    (require-user
      (standard-page
        "Events"
        (html
          (:h1 "Events")
          (:h2 "Ideas")
          (:ul
            (:li "create a new event")
            (:li "upcoming events")
            (:li "events friends are going to")
            ))
        :selected "events"))))

(defroute "/people/<name>" (name)
  (:get
    (require-user
      (standard-page
        "Home"
        (html
          (:div :class "profile"
            (:div :class "activity"
              (str (offer-activity-item
                :time (get-universal-time)
                :user-name "Benjamin Crandall"
                :user-id "ben"
                :offer-id "12345"
                :next-url "/home"
                :hearts 3
                :comments 7
                :text "[google](http://google.com) Saxophone lessons. I am **conservatory trained** (Bachelor of Music in Jazz Saxophone Performance from the CCM at University of Cincinnati). I have been playing for over 20 years, performing professionally in a reggae band (JohnStone Reggae in Washington DC and multiple jazz ensembles (currently StoneCold Jazz in Eugene.)"))
              (str (offer-activity-item
                :time (get-universal-time)
                :user-name "Benjamin Crandall"
                :user-id "ben"
                :offer-id "12345"
                :next-url "/home"
                :hearts 3
                :comments 7
                :text "[google](http://google.com) Saxophone lessons. I am **conservatory trained** (Bachelor of Music in Jazz Saxophone Performance from the CCM at University of Cincinnati). I have been playing for over 20 years, performing professionally in a reggae band (JohnStone Reggae in Washington DC and multiple jazz ensembles (currently StoneCold Jazz in Eugene.)")))
            ))
        :top (html
               (:div :class "profile"
               (:img :src "/media/oldeamon.jpg") 
               (:div :class "basics"
                 (:h1 "Eamon Walker")
                 (:p :class "city" "Eugene, OR")
               (:form :method "GET" :action "/people/eamon/message"
                 (:input :type "submit" :value "Send a message")) 
               (:form :method "POST" :action "/friends"
                 (:input :type "hidden" :name "add" :value "eamon")
                 (:input :type "hidden" :name "next" :value "/people/eamon")
                 (:input :type "submit" :value "Add as friend")))
               (:p :class "bio" "Kindista co-creator. I am committed to living fully in gift, which means that I don't charge for anything. If you appreciate what I do, please support me! xxxxxx")

               (:menu :class "bar"
                 (:h3 :class "label" "Profile Menu")
                 (:li :class "selected" "Activity")
                 (:li (:a :href "/people/eamon/resources" "Resources"))
                 (:li (:a :href "/people/eamon/testimonials" "Testimonials"))
                 (:li (:a :href "/people/eamon/blog" "Blog"))
                 (:li :class "notonly" (:a :href "/people/eamon/friends" "Mutual Friends")))))
        :right (html
                 (:div :class "item people"
                  (:h3 "Mutual Friends")
                  (:ul
                    (:li (:a :href "/people/eamon" "Eamon Walker"))
                    (:li (:a :href "/people/eamon" "Eamon Walker"))
                    (:li (:a :href "/people/eamon" "Eamon Walker"))
                    (:li (:a :href "/people/eamon" "Eamon Walker"))
                    (:li (:a :href "/people/eamon/friends" "and xx more")))))

        :selected "people"))))

(defun parse-subject-list (subject-list &key remove)
  (delete-duplicates
    (iter (for subject in (split #\, subject-list))
          (unless (equalp subject remove)
            (acond
              ((scan +number-scanner+ subject)
               (setf subject (parse-integer subject))
               (awhen (db subject)
                 (when (or (eq (getf it :type) :person)
                           (eq (getf it :type) :project))
                   (collect subject at beginning))))
              ((gethash subject *username-index*)
               (collect it at beginning))
              ((gethash subject *email-index*)
               (collect it at beginning)))))))

(defun names-list (id-list)
  (iter (for id in id-list)
        (collect (cons id (getf (db id) :name)) at beginning)))

(defun testimonial-compose (&key subjects text next)
  (if subjects
    (standard-page
     "Compose a Testimonial"
     (html
       (:div :class "item"
        (:h2 "Compose a Testimonial"))
       (:div :class "item"
        (:form :method "post" :action "/testimonials/compose" :class "recipients"
          (:label "About:")
          (:menu :class "recipients"
           (unless subjects
             (htm (:li (:em "nobody yet"))))
           (dolist (subject subjects)
             (htm
               (:li (str (second subject)) (:button :class "text large" :type "submit" :name "remove" :value (first subject) " ⨯ ")))))
          (when subjects
            (htm (:input :type "hidden" :name "subject" :value (format nil "~{~A~^,~}" (mapcar #'car subjects)))))
          (when next
            (htm (:input :type "hidden" :name "next" :value next)))

          (:p (:button :type "submit" :class "text" :name "add" :value "new" "+ Add a person or project"))
          (:textarea :rows "8" :name "text" (str text))
          (:p  (:input :type "submit" :class "cancel" :name "cancel" :value "Cancel")
          (:input :type "submit" :class "submit" :name "create" :value "Create")))))
     :selected "people")
    (testimonial-add-subject :text text :next next)))

(defun friends-alphabetically (&optional (user *user*))
  (sort (iter (for friend in (getf user :friends))
              (collect (list friend (getf (db friend) :name))))
        #'string-lessp :key #'cadr))

(defun testimonial-add-subject (&key subjects text next (results 'none))
  (standard-page
    "Compose a Testimonial"
    (html
      (:div :class "item"
       (:h2 "Who would you like to write about?")
       (:h3 "Search for a person or project")
       (:form :method "post" :action "/testimonials/compose"
         (:input :type "text" :name "name")
         (:input :type "submit" :class "submit" :name "search" :value "Search")

         (if (eq results 'none)
           (progn
             (htm
               (:h3 "Select one of your friends")
               (:menu
                 (dolist (friend (friends-alphabetically *user*))
                   (htm (:li (:button :class "text" :type "submit" :value (car friend) :name "add" (str (cadr friend)))))))))
           (progn
             (htm
               (:h3 "Search results")
               (:menu
                 (dolist (result results)
                   (htm (:li (:button :class "text" :type "submit" :value (car result) :name "add" (str (cadr result))))))))))

         (:input :type "submit" :class "cancel" :value "Back")

         (when subjects
           (htm (:input :type "hidden" :name "subject" :value (format nil "~{~A~^,~}" (mapcar #'car subjects)))))
         (when next
           (htm (:input :type "hidden" :name "next" :value next)))

         (when text
           (htm (:input :type "hidden" :name "text" :value text)))

         )))
    :selected "people"))

(defroute "/testimonials/compose" ()
  (:get
    (require-user
      (testimonial-compose :subjects (parse-subject-list (get-parameter "subject")))))
  (:post
    (require-user
      (cond
        ((post-parameter "cancel")
         (see-other (or (post-parameter "next") "/home")))
        ((post-parameter "create")
         (let ((subjects (parse-subject-list (post-parameter "subject") :remove (write-to-string *userid*))))
           (cond
             (subjects

               
               )
             )
           )
         
         )
        ((post-parameter "add")
         (if (string= (post-parameter "add") "new")
           (testimonial-add-subject :subjects (parse-subject-list (post-parameter "subject"))
                                    :text (post-parameter "text")
                                    :next (post-parameter "next"))
           (testimonial-compose
             :text (post-parameter "text")
             :subjects (parse-subject-list
                         (format nil "~A,~A" (post-parameter "add") (post-parameter "subject"))))))

        ((post-parameter "search")
         (testimonial-add-subject :subjects (parse-subject-list (post-parameter "subject"))
                                  :text (post-parameter "text")
                                  :next (post-parameter "next")
                                  :results (iter (for id in (metaphone-index-query *metaphone-index* (post-parameter "name")))
                                                 (collect (list id (getf (db id) :name))))))
        (t
         (testimonial-compose
           :text (post-parameter "text")
           :subjects (parse-subject-list
                       (post-parameter "subject")
                       :remove (post-parameter "remove"))))))))


;;; }}}
