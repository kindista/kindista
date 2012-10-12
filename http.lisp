(in-package :kindista)

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

(defun run ()
  (load-users)
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

(defun password-match-p (username password)
  (let ((crypted-password (getf (load-user username) :pass)))
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
  `(let ((*user* (or *user* (check-token-cookie))))
     ,@body))

(defmacro require-user (&body body)
  `(with-user
     (if *user*
       (progn ,@body)
       (authorization-required))))

;; tokens

(defun make-token (id)
  (with-mutex (*make-token-lock*)
    (do (token)
      ((not (assoc (setf token (random-password 30)) *auth-tokens* :test #'string=))
      (prog1 token
        (setf *auth-tokens* (acons token (list id (get-universal-time)) *auth-tokens*)))))))

(defun check-token (token)
  (let ((value (assoc token *auth-tokens* :test #'string=)))
    (when (and value (< (get-universal-time) (+ (caddr value) 2592000)))
      (cadr value))))

(defun delete-token-cookie ()
  (awhen (cookie-in "token")
    (awhen (assoc it *auth-tokens* :test #'string=)
      (setf (caddr it) 0))
    (set-cookie "token" :value ""
                :http-only t
                :expires 0
                :secure nil)))

(defun see-other (url)
  (setf (return-code*) +http-see-other+)
  (setf (header-out :location) url)
  "")

(defmacro html (&body body)
  (let ((sym (gensym)))
    `(with-html-output-to-string (,sym)
       ,@body)))

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
                        (:img :src "/media/eamon.jpg"))
                       (:td (:a :href "/people/eamon" "Eamon Walker")))
                     (:tr
                       (:td (:a :href "/logout" "Log out"))))

                   (str (menu '(("Home" "news" "")
                                ("Inbox" "inbox")
                                ("People" "people")
                                ("Events" "events")
                                ("Resources" "resources"))
                              selected))

                   (:p :id "copyright"
                     "Kindista &copy; 2012"
                     (:br)
                     "Programmed in " (:a :href "http://www.gigamonkeys.com/book/introduction-why-lisp.html" "Common Lisp")) 

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
                   (awhen (get-parameter "retry")
                     (htm (:p :class "error" "The username or password was incorrect.")))
                   (:form :method "POST" :action "/login"
                     (:div :id "login"
                       (awhen (get-parameter "next")
                         (htm (:input :type "hidden" :name "next" :value it)))
                       (:input :type "text" :name "username" :placeholder "Username or email")
                       (:input :type "password" :name "password" :placeholder "Password"))
                     (:input :type "submit" :value "Log in"))
                   (:p (:a :href "/password/reset" "Forgot your password?"))
                   (:p "New to Kindista?"
                    (:br)
                    (:a :href "/account/create" "Sign up for an account"))
                   (:menu :id "footer"
                     (:li (:a :href "/help" "Help"))
                     (:li (:a :href "/about" "About"))
                     (:li (:a :href "/blog" "Blog"))
                    )
                   (:br)
                   (:br)
                   " ")
                 :class "landing")))))

(defroute "/logout" ()
  (:get
    (setf (return-code*) +http-see-other+)
    (setf (header-out :location) "/")
    (delete-token-cookie)
    ""))

(defroute "/login" ()
  (:get
    (setf (return-code*) +http-see-other+)
    (setf (header-out :location) "/")
    "")
  (:post
    (let ((username (post-parameter "username"))
          (next (post-parameter "next"))
          )
      (when (find #\@ username :test #'equal)
        (setf username (gethash username *email-to-username*)))
      (cond
        ((password-match-p username (post-parameter "password"))
         (set-cookie "token" :value (make-token username)
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

(defun activity-icons (&key url hearts comments)
  (html
    (:a :class "icons" :href url
      (when hearts
        (htm
          (:img :alt "love:" :src "/media/icons/heart16.png") 
          ;(:span :class "unicon" "♥ ")
          (str hearts))) 
      (when comments
        (htm
          (:img :alt "comments:" :src "/media/icons/comment16.png") 
          ;(:span :class "unicon" " ✎ ")
          (str comments))))))

(defun activity-item (&key url content time next-url hearts comments)
  (html
    (:div :class "item left resource"
      (str (timestamp time))
      (str content)
      (:div :class "actions"
        (str (love-button url))
        " &middot; "
        (str (comment-button url))
        " &middot; "
        (str (flag-button url))
        (str (activity-icons :hearts hearts :comments comments :url (s+ url "/comments")))))))

(defun offer-activity-item (&key time user-name user-id offer-id next-url hearts comments text)
  (activity-item :url (s+ "/offers/" offer-id)
                 :time time
                 :next-url next-url
                 :hearts hearts
                 :comments comments
                 :content (html
                            (:a :href (s+ "/people/" user-id) (str user-name))
                            " posted a new "
                            (:a :href (s+ "/people/" user-id "/offers#" offer-id) "offer")
                            (:blockquote (str (second (multiple-value-list (markdown text :stream nil))))))))

(defun joined-activity-item (&key time user-name user-id hearts comments next-url)
  (activity-item :url (s+ "/people/" user-id)
                 :time time
                 :next-url next-url
                 :hearts hearts
                 :comments comments
                 :content (html
                            (:a :href (s+ "/people/" user-id) (str user-name))
                            " joined Kindista")))

(defroute "/home" ()
  (:get
    (require-user
      (standard-page
        "Home"
        (html
          (:div :class "activity"
            (:div :class "item"
              (:h2 "Recent Activity"))
            (str (offer-activity-item
              :time (get-universal-time)
              :user-name "Benjamin Crandall"
              :user-id "ben"
              :offer-id "12345"
              :next-url "/home"
              :hearts 3
              :comments 7
              :text "[google](http://google.com) Saxophone lessons. I am **conservatory trained** (Bachelor of Music in Jazz Saxophone Performance from the CCM at University of Cincinnati). I have been playing for over 20 years, performing professionally in a reggae band (JohnStone Reggae in Washington DC and multiple jazz ensembles (currently StoneCold Jazz in Eugene.)"))
            (:div :class "item resource"
              (:div :class "timestamp" "Today at 4:40PM")
              (:a :href "/people/ben" "Benjamin Crandall") " posted a new "
              (:a :href "/people/ben/offers#12345" "offer")
              (:blockquote
                (:p "Saxophone lessons. I am conservatory trained (Bachelor of Music in Jazz Saxophone Performance from the CCM at University of Cincinnati. I have been playing for over 20 years, performing profession)ally in a reggae band (JohnStone Reggae in Washington DC and multiple )jazz ensembles (currently StoneCold Jazz in Eugene.)"))
              (:div :class "actions"
                (:form :method "POST" :action "/offers/12345"
                  (:input :type "submit" :name "like" :value "Love"))
                " &middot; "
                (:form :method "GET" :action "/offers/12345/comments"
                  (:input :type "submit" :value "Discuss"))
                " &middot; "
                (:form :method "GET" :action "/offers/12345/flag"
                  (:input :type "hidden" :name "next" :value "/home")
                  (:input :type "submit" :value "Flag"))
                (:div :class "icons"
                  (:img :src "/media/icons/comment16.png")
                  "5"
                  (:img :src "/media/icons/heart16.png")
                  "5"
                 )
                )
              )
            (:div :class "item testimonial"
              (:div :class "timestamp" "Today at 4:30PM")
              (:a :href "/people/eamon" "Eamon Walker") " wrote about "
              (:a :href "/people/ben" "Benjamin Crandall")
              (:blockquote
                (:p "Benjamin and I have been friends since last summer, when I first attended
                     one of his Taiji classes. I had been interested in taking Taiji classes
                     for a while and was intrigued that Benjamin's classes were being offered
                     as a gift&mdash;with no up-front or specified fee.")
                (:p "Kindista is the product of our friendship. Benjamin has believed so strongly
                     in Kindista that he has supported me in building it with him. Since the
                     beginning of 2012 he has provided me with housing, and since the Fall of 
                     2011 has provided me regularly with enough money to cover my basic costs
                     of living.")))
            (:div :class "item joined"
              (:div :class "timestamp" "June 15, 2012")
              (:a :href "/people/eamon" "Eamon Walker") " joined Kindista")))
         :right (html
                  (:div :class "item"
                    (:h2 "Upcoming Events")
                    (:menu
                      (:li "10/20 7:00PM " (:a :href "x" "East Eugene Gift Circle"))       
                      (:li "10/24 7:00PM " (:a :href "x" "West Eugene Gift Circle")))))))))

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


;;; }}}
