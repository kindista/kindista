(in-package :kindista)

(defun create-request (&key (by *userid*) text tags expires)
  (insert-db (list :type :request
                         :by by
                         :text text
                         :tags tags
                         :expires (+ (get-universal-time) (* expires 86400))
                         :created (get-universal-time))))

(defun index-request (id data)
  (with-locked-hash-table (*request-index*)
    (setf (gethash (getf data :by) *request-index*)
          (union (gethash (getf data :by) *request-index*)
                 (list id))))
  (resource-geo-index-insert *request-geo-index*
                             (or (getf data :lat)
                                 (getf (db (getf data :by)) :lat))
                             (or (getf data :long)
                                 (getf (db (getf data :by)) :long))
                             id
                             (getf data :created)
                             (getf data :tags))
  (activity-geo-index-insert (or (getf data :lat)
                                 (getf (db (getf data :by)) :lat))
                             (or (getf data :long)
                                 (getf (db (getf data :by)) :long))
                             id
                             (getf data :created)))

(defun request-compose (&key text)
  (standard-page
   "Post a request"
   (html
     (:div :class "item"
      (:h2 "Post a request")
      (:form :method "post" :action "/requests/new"
        (:textarea :cols "40" :rows "8" :name "text" (str text))
        (:p  (:button :class "no" :type "submit" :class "cancel" :name "cancel" "Cancel")
        (:button :class "yes" :type "submit" :class "submit" :name "next" "Next")))))
   :selected "people"))

(defun request-compose-next (&key text expires error tags)
  ; show the list of top-level tags
  ; show recommended tags
  ; show preview
  ; cancel button
  ; edit (back) button
  ; create button
  (let ((suggested (or tags (get-tag-suggestions text))))
    (standard-page
     "Post a request"
     (html
       (:div :class "item"
        (:h2 "Preview your request")
        (when error
          (htm
            (:p :class "error" (str error))))
        (:form :method "post" :action "/requests/new" :class "post-next"
          (:input :type "hidden" :name "text" :value text)
          (:p (str text)
              " "
              (:button :class "red" :type "submit" :class "cancel" :name "back" "edit")) 
          (:h2 "select an expiration date")
          (:p (:label :for "expires" "expires in: ")
              (:select :name "expires"
                (:option :value "7" :selected (when (eql expires 7) t) "one week")
                (:option :value "14" :selected (when (eql expires 14) t)"two weeks")
                (:option :value "30" :selected (when (eql expires 30) t)"one month")
                (:option :value "90" :selected (when (eql expires 90) t)"three months") 
                (:option :value "180" :selected (when (eql expires 180) t)"six months"))) 
          (:h2 "select at least one keyword")
          (dolist (tag *top-tags*)
            (htm (:div :class "tag"
                   (:input :type "checkbox"
                           :name "tag"
                           :value tag
                           :checked (when (member tag suggested :test #'string=)
                                      (setf suggested (remove tag suggested :test #'string=))
                                      ""))
                   (:span (str tag)))))
          (:h2 "additional keywords (optional)")
          (:input :type "text" :name "tags" :size 40
                 :placeholder "e.g. produce, bicycle, tai-chi"
                 :value (format nil "~{~a~^,~^ ~}" suggested)
                 )
          (:p (:button :class "yes" :type "submit" :class "submit" :name "create" "Post request")))))
     :selected "people")))

; author
; creation date
; edited date
; text
; expiration date
; tags (at least 1)
; privacy ('all 'friends or listname)

(defroute "/requests/new" ()
  (:get
    (require-user
      (request-compose)))
  (:post
    (require-user
      (cond
        ((post-parameter "cancel")
         (see-other (or (post-parameter "next") "/home")))

        ((post-parameter "back")
         (request-compose :text (post-parameter "text")))

        ((and (post-parameter "next")
              (post-parameter "text"))
          (request-compose-next :text (post-parameter "text")))

        ((and (post-parameter "create")
              (post-parameter "text")
              (scan +number-scanner+ (post-parameter "expires")))

         (let ((tags (iter (for pair in (post-parameters*))
                           (when (and (string= (car pair) "tag")
                                      (scan *tag-scanner* (cdr pair)))
                             (collect (cdr pair))))))
           (iter (for tag in (tags-from-string (post-parameter "tags")))
                 (setf tags (cons tag tags)))
           
           (if (intersection tags *top-tags* :test #'string=)
             (see-other (format nil "/requests/~A"
                                    (create-request :text (post-parameter "text")
                                                    :tags tags
                                                    :expires (parse-integer
                                                               (post-parameter "expires")))))
             (request-compose-next :text (post-parameter "text")
                                   :expires (post-parameter "expires")
                                   :tags tags
                                   :error "You must select at least one keyword"))))
        (t
         (request-compose
           :text (post-parameter "text")))))))

(defroute "/requests/<int:id>" (id)
  (:get
    (setf id (parse-integer id))
    (aif (db id)
      (require-user
        (standard-page
          "First few words... | Kindista"
          (html
            (:div :class "activity"
              (str (request-activity-item :time (getf it :created)
                                          :request-id (write-to-string id)
                                          :user-name (getf (db (getf it :by)) :name)
                                          :user-id (username-or-id (getf it :by))
                                          :hearts (length (loves id))
                                          :comments (length (comments id))
                                          :text (getf it :text)))))
          :selected "requests"))
      (standard-page "Not found" "not found")))
  (:post
    (require-user
      (setf id (parse-integer id)) 
      (aif (db id)
        (cond
          ((and (post-parameter "love")
                (member (getf it :type) '(:gratitude :offer :request)))
           (love id)
           (see-other (or (post-parameter "next") (referer))))
          ((and (post-parameter "unlove")
                (member (getf it :type) '(:gratitude :offer :request)))
           (unlove id)
           (see-other (or (post-parameter "next") (referer)))))
        (standard-page "Not found" "not found")))))

(defroute "/requests" ()
  (:get
    (require-user
      (let ((base (iter (for tag in (split "&" (query-string*)))
                        (when (scan *tag-scanner* tag)
                          (collect tag)))))
        (multiple-value-bind (tags items)
            (nearby-resources-top-tags *request-geo-index* :base base)
          (standard-page
           "Requests"
           (html
             (unless base
               (htm
                 (:div :class "item"
                   (:h4 "post a request") 
                   (:form :method "post" :action "/requests/new"
                     (:table :class "post"
                       (:tr
                         (:td (:textarea :cols "1000" :rows "4" :name "text"))
                         (:td
                           (:button :class "yes" :type "submit" :class "submit" :name "next" "Post"))))))))
             (:div :class "activity"
               (dolist (item items)
                 (let* ((request (db item))
                        (user (db (getf request :by))))
                   (str (request-activity-item :time (getf request :created)
                                               :request-id (write-to-string item)
                                               :user-name (getf user :name)
                                               :user-id (write-to-string (getf request :by))
                                               :hearts (length (loves item))
                                               :comments (length (comments item))
                                               :text (getf request :text)))))))
           :top (when (getf *user* :help)
                  (welcome-bar
                    (html
                      (:h2 "Getting started with requests")
                      (:p "Here are some things you can do to get started:")
                      (:ul
                        (:li (:a :href "/requests/compose" "Post a request") " to the community for something you need.")
                        (:li "Browse recently posted requests listed below.")
                        (:li "Find specific requests by selecting keywords from the " (:strong "browse by keyword") " menu.")
                        (:li "Search for requests using the search "
                          (:span :class "menu-button" "button")
                          (:span :class "menu-showing" "bar")
                          " at the top of the screen.")))))
          :right (html
                   (:form :class "item" :method "post" :action "/settings"
                     (:h2 "change search distance")
                     (:input :type "hidden" :name "next" :value (script-name*))
                     (let ((distance (getf *user* :distance)))
                       (htm
                         (:select :name "distance"
                           (:option :value "2" :selected (when (eql distance 2) "") "2 miles")
                           (:option :value "5" :selected (when (eql distance 5) "") "5 miles")
                           (:option :value "10" :selected (when (eql distance 10) "") "10 miles")
                           (:option :value "25" :selected (when (eql distance 25) "") "25 miles")
                           (:option :value "100" :selected (when (eql distance 100) "") "100 miles"))))
                     (:input :type "submit" :value "Change"))
                   (:div :class "item"
                     (:h2 "browse by keyword")
                     (when base
                       (htm
                         (:p (:a :href "?" "show all requests"))   
                         (:p (:strong "keywords selected: ")) 
                         (:ul :class "keywords"
                           (dolist (tag base)
                             (htm
                               (:li
                                 (:a :href (format nil "?~{~a~^&~}" (remove tag base :test #'string=))
                                     "[x]")      
                                 " "
                                 (:a :href (format nil "?~a" tag) (str tag)) 
                                 ))))))
                     (dolist (tag tags)
                       (if (string= (first tag) "etc")
                         (htm
                           (:div :class "category"
                            (:h3 (:a :href "/requests/all" 
                                     (str (s+ "etc (" (write-to-string (second tag)) ")"))))
                            (iter (for subtag in (third tag))
                                  (for i downfrom (length (third tag)))
                                  (htm
                                    (:a :href (if (string= (first subtag) "more")
                                                "/requests/all"
                                                (format nil "?~{~a&~}~a" base (first subtag)))
                                        (str (s+ (car subtag) " (" (write-to-string (cdr subtag)) ")")))
                                    (unless (= i 1)
                                      (str ", "))))))
                         (htm
                           (:div :class "category"
                            (:h3 (:a :href (format nil "?~{~a&~}~a" base (first tag))
                                     (str (s+ (first tag) " (" (write-to-string (second tag)) ")"))))
                            (iter (for subtag in (third tag))
                                  (for i downfrom (length (third tag)))
                                  (htm
                                    (:a :href (if (string= (first subtag) "more")
                                                (format nil "?~{~a&~}~a" base (first tag))
                                                (format nil "?~{~a&~}~a&~a" base (first tag) (first subtag)))
                                        (str (s+ (car subtag) " (" (write-to-string (cdr subtag)) ")")))
                                    (unless (= i 1)
                                      (str ", "))))))))))
           :selected "requests"))))))


(defroute "/requests/all" ()
  (:get
    (require-user
      (let ((base (iter (for tag in (split "&" (query-string*)))
                        (when (scan *tag-scanner* tag)
                          (collect tag)))))
        (multiple-value-bind (tags items)
            (nearby-resources-top-tags *request-geo-index* :count 10000 :subtag-count 10)
          (standard-page
           "Requests"
           (html
             (unless base
               (htm
                 (:div :class "item"
                   (:h4 "post a request") 
                   (:form :method "post" :action "/requests/new"
                     (:table :class "post"
                       (:tr
                         (:td (:textarea :cols "1000" :rows "4" :name "text"))
                         (:td
                           (:button :class "yes" :type "submit" :class "submit" :name "next" "Post"))))))))
             
                 (:div :class "item"
                   (:h2 "browse by keyword")
                   (when base
                     (htm
                       (:p (:a :href "?" "show all requests"))   
                       (:p (:strong "keywords selected: ")) 
                       (:ul :class "keywords"
                         (dolist (tag base)
                           (htm
                             (:li
                               (:a :href (format nil "/requests?~{~a~^&~}" (remove tag base :test #'string=))
                                   "[x]")      
                               " "
                               (:a :href (format nil "/requests?~a" tag) (str tag)) 
                               ))))))
                   (dolist (tag tags)
                     (htm
                       (:div :class "category"
                        (:h3 (:a :href (format nil "/requests?~{~a&~}~a" base (first tag))
                                 (str (s+ (first tag) " (" (write-to-string (second tag)) ")"))))
                        (iter (for subtag in (third tag))
                              (for i downfrom (length (third tag)))
                              (htm
                                (:a :href (format nil "/requests?~{~a&~}~a&~a" base (first tag) (first subtag))
                                    (str (s+ (car subtag) " (" (write-to-string (cdr subtag)) ")")))
                                (unless (= i 1)
                                  (str ", ")))))))))
           :top (when (getf *user* :help)
                  (welcome-bar
                    (html
                      (:h2 "Getting started with requests")
                      (:p "Here are some things you can do to get started:")
                      (:ul
                        (:li (:a :href "/requests/new" "Post a request") " to the community for something you need.")
                        (:li "Browse recently posted requests listed below.")
                        (:li "Find specific requests by selecting keywords from the " (:strong "browse by keyword") " menu.")
                        (:li "Search for requests using the search "
                          (:span :class "menu-button" "button")
                          (:span :class "menu-showing" "bar")
                          " at the top of the screen.")))))
           :selected "requests"))))))
