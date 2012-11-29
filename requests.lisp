(in-package :kindista)

(defun create-request (&key (by *userid*) text tags expires)
  (insert-db (list :type :request
                         :by by
                         :text text
                         :tags tags
                         :expires (+ (get-universal-time) (* expires 86400))
                         :created (get-universal-time))))

(defun index-request (id data)
  (let* ((by (getf data :by))
         (lat (or (getf data :lat) (getf (db by) :lat)))
         (long (or (getf data :long) (getf (db by) :long))))

    (timeline-insert (getf data :author) (getf data :created) id)
    
    (with-locked-hash-table (*request-index*)
      (setf (gethash by *request-index*)
            (push id (gethash by *request-index*))))

    (with-locked-hash-table (*request-stem-index*)
      (dolist (stem (stem-text (getf data :text)))
        (push (list (getf data :created)
                    lat
                    long
                    id)
              (gethash stem *request-stem-index*))))

    (resource-geo-index-insert *request-geo-index*
                               lat
                               long
                               id
                               (getf data :created)
                               (getf data :tags))
    (activity-geo-index-insert lat
                               long
                               id
                               (getf data :created)
                               (list by))))

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
                                          :request-id id
                                          :user-name (getf (db (getf it :by)) :name)
                                          :user-id (username-or-id (getf it :by))
                                          :hearts (length (loves id))
                                          ;:comments (length (comments id))
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
      (let ((page (if (scan +number-scanner+ (get-parameter "p"))
                    (parse-integer (get-parameter "p"))
                    0))
            (base (iter (for tag in (split " " (get-parameter "q")))
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
             (:form :class "item" :method "post" :action "/settings"
               (:strong "show results within ")
               (:input :type "hidden" :name "next" :value (format nil "/requests~a~{~a~^+~}" (if base "?q=" "") base))
               (let ((distance (rdist)))
                 (htm
                   (:select :name "rdist" :onchange "this.form.submit()"
                     (:option :value "1" :selected (when (eql distance 1) "") "1 mile")
                     (:option :value "2" :selected (when (eql distance 2) "") "2 miles")
                     (:option :value "5" :selected (when (eql distance 5) "") "5 miles")
                     (:option :value "10" :selected (when (eql distance 10) "") "10 miles")
                     (:option :value "25" :selected (when (eql distance 25) "") "25 miles")
                     (:option :value "100" :selected (when (eql distance 100) "") "100 miles"))))
               " "
               (:input :type "submit" :class "no-js" :value "apply"))
             (let ((start (* page 20)))
               (htm
                 (:div :class "activity"
                   (iter (for i from 0 to (+ start 20))
                         (cond
                           ((< i start)
                            (setf items (cdr items)))

                           ((and (>= i start) items)
                            (let* ((item (car items))
                                   (request (db item))
                                   (user (db (getf request :by))))
                              (str (request-activity-item :time (getf request :created)
                                                          :request-id item
                                                          :user-name (getf user :name)
                                                          :user-id (username-or-id (getf request :by))
                                                          :hearts (length (loves item))
                                                          ;:comments (length (comments item))
                                                          :text (getf request :text))))
                            (setf items (cdr items)))

                           (t
                            (when (< (rdist) 100)
                              (htm
                                (:div :class "item small"
                                 (:em "Increasing the ")(:strong "show results within")(:em " distance may yield more results."))))
                            (finish)))

                         (finally
                           (when (or (> page 0) (cdr items))
                             (htm
                               (:div :class "item"
                                (when (> page 0)
                                  (htm
                                    (:a :href (format nil "/requests?p=~a~a~{~a~^+~}" (- page 1) (if base "&q=" "") base) "< previous page")))
                                "&nbsp;"
                                (when (cdr items)
                                  (htm
                                    (:a :style "float: right;" :href (format nil "/requests?p=~a~a~{~a~^+~}" (+ page 1) (if base "&q=" "") base) "next page >"))))))))))))
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
                   (:div :class "item"
                     (:h2 "browse by keyword")
                     (when base
                       (htm
                         (:p (:a :href "/requests" "show all requests"))   
                         (:p (:strong "keywords selected: ")) 
                         (:ul :class "keywords"
                           (dolist (tag base)
                             (htm
                               (:li
                                 (:a :href (format nil "?q=~{~a~^+~}" (remove tag base :test #'string=))
                                     "[x]")      
                                 " "
                                 (:a :href (format nil "?q=~a" tag) (str tag)) 
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
                                                (format nil "?q=~{~a+~}~a" base (first subtag)))
                                        (str (s+ (car subtag) " (" (write-to-string (cdr subtag)) ")")))
                                    (unless (= i 1)
                                      (str ", "))))))
                         (htm
                           (:div :class "category"
                            (:h3 (:a :href (format nil "?q=~{~a+~}~a" base (first tag))
                                     (str (s+ (first tag) " (" (write-to-string (second tag)) ")"))))
                            (iter (for subtag in (third tag))
                                  (for i downfrom (length (third tag)))
                                  (htm
                                    (:a :href (if (string= (first subtag) "more")
                                                (format nil "?q=~{~a+~}~a" base (first tag))
                                                (format nil "?q=~{~a+~}~a+~a" base (first tag) (first subtag)))
                                        (str (s+ (car subtag) " (" (write-to-string (cdr subtag)) ")")))
                                    (unless (= i 1)
                                      (str ", "))))))))
                     (unless base
                       (htm
                         (:div :class "category"
                          (:h3 (:a :href "/requests/all" "show all keywords")))))))
           :selected "requests"))))))


(defroute "/requests/all" ()
  (:get
    (require-user
      (let ((base (iter (for tag in (split " " (get-parameter "q")))
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
                       (:p (:a :href "/requests" "show all requests"))   
                       (:p (:strong "keywords selected: ")) 
                       (:ul :class "keywords"
                         (dolist (tag base)
                           (htm
                             (:li
                               (:a :href (format nil "/requests?q=~{~a~^+~}" (remove tag base :test #'string=))
                                   "[x]")      
                               " "
                               (:a :href (format nil "/requests?q=~a" tag) (str tag)) 
                               ))))))
                   (dolist (tag tags)
                     (htm
                       (:div :class "category"
                        (:h3 (:a :href (format nil "/requests?q=~{~a+~}~a" base (first tag))
                                 (str (s+ (first tag) " (" (write-to-string (second tag)) ")"))))
                        (iter (for subtag in (third tag))
                              (for i downfrom (length (third tag)))
                              (htm
                                (:a :href (format nil "/requests?q=~{~a+~}~a+~a" base (first tag) (first subtag))
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
