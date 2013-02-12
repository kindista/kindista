(in-package :kindista)

(defun requests-help-text ()
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

(defroute "/requests/new" ()
  (:get
    (require-user
      (enter-inventory-text :title "Post a request"
                            :action "/requests/new"
                            :selected "requests")))
  (:post
    (post-new-inventory-item "request" :url (s+ "/requests/new"))))

(defroute "/requests/<int:id>" (id)
  (:get
    (setf id (parse-integer id))
    (aif (db id)
      (with-user
        (standard-page
          "First few words... | Kindista"
          (html
            (:div :class "activity"
              (str (inventory-activity-item "request" (gethash id *db-results*) :show-distance t))))
          :selected "requests"))
      (standard-page "Not found" "not found")))
  (:post
    (require-user
      (setf id (parse-integer id)) 
      (aif (db id)
        (cond
          ((and (post-parameter "love")
                (member (getf it :type) '(:gratitude :resource :request)))
           (love id)
           (see-other (or (post-parameter "next") (referer))))
          ((and (post-parameter "unlove")
                (member (getf it :type) '(:gratitude :resource :request)))
           (unlove id)
           (see-other (or (post-parameter "next") (referer)))))
        (standard-page "Not found" "not found")))))

(defroute "/requests/<int:id>/edit" (id)
  (:get
    (require-user
      (let* ((request (db (parse-integer id))))
        (require-test ((eql *userid* (getf request :by))
                     "You can only edit your own requests.")
          (enter-inventory-tags :title "Edit your request"
                                :action (s+ "/requests/" id "/edit")
                                :text (getf request :text)
                                :tags (getf request :tags)
                                :button-text "Save request"
                                :selected "requests")))))
  (:post
    (post-existing-inventory-item "request" :id id
                                            :url (s+ "/requests/" id "/edit"))))

(defroute "/requests" ()
  (:get
    (with-user
      (with-location
        (let* ((page (if (scan +number-scanner+ (get-parameter "p"))
                       (parse-integer (get-parameter "p"))
                       0))
               (q (get-parameter "q"))
               (base (iter (for tag in (split " " (get-parameter "kw")))
                           (when (scan *tag-scanner* tag)
                             (collect tag))))
               (start (* page 20)))
          (when (string= q "") (setf q nil))
          (multiple-value-bind (tags items)
              (nearby-inventory-top-tags :request :base base :q q)
            (standard-page
             "Requests"
             (inventory-body-html "request" :base base 
                                            :q q 
                                            :items items 
                                            :start start 
                                            :page page)
          :top (when (getf *user* :help)
                 (requests-help-text))
          :search q
          :search-scope (if q "requests" "all")
          :right (html
                   (:h3 "browse by keyword")
                   (when base
                     (htm
                       (:p (:strong "keywords selected: ")) 
                       (:ul :class "keywords"
                         (dolist (tag base)
                           (htm
                             (:li
                               (:a :href (url-compose "/requests" "kw" tag "q" q) (str tag)) 
                               " "
                               (:a :href (url-compose "/requests" "kw" (remove tag base :test #'string=) "q" q)
                                   "[x]")      
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
                                              (url-compose "" "kw" (format nil "~{~a+~}~a" base (first subtag)) "q" q) )
                                      (str (s+ (car subtag) " (" (write-to-string (cdr subtag)) ")")))
                                  (unless (= i 1)
                                    (str ", "))))))
                       (htm
                         (:div :class "category"
                          (:h3 (:a :href (url-compose "" "kw" (format nil "~{~a+~}~a" base (first tag)) "q" q)
                                   (str (s+ (first tag) " (" (write-to-string (second tag)) ")"))))
                          (iter (for subtag in (third tag))
                                (for i downfrom (length (third tag)))
                                (htm
                                  (:a :href (url-compose "" "kw"
                                                         (if (string= (first subtag) "more")
                                                           (format nil "~{~a+~}~a" base (first tag))
                                                           (format nil "~{~a+~}~a+~a" base (first tag) (first subtag)))
                                                         "q" q)
                                      (str (s+ (car subtag) " (" (write-to-string (cdr subtag)) ")")))
                                  (unless (= i 1)
                                    (str ", "))))))))
                   (unless base
                     (htm
                       (:div :class "category"
                        (:h3 (:a :href "/requests/all" "show all keywords"))))))
         :selected "requests")))))))


(defroute "/requests/all" ()
(:get
  (require-user
    (let ((base (iter (for tag in (split " " (get-parameter "kw")))
                      (when (scan *tag-scanner* tag)
                        (collect tag)))))
      (multiple-value-bind (tags items)
          (nearby-inventory-top-tags *request-geo-index* :count 10000 :subtag-count 10)
        (standard-page
         "Requests"
         (html
           (unless base
             (simple-inventory-entry-html "request"))
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
                         (:a :href (format nil "/requests?kw=~{~a~^+~}" (remove tag base :test #'string=))
                             "[x]")      
                         " "
                         (:a :href (format nil "/requests?kw=~a" tag) (str tag)) 
                         ))))))
             (dolist (tag tags)
               (htm
                 (:div :class "category"
                  (:h3 (:a :href (format nil "/requests?kw=~{~a+~}~a" base (first tag))
                           (str (s+ (first tag) " (" (write-to-string (second tag)) ")"))))
                  (iter (for subtag in (third tag))
                        (for i downfrom (length (third tag)))
                        (htm
                          (:a :href (format nil "/requests?kw=~{~a+~}~a+~a" base (first tag) (first subtag))
                              (str (s+ (car subtag) " (" (write-to-string (cdr subtag)) ")")))
                          (unless (= i 1)
                            (str ", ")))))))))
           :top (when (getf *user* :help)
                 (requests-help-text))
           :selected "requests"))))))
