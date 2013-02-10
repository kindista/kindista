(in-package :kindista)

(defroute "/requests/new" ()
  (:get
    (require-user
      (enter-inventory-text :request)))
  (:post
    (require-user
      (cond
        ((post-parameter "cancel")
         (see-other (or (post-parameter "next") "/home")))

        ((post-parameter "back")
         (enter-inventory-text :request :text (post-parameter "text")))

        ((and (post-parameter "next")
              (post-parameter "text"))
          (enter-inventory-tags :type :request
                                 :text (post-parameter "text")))

        ((and (post-parameter "create")
              (post-parameter "text")) 

         (let ((tags (iter (for pair in (post-parameters*))
                           (when (and (string= (car pair) "tag")
                                      (scan *tag-scanner* (cdr pair)))
                             (collect (cdr pair))))))
           (iter (for tag in (tags-from-string (post-parameter "tags")))
                 (setf tags (cons tag tags)))
           
           (if (intersection tags *top-tags* :test #'string=)
             (see-other 
               (format nil "/requests/~A"
                 (create-inventory-item :type :request 
                                        :text (post-parameter "text") :tags tags)))

             (enter-inventory-tags :type :request
                                    :text (post-parameter "text")
                                    :tags tags
                                    :error "You must select at least one keyword"))))
        (t
         (enter-inventory-text
           :type :request
           :text (post-parameter "text")))))))

(defroute "/requests/<int:id>" (id)
  (:get
    (setf id (parse-integer id))
    (aif (db id)
      (with-user
        (standard-page
          "First few words... | Kindista"
          (html
            (:div :class "activity"
              (str (request-activity-item (gethash id *db-results*) :show-distance t))))
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
          (enter-inventory-tags :type :request
                                 :text (getf request :text)
                                 :tags (getf request :tags)
                                 :existing-url (s+ "/requests/" id "/edit"))))))
  (:post
    (require-user
      (let* ((request (db (parse-integer id))))
        (require-test ((eql *userid* (getf request :by))
                     "You can only edit your own requests.")
          (cond
            ((post-parameter "delete")
             (confirm-delete :url (s+ "/requests/" id "/edit")
                             :type "request"
                             :text (getf request :text)
                             :next-url (referer)))

            ((post-parameter "really-delete")
             (delete-inventory-item (parse-integer id))
             (flash "Your request has been deleted!")
             (see-other (or (post-parameter "next") "/home")))

            ((post-parameter "back")
             (enter-inventory-text :text (getf request :text)
                               :type :request
                               :existing-url (s+ "/requests/" id "/edit")  ))

            ((and (post-parameter "next")
                  (post-parameter "text"))

             (enter-inventory-tags :type :request
                                    :text (post-parameter "text")
                                    :tags (getf request :tags)
                                    :existing-url (s+ "/requests/" id "/edit")))

            ((and (post-parameter "create")
                  (post-parameter "text")) 

             (let ((tags (iter (for pair in (post-parameters*))
                               (when (and (string= (car pair) "tag")
                                          (scan *tag-scanner* (cdr pair)))
                                 (collect (cdr pair))))))
               (iter (for tag in (tags-from-string (post-parameter "tags")))
                     (setf tags (cons tag tags)))
               
               (if (intersection tags *top-tags* :test #'string=)
                 (progn
                   (modify-inventory-item (parse-integer id) :text (post-parameter "text")
                                                       :tags tags)
                                                                       
                   (see-other (s+ "/requests/" id)))

                 (enter-inventory-tags :type :request
                                       :text (post-parameter "text")
                                       :tags tags
                                       :error "You must select at least one keyword"))))
            (t
             (enter-inventory-tags :type :request
                                    :text (getf request :text)
                                    :tags (getf request :tags)
                                    :existing-url (s+ "/requests/" id "/edit")))))))))

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
              (html
                (:div :class "activity"
                  (:div :class "item"
                    (unless (or (not *user*) base q)
                      (htm
                        (:h4 "post a request") 
                        (:form :method "post" :action "/requests/new"
                          (:table :class "post"
                            (:tr
                              (:td (:textarea :cols "1000" :rows "4" :name "text"))
                              (:td
                                (:button :class "yes" :type "submit" :class "submit" :name "next" "Post")))))))

                    (:form :method "post" :action "/settings"
                      (when (or base q)
                        (htm
                          (:p :style "float: right;" (:a :href "/requests" "show all requests"))))
                      (:strong :class "small" "show results within ")
                      (:input :type "hidden" :name "next" :value (url-compose "/requests" "q" q "kw" base))
                      (let ((distance (user-rdist)))
                        (htm
                          (:select :name "rdist" :onchange "this.form.submit();"
                            (:option :value "1" :selected (when (eql distance 1) "") "1 mile")
                            (:option :value "2" :selected (when (eql distance 2) "") "2 miles")
                            (:option :value "5" :selected (when (eql distance 5) "") "5 miles")
                            (:option :value "10" :selected (when (eql distance 10) "") "10 miles")
                            (:option :value "25" :selected (when (eql distance 25) "") "25 miles")
                            (:option :value "100" :selected (when (eql distance 100) "") "100 miles"))))
                      " "
                      (:input :type "submit" :class "no-js" :value "apply")
                      (when q
                        (htm
                          (:p (:strong :class "small" "showing requests matching \"") (str q) (:strong "\"")) ))))
                  (iter (for i from 0 to (+ start 20))
                        (cond
                          ((< i start)
                           (pop items))

                          ((and (>= i start) items)
                           (str (request-activity-item (pop items) :show-distance t)))

                          (t
                           (when (< (user-rdist) 100)
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
                                   (:a :href (url-compose "/requests" "p" (- page 1) "kw" base) "< previous page")))
                               "&nbsp;"
                               (when (cdr items)
                                 (htm
                                   (:a :style "float: right;" :href (url-compose "/requests" "p" (+ page 1) "kw" base) "next page >")         )))))))))
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
