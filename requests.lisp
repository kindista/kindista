(in-package :kindista)

(defun create-request (&key (by *userid*) text tags)
  (insert-db (list :type :request
                         :by by
                         :text text
                         :tags tags
                         :created (get-universal-time))))

(defun index-request (id data)
  (let* ((by (getf data :by))
         (result (make-result :latitude (or (getf data :lat) (getf (db (getf data :by)) :lat))
                              :longitude (or (getf data :long) (getf (db (getf data :by)) :long))
                              :id id
                              :type :request
                              :people (list by)
                              :created (or (getf data :edited) (getf data :created))
                              :tags (getf data :tags))))

    (with-locked-hash-table (*db-results*)
      (setf (gethash id *db-results*) result))

    ;(timeline-insert (getf data :author) result)
    
    (with-locked-hash-table (*request-index*)
      (push id (gethash by *request-index*)))

    (let ((stems (stem-text (getf data :text))))
      (with-locked-hash-table (*request-stem-index*)
        (dolist (stem stems)
          (push result (gethash stem *request-stem-index*)))))

    (with-locked-hash-table (*activity-person-index*)
      (asetf (gethash by *activity-person-index*)
             (sort (push result it) #'> :key #'result-created)))

    (geo-index-insert *request-geo-index* result)
    (geo-index-insert *activity-geo-index* result)))

(defun modify-request (id &key text tags latitude longitude)
  (let ((result (gethash id *db-results*))
        (data (db id))
        (now (get-universal-time)))

    (when text
      (let* ((oldstems (stem-text (getf data :text)))
             (newstems (stem-text text))
             (common (intersection oldstems newstems :test #'string=)))

        (flet ((commonp (stem)
                 (member stem common :test #'string=)))

          (setf oldstems (delete-if #'commonp oldstems))
          (setf newstems (delete-if #'commonp newstems))
          
          (with-locked-hash-table (*request-stem-index*)
            (dolist (stem oldstems)
              (asetf (gethash stem *request-stem-index*)
                     (remove result it))) 

            (dolist (stem newstems)
              (push result (gethash stem *request-stem-index*)))))))

    (unless (equal tags (getf data :tags))
      (setf (result-tags result) tags))

    (when (and latitude
               longitude
               (or (not (eql latitude (getf data :lat)))
                   (not (eql longitude (getf data :long)))))

      (geo-index-remove *request-geo-index* result)
      (geo-index-remove *activity-geo-index* result)
      (setf (result-latitude result) latitude)
      (setf (result-longitude result) longitude)
      (geo-index-insert *request-geo-index* result)
      (geo-index-insert *activity-geo-index* result))

    (setf (result-created result) now)
    
    (with-locked-hash-table (*activity-person-index*)
      (asetf (gethash id *activity-person-index*)
             (sort it #'> :key #'result-created)))
    
    (modify-db id :text text :tags tags :lat latitude :long longitude :edited now)))

(defun delete-request (id)
  (let ((result (gethash id *db-results*))
        (data (db id)))

    (with-locked-hash-table (*db-results*)
      (remhash id *db-results*))
    
    (with-locked-hash-table (*request-index*)
      (asetf (gethash (getf data :by) *request-index*)
             (remove id it)))

    (let ((stems (stem-text (getf data :text))))
      (with-locked-hash-table (*request-stem-index*)
        (dolist (stem stems)
          (asetf (gethash stem *request-stem-index*)
                 (remove result it)))))

    (with-locked-hash-table (*activity-person-index*)
      (asetf (gethash (getf data :by) *activity-person-index*)
             (remove result it)))

    (geo-index-remove *request-geo-index* result)
    (geo-index-remove *activity-geo-index* result)

    (remove-from-db id)))

(defun request-compose (&key text existing-url)
  (standard-page
   (if existing-url "Edit your request" "Post a request")
   (html
     (:div :class "item"
      (:h2 (str (if existing-url "Edit your request" "Post a request")))
      (:form :method "post" :action (or existing-url "/requests/new")
        (:textarea :cols "40" :rows "8" :name "text" (str text))
        (:p  (:button :class "no" :type "submit" :class "cancel" :name "cancel" "Cancel")
        (:button :class "yes" :type "submit" :class "submit" :name "next" "Next")))))
   :selected "requests"))

(defun request-compose-next (&key text error tags existing-url)
  ; show the list of top-level tags
  ; show recommended tags
  ; show preview
  ; cancel button
  ; edit (back) button
  ; create button
  (let ((suggested (or tags (get-tag-suggestions text))))
    (standard-page
     (if existing-url "Edit your request" "Post a request")
     (html
       (:div :class "item"
        (:h2 "Preview your request")
        (when error
          (htm
            (:p :class "error" (str error))))
        (:form :method "post" :action (or existing-url "/requests/new") :class "post-next"
          (:input :type "hidden" :name "text" :value (escape-for-html text))
          (:p (cl-who:esc text)
              " "
              (:button :class "red" :type "submit" :class "cancel" :name "back" "edit")) 
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
          (:p (:button :class "yes" :type "submit" :class "submit" :name "create" (str (if existing-url "Save request" "Post request")))))))
     :selected "requests")))

; author
; creation date
; edited date
; text
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
              (post-parameter "text")) 

         (let ((tags (iter (for pair in (post-parameters*))
                           (when (and (string= (car pair) "tag")
                                      (scan *tag-scanner* (cdr pair)))
                             (collect (cdr pair))))))
           (iter (for tag in (tags-from-string (post-parameter "tags")))
                 (setf tags (cons tag tags)))
           
           (if (intersection tags *top-tags* :test #'string=)
             (see-other (format nil "/requests/~A"
                                    (create-request :text (post-parameter "text")
                                                    :tags tags)))

             (request-compose-next :text (post-parameter "text")
                                   :tags tags
                                   :error "You must select at least one keyword"))))
        (t
         (request-compose
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
                (member (getf it :type) '(:gratitude :offer :request)))
           (love id)
           (see-other (or (post-parameter "next") (referer))))
          ((and (post-parameter "unlove")
                (member (getf it :type) '(:gratitude :offer :request)))
           (unlove id)
           (see-other (or (post-parameter "next") (referer)))))
        (standard-page "Not found" "not found")))))

(defroute "/requests/<int:id>/edit" (id)
  (:get
    (require-user
      (let* ((request (db (parse-integer id))))
        (require-test ((eql *userid* (getf request :by))
                     "You can only edit your own requests.")
          (request-compose-next :text (getf request :text)
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
             (delete-request (parse-integer id))
             (flash "Your request has been deleted!")
             (see-other (or (post-parameter "next") "/home")))

            ((post-parameter "back")
             (request-compose :text (getf request :text)
                              :existing-url (s+ "/requests/" id "/edit")  ))

            ((and (post-parameter "next")
                  (post-parameter "text"))

             (request-compose-next :text (post-parameter "text")
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
                   (modify-request (parse-integer id) :text (post-parameter "text")
                                                      :tags tags)
                                                                       
                   (see-other (s+ "/requests/" id)))

                 (request-compose-next :text (post-parameter "text")
                                       :tags tags
                                       :error "You must select at least one keyword"))))
            (t
             (request-compose-next :text (getf request :text)
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
              (nearby-resources-top-tags 'request :base base :q q)
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
