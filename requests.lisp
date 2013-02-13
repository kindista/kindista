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
    (post-new-inventory-item "request" :url "/requests/new")))

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
            :right (browse-inventory-tags "request" :q q :base base :tags tags)
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
           (browse-all-inventory-tags "request" :base base :tags tags)
           :top (when (getf *user* :help)
                 (requests-help-text))
           :selected "requests"))))))
