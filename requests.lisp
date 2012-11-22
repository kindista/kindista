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
  (geo-index-insert (or (getf data :lat)
                        (getf (db (getf data :by)) :lat))
                    (or (getf data :long)
                        (getf (db (getf data :by)) :long))
                    id
                    (getf data :tags)
                    :index *resource-geo-index*) 
  (geo-index-insert (or (getf data :lat)
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
        (:p (:label :for "expires" "expires in: ")
            (:select :name "expires"
              (:option :value "7" "one week")
              (:option :value "14" "two weeks")
              (:option :value "30" "one month")
              (:option :value "90" "three months") 
              (:option :value "180" "six months"))) 
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
        (:h2 "Post a request")
        (when error
          (htm
            (:p :class "error" (str error))))
        (:form :method "post" :action "/requests/new"
          (:input :type "hidden" :name "text" :value text)
          (:input :type "hidden" :name "expires" :value expires)
          (:p (str text))
          (:h2 "select at least one top-level tag")
          (dolist (tag *top-tags*)
            (htm (:div :class "tag"
                   (:input :type "checkbox"
                           :name "tag"
                           :value tag
                           :checked (when (member tag suggested :test #'string=)
                                      ""))
                   (:span (str tag)))))
          (:h2 "extra tags")
          (:input :type "text" :name "tags" :size 40 :placeholder "e.g. produce, bicycle, tai-chi")
          (:p (:button :class "no" :type "submit" :class "cancel" :name "back" "Back")
              (:button :class "yes" :type "submit" :class "submit" :name "create" "Create")))))
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
              (post-parameter "text")
              (post-parameter "expires"))
          (request-compose-next :text (post-parameter "text")
                                :expires (post-parameter "expires")))
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
                                   :error "You must select at least one top-level tag."))))
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
            (str (request-activity-item :time (getf it :created)
                                        :request-id (write-to-string id)
                                        :user-name (getf (db (getf it :by)) :name)
                                        :user-id (username-or-id (getf it :by))
                                        :hearts (length (loves id))
                                        :comments (length (comments id))
                                        :text (getf it :text))))
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
      (standard-page
        "Requests"
        (html
          (:div :class "categories"
            (:h2 "browse categories")
            (dolist (tag (nearby-top-tags))
              (htm
                (:div :class "category"
                 (:h3 (:a :href (s+ "/tag/" (first tag)) (str (s+ (first tag) " (" (write-to-string (second tag)) ")"))))
                 (iter (for subtag in (third tag))
                       (for i downfrom (length (third tag)))
                       (htm
                         (:a :href (s+ "/tag/" (first tag) "/" (first subtag))
                             (str (s+ (car subtag) " (" (write-to-string (cdr subtag)) ")")))
                         (unless (= i 1)
                           (str ", ")))))))))
        :selected "requests"))))
