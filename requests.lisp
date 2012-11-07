(in-package :kindista)

(defun create-request (&key (by ruserid*) text)
  (insert-db (list :type :request
                         :by by
                         :text text
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
                    (getf data :created)))

(defun request-compose (&key text)
  (standard-page
   "Post a request"
   (html
     (:div :class "item"
      (:h2 "Post a request"))
     (:div :class "item"
      (:form :method "post" :action "/requets/post"
        (:textarea :rows "8" :name "text" (str text))
        (:p  (:button :class "no" :type "submit" :class "cancel" :name "cancel" "Cancel")
        (:button :class "yes" :type "submit" :class "submit" :name "create" "Create")))))
   :selected "people"))

(defroute "/requests/compose" ()
  (:get
    (require-user
      (request-compose)))
  (:post
    (require-user
      (cond
        ((post-parameter "cancel")
         (see-other (or (post-parameter "next") "/home")))
        ((post-parameter "create")
         (let ((subjects (parse-subject-list (post-parameter "subject") :remove (write-to-string *userid*))))
           (cond
             ((post-parameter "text")
              (see-other (format nil "/requests/~A"
                                     (create-request :text (post-parameter "text")))))
             (t
              "totally blank"))))
        (t
         (request-compose
           :text (post-parameter "text")
           :subjects (parse-subject-list
                       (post-parameter "subject")
                       :remove (post-parameter "remove"))))))))

(defroute "/requests/<int:id>" (id)
  (:get
    (setf id (parse-integer id))
    (aif (db id)
      (require-user
        (standard-page
          "First few words... | Kindista"
          (html
            (str (request-activity-item :time (getf it :created)
                                        :request-id id
                                        :user-name (getf (db (getf it :by)) :name)
                                        :user-id (username-or-id (getf it :by))
                                        :hearts (length (loves id))
                                        :comments (length (comments id))
                                        :text (getf it :text))))))
      (standard-page "Not found" "not found")))
  (:post
    (require-user
      (setf id (parse-integer id)) 
      (aif (db id)
        (cond
          ((and (post-parameter "love")
                (member (getf it :type) '(:testimonial :offer :request)))
           (love id)
           (see-other (or (post-parameter "next") (referer))))
          ((and (post-parameter "unlove")
                (member (getf it :type) '(:testimonial :offer :request)))
           (unlove id)
           (see-other (or (post-parameter "next") (referer)))))
        (standard-page "Not found" "not found")))))
