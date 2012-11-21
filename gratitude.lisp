(in-package :kindista)

(defun create-gratitude (&key author subjects text)
  (insert-db `(:type :gratitude
               :author ,author
               :subjects ,subjects
               :text ,text
               :created ,(get-universal-time))))

(defun index-gratitude (id data)
  (timeline-insert (getf data :author) (getf data :created) id)

  (let ((user (db (getf data :author))))
    (geo-index-insert (getf user :lat)
                      (getf user :long)
                      id
                      (getf data :created)))

  (with-locked-hash-table (*gratitude-index*)
    (dolist (subject (getf data :subjects))
      (timeline-insert subject (getf data :created) id)
      (let ((user (db subject)))
        (geo-index-insert (getf user :lat)
                          (getf user :long)
                          id
                          (getf data :created)))
      (push id (gethash subject *gratitude-index*)))))

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

(defun gratitude-compose (&key subjects text next)
  (if subjects
    (standard-page
     "Express gratitude"
     (html
       (:div :class "item"
        (:h2 "Express gratitude"))
       (:div :class "item"
        (:form :method "post" :action "/gratitude/compose" :class "recipients"
          (:label "About:")
          (:menu :class "recipients"
           (unless subjects
             (htm (:li (:em "nobody yet"))))
           (dolist (subject subjects)
             (htm
               (:li (str (getf (db subject) :name)) (:button :class "text large" :type "submit" :name "remove" :value subject " ⨯ ")))))
          (when subjects
            (htm (:input :type "hidden" :name "subject" :value (format nil "~{~A~^,~}" subjects))))
          (when next
            (htm (:input :type "hidden" :name "next" :value next)))

          (:p (:button :type "submit" :class "text" :name "add" :value "new" "+ Add a person or project"))
          (:textarea :rows "8" :name "text" (str text))
          (:p  (:input :type "submit" :class "cancel" :name "cancel" :value "Cancel")
          (:input :type "submit" :class "submit" :name "create" :value "Create")))))
     :selected "people")
    (gratitude-add-subject :text text :next next)))

(defun gratitude-add-subject (&key subjects text next (results 'none))
  (standard-page
    "Express gratitude"
    (html
      (:div :class "item"
       (:h2 "Who would you like to write about?")
       (:h3 "Search for a person or project")
       (:form :method "post" :action "/gratitude/compose"
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
           (htm (:input :type "hidden" :name "subject" :value (format nil "~{~A~^,~}" subjects))))
         (when next
           (htm (:input :type "hidden" :name "next" :value next)))

         (when text
           (htm (:input :type "hidden" :name "text" :value text)))

         )))
    :selected "people"))

(defroute "/gratitude/compose" ()
  (:get
    (require-user
      (gratitude-compose :subjects (parse-subject-list (get-parameter "subject")))))
  (:post
    (require-user
      (cond
        ((post-parameter "cancel")
         (see-other (or (post-parameter "next") "/home")))
        ((post-parameter "create")
         (let ((subjects (parse-subject-list (post-parameter "subject") :remove (write-to-string *userid*))))
           (cond
             ((and subjects (post-parameter "text"))
              (see-other (format nil "/gratitude/~A"
                                 (create-gratitude :author *userid*
                                                   :subjects subjects
                                                   :text (post-parameter "text")))))
             (subjects
              "no text")
             ((post-parameter "text")
              "no subject")
             (t
              "totally blank"))))
        ((post-parameter "add")
         (if (string= (post-parameter "add") "new")
           (gratitude-add-subject :subjects (parse-subject-list (post-parameter "subject"))
                                  :text (post-parameter "text")
                                  :next (post-parameter "next"))
           (gratitude-compose
             :text (post-parameter "text")
             :subjects (parse-subject-list
                         (format nil "~A,~A" (post-parameter "add") (post-parameter "subject"))))))

        ((post-parameter "search")
         (gratitude-add-subject :subjects (parse-subject-list (post-parameter "subject"))
                                :text (post-parameter "text")
                                :next (post-parameter "next")
                                :results (iter (for id in (metaphone-index-query *metaphone-index* (post-parameter "name")))
                                                 (collect (list id (getf (db id) :name))))))
        (t
         (gratitude-compose
           :text (post-parameter "text")
           :subjects (parse-subject-list
                       (post-parameter "subject")
                       :remove (post-parameter "remove"))))))))

(defroute "/gratitude/<int:id>" (id)
  (:get
    (setf id (parse-integer id))
    (aif (db id)
      (require-user
        (standard-page
          "First few words... | Kindista"
          (html
            (str (gratitude-activity-item :time (getf it :created)
                                          :id id
                                          :next-url (script-name*)
                                          :text (getf it :text))))))
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
