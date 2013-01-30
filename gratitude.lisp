(in-package :kindista)

(defun create-gratitude (&key author subjects text)
  (insert-db `(:type :gratitude
               :author ,author
               :subjects ,subjects
               :text ,text
               :created ,(get-universal-time))))

(defun index-gratitude (id data)
  (let* ((author (db (getf data :author)))
         (created (getf data :created))
         (subjects (getf data :subjects))
         (people (cons (getf data :author) subjects))
         (result (make-result :latitude (getf author :lat)
                              :longitude (getf author :long)
                              :people people
                              :created created
                              :type :gratitude
                              :id id)))
    
    (with-locked-hash-table (*db-results*)
      (setf (gethash id *db-results*) result))

    (geo-index-insert *activity-geo-index* result)

    (with-locked-hash-table (*activity-person-index*)
      (dolist (person people)
        (asetf (gethash person *activity-person-index*)
               (sort (push result it) #'> :key #'result-created))))

    (dolist (subject subjects)
      (let ((user (db subject)))
        (geo-index-insert *activity-geo-index* (make-result :latitude (getf user :lat)
                                                            :longitude (getf user :long)
                                                            :people people
                                                            :id id
                                                            :created created))))))

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


(defun delete-gratitude (id)
  (let* ((result (gethash id *db-results*))
         (data (db id))
         (people (cons (getf data :author) (getf data :subjects))))

    (with-locked-hash-table (*db-results*)
      (remhash id *db-results*))

    (with-locked-hash-table (*activity-person-index*)
      (dolist (person people)
        (asetf (gethash person *activity-person-index*)
               (remove result it))))

    (geo-index-remove *activity-geo-index* result)
    (remove-from-db id)))

(defun gratitude-compose (&key subjects text next existing-url)
  (if subjects
    (standard-page
     (if existing-url "Edit your statement of gratitude" "Express gratitude")
     (html
       (:div :class "item"
        (:h2 (str (if existing-url "Edit your statement of gratitude"
                                   "Express gratitude")))
       (:div :class "item"
        (:form :method "post" 
               :action (or existing-url "/gratitude/new") 
               :class "recipients"
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
          (:input :type "submit" :class "submit" :name "create" :value "Create"))))))
     :selected "people")
    (gratitude-add-subject :text text :next next)))

(defun gratitude-add-subject (&key subjects text next (results 'none))
  (standard-page
    "Express gratitude"
    (html
      (:div :class "item"
       (:h2 "Who would you like to write about?")
       (:h3 "Search for a person or project")
       (:form :method "post" :action "/gratitude/new"
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

(defroute "/gratitude/new" ()
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
                                :results (iter (for result in (metaphone-index-query *metaphone-index* (post-parameter "name")))
                                               (let ((id (result-id result)))
                                                 (collect (list id (getf (db id) :name)))))))
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
            (str (gratitude-activity-item (make-result :id id
                                                       :created (getf it :created)
                                                       :people (cons (getf it :author) (getf it :subjects)))
                                          :next-url (script-name*))))))
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

(defroute "/gratitude/<int:id>/edit" (id)
  (:get
    (require-user
      (let* ((gratitude (db (parse-integer id))))
        (require-test ((eql *userid* (getf gratitude :author))
                       "You can only edit gratitudes you have written.")
          (gratitude-compose :subjects (getf gratitude :subjects)
                             :text (getf gratitude :text)
                             :existing-url (s+ "/gratitude/" id "/edit"))))))
  (:post
    (require-user
      (let* ((gratitude (db (parse-integer id))))
        (require-test ((eql *userid* (getf gratitude :author))
                       "You can only edit gratitudes you have written.")
          (cond
            ((post-parameter "delete")
             (confirm-delete :url (s+ "/gratitude/" id "/edit")
                             :type "gratitude"
                             :text (getf gratitude :text)
                             :next-url(referer)))
            ((post-parameter "really-delete")
             (delete-gratitude (parse-integer id))
             (flash "Your statement of gratitude has been deleted!")
             (see-other (or (post-parameter "next") "/home")))

            (t
             (gratitude-compose :subjects (getf gratitude :subjects)
                                :text (getf gratitude :text)
                                :existing-url (s+ "/gratitude/" id "/edit")))
))))))

