(in-package :kindista)

(defun create-inventory-item (&key type (by *userid*) text tags)
  (insert-db (list :type type
                   :by by
                   :text text
                   :tags tags
                   :created (get-universal-time))))

(defun index-inventory-item (id data)
  (let* ((by (getf data :by))
         (type (getf data :type))
         (result (make-result :latitude (or (getf data :lat) (getf (db (getf data :by)) :lat))
                              :longitude (or (getf data :long) (getf (db (getf data :by)) :long))
                              :id id
                              :type type
                              :people (list by)
                              :created (or (getf data :edited) (getf data :created))
                              :tags (getf data :tags))))

    (with-locked-hash-table (*db-results*)
      (setf (gethash id *db-results*) result))

    (if (eq type :resource)
      (with-locked-hash-table (*resource-index*)
        (push id (gethash by *resource-index*)))
      (with-locked-hash-table (*request-index*)
        (push id (gethash by *request-index*))))

    (let ((stems (stem-text (getf data :text))))
      (if (eq type :resource)
        (with-locked-hash-table (*resource-stem-index*)
          (dolist (stem stems)
            (push result (gethash stem *resource-stem-index*)))) 
        (with-locked-hash-table (*request-stem-index*)
          (dolist (stem stems)
            (push result (gethash stem *request-stem-index*))))))

    (with-locked-hash-table (*activity-person-index*)
      (asetf (gethash by *activity-person-index*)
             (sort (push result it) #'> :key #'result-created)))

    (if (eq type :resource)
      (geo-index-insert *resource-geo-index* result)
      (geo-index-insert *request-geo-index* result))
    (geo-index-insert *activity-geo-index* result)))

(defun modify-inventory-item (id &key text tags latitude longitude)
  (let* ((result (gethash id *db-results*))
         (type (result-type result))
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
          
          (when (eq type :resource)          
            (with-locked-hash-table (*resource-stem-index*)
              (dolist (stem oldstems)
                (asetf (gethash stem *resource-stem-index*)
                       (remove result it))))
              (dolist (stem newstems)
                (push result (gethash stem *resource-stem-index*))))

          (when (eq type :request)          
            (with-locked-hash-table (*request-stem-index*)
              (dolist (stem oldstems)
                (asetf (gethash stem *request-stem-index*)
                       (remove result it))))
              (dolist (stem newstems)
                (push result (gethash stem *request-stem-index*)))))))

    (unless (equal tags (getf data :tags))
      (setf (result-tags result) tags))

    (when (and latitude
               longitude
               (or (not (eql latitude (getf data :lat)))
                   (not (eql longitude (getf data :long)))))

      (if (eq type :resource)          
        (geo-index-remove *resource-geo-index* result)  
        (geo-index-remove *request-geo-index* result))
      (geo-index-remove *activity-geo-index* result)
      (setf (result-latitude result) latitude)
      (setf (result-longitude result) longitude)
      (if (eq type :resource)          
        (geo-index-insert *resource-geo-index* result)  
        (geo-index-insert *request-geo-index* result))
      (geo-index-insert *activity-geo-index* result))

    (setf (result-created result) now)
    
    (with-locked-hash-table (*activity-person-index*)
      (asetf (gethash id *activity-person-index*)
             (sort it #'> :key #'result-created)))
    
    (modify-db id :text text :tags tags :lat latitude :long longitude :edited now)))

(defun delete-inventory-item (id)
  (let* ((result (gethash id *db-results*))
         (type (result-type result))
         (data (db id)))

    (when (eq type :resource)
      (with-locked-hash-table (*resource-index*)
        (asetf (gethash (getf data :by) *resource-index*)
               (remove id it)))
      (let ((stems (stem-text (getf data :text))))
        (with-locked-hash-table (*resource-stem-index*)
          (dolist (stem stems)
            (asetf (gethash stem *resource-stem-index*)
                   (remove result it)))))
      (geo-index-remove *resource-geo-index* result))

    (when (eq type :request)
      (with-locked-hash-table (*request-index*)
        (asetf (gethash (getf data :by) *request-index*)
               (remove id it)))
      (let ((stems (stem-text (getf data :text))))
        (with-locked-hash-table (*request-stem-index*)
          (dolist (stem stems)
            (asetf (gethash stem *request-stem-index*)
                   (remove result it)))))
      (geo-index-remove *request-geo-index* result))

    (with-locked-hash-table (*activity-person-index*)
      (asetf (gethash (getf data :by) *activity-person-index*)
             (remove result it)))

    (geo-index-remove *activity-geo-index* result)

    (with-locked-hash-table (*db-results*)
      (remhash id *db-results*))
    
    (remove-from-db id)))

(defun enter-inventory-text (&key text type existing-url)
  (standard-page
    (if (eq type :resource)
      (if existing-url "Edit your resource" "Post a resource")
      (if existing-url "Edit your request" "Post a request"))
   (html
     (:div :class "item"
       (if (eq type :resource)
         (htm (:h2 (str (if existing-url "Edit your resource" "Post a resource"))))
         (htm (:h2 (str (if existing-url "Edit your request" "Post a request")))))
      (:form :method "post" 
             :action (or existing-url (if (eq type :resource) "/resources/new" 
                                                          "/requests/new"))
        (:textarea :cols "40" :rows "8" :name "text" (str text))
        (:p  (:button :class "no" :type "submit" :class "cancel" :name "cancel" "Cancel")
        (:button :class "yes" :type "submit" :class "submit" :name "next" "Next")))))
   :selected (if (eq type :resource) "resources" "requests")))

(defun enter-inventory-tags (&key type text error tags existing-url)
  ; show the list of top-level tags
  ; show recommended tags
  ; show preview
  ; cancel button
  ; edit (back) button
  ; create button
  (let ((suggested (or tags (get-tag-suggestions text))))
    (standard-page
    (if (eq type :resource)
      (if existing-url "Edit your resource" "Post a resource")
      (if existing-url "Edit your request" "Post a request"))
     (html
       (:div :class "item"
        (:h2 "Preview your " (str (if (eq type :resource) "resource" "request")))
        (when error
          (htm
            (:p :class "error" (str error))))
        (:form :class "post-next"
               :method "post" 
               :action (or existing-url (if (eq type :resource) "/resources/new" 
                                                             "/requests/new"))
          (:input :type "hidden" :name "text" :value (escape-for-html text))
          (:p (cl-who:esc text)
              " "
              (:button :class "red" :type "submit" :class "cancel" :name "back" "edit")) 
          (:h2 "select at least one keyword")
          (dolist (tag *top-tags*)
            (htm 
              (:div :class "tag"
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
                  :value (format nil "~{~a~^,~^ ~}" suggested))

          (:p (:button :class "yes" 
                       :type "submit" 
                       :class "submit" 
                       :name "create" 
                       (if (eq type :resource)
                         (str (if existing-url "Save offer" "Post offer"))     
                         (str (if existing-url "Save request" "Post request"))))))))
     :selected (if (eq type :resource) "resources" "requests"))))

; author
; creation date
; edited date
; text
; tags (at least 1)
; privacy ('all 'friends or listname)

;(defun post-new-inventory-item ( 


(defun nearby-inventory-items (type &key base (subtag-count 4) (distance 50) q)
  (with-location
    (let ((nearby (sort
                    (if q
                      (result-id-intersection
                        (geo-index-query (case type
                                           ('resource *resource-geo-index*)
                                           (t *request-geo-index*))
                                         *latitude*
                                         *longitude*
                                         distance)
                        (stem-index-query (case type
                                           ('resource *resource-stem-index*)
                                           (t *request-stem-index*))
                                          q))
                      (geo-index-query (case type
                                         ('resource *resource-geo-index*)
                                         (t *request-geo-index*))
                                         *latitude*
                                         *longitude*
                                       distance)) 
                    #'> :key #'inventory-rank))
          (items nil))
      (let ((tags (make-hash-table :test 'equalp)))
        (dolist (item nearby)
          (dolist (tag (result-tags item))
            (push item (gethash tag tags))))

        (if base
          ; get each base tag's list of items
          ; get intersection of those lists
          ; remove base tags from hashtable
          ; set all remaining tags lists to be intersection of tag list and previous intersection
          (progn
            (setf items (iter (for tag in base)
                              (reducing (gethash tag tags) by #'result-id-intersection)
                              (remhash tag tags))) 
            (iter (for (tag tag-items) in-hashtable tags)
                  (let ((new-items (intersection tag-items items :key #'result-id)))
                    (if new-items
                      (setf (gethash tag tags) new-items)
                      (remhash tag tags)))))
          
          (setf items nearby))
                

        ; for each tag, number of contents + ordered list of subtags (up to 4)
        
        (values (iter (for (tag tag-items) in-hashtable tags)
                      (collect (list tag
                                     (length tag-items)
                                     (when (cdr tag-items)
                                       (let* ((subtags (sort
                                                         (iter (for (subtag subtag-items) in-hashtable tags)
                                                               (unless (string= tag subtag)
                                                                 (awhen (intersection tag-items subtag-items :key #'result-id)
                                                                   (collect (cons subtag (length it))))))
                                                         #'> :key #'cdr))
                                              (top-subtags (subseq subtags 0
                                                                   (min (length subtags) subtag-count))))
                                         (if (< subtag-count (length subtags))
                                           (append (sort (subseq top-subtags 0 (- subtag-count 1))
                                                         #'string< :key #'car)
                                                   (list
                                                     (cons
                                                       "more"
                                                       (reduce #'+ (subseq subtags (- subtag-count 1)) :key #'cdr))))
                                           (sort top-subtags #'string< :key #'car)))))))
                                                       
                items)))))

(defun nearby-inventory-top-tags (type &key (count 9) (more t) base (subtag-count 4) q)
  (multiple-value-bind (nearby items)
      (nearby-inventory-items type :base base :subtag-count subtag-count :q q)
    (let* ((tags (sort (if base
                         nearby
                         (remove-if-not #'top-tag-p nearby :key #'first))
                       #'> :key #'second))
           (top-tags (subseq tags 0 (min count (length tags))))) 
      (cond
        ((and more (> (length tags) (+ count 1)))
         (values
           (append (sort top-tags #'string< :key #'first)
                   (list
                     (let* ((more-tags (subseq tags count))
                            (subtags (iter (for tag in (subseq more-tags 0
                                                               (min 6 (length more-tags))))
                                           (collect
                                             (cons (first tag) (second tag))))))
                       (list "etc"
                         (reduce #'+ more-tags :key #'second)
                         (if (< 6 (length more-tags))
                           (append (sort (subseq subtags 0 5) #'string< :key #'car)
                                   (list
                                     (cons "more" (reduce #'+ (subseq more-tags 5) :key #'second))))
                           (sort subtags #'string< :key #'car))))))
                 items))
        ((and more (= (length tags) (+ count 1)))
         (values (sort tags #'string< :key #'first) items))
        (t
         (values (sort top-tags #'string< :key #'first) items))))))
