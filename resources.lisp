(in-package :kindista)

(defun create-offer (&key by text)
  (insert-db (list :type :offer
                             :by by
                             :text text
                             :created (get-universal-time))))

(defun index-offer (id data)
  (let* ((by (getf data :by))
         (result (make-result :latitude (getf data :lat)
                              :longitude (getf data :long)
                              :id id
                              :type :offer
                              :people (list by)
                              :created (getf data :created)
                              :tags (getf data :tags))))

    (timeline-insert (getf data :author) result)
    
    (with-locked-hash-table (*request-index*)
      (setf (gethash by *request-index*)
            (push id (gethash by *request-index*))))

    (with-locked-hash-table (*request-stem-index*)
      (dolist (stem (stem-text (getf data :text)))
        (push result (gethash stem *request-stem-index*))))

    (with-locked-hash-table (*activity-person-index*)
      (push result (gethash by *activity-person-index*)))

    (geo-index-insert *request-geo-index* result)
    (geo-index-insert *activity-geo-index* result)))

(defun nearby-resources (type &key base (subtag-count 4) (distance 50) q)
  (with-location
    (let ((nearby (sort
                    (if q
                      (result-id-intersection
                        (geo-index-query (case type
                                           ('offer *offer-geo-index*)
                                           (t *request-geo-index*))
                                         *latitude*
                                         *longitude*
                                         distance)
                        (stem-index-query (case type
                                           ('offer *offer-stem-index*)
                                           (t *request-stem-index*))
                                          q))
                      (geo-index-query (case type
                                         ('offer *offer-geo-index*)
                                         (t *request-geo-index*))
                                         *latitude*
                                         *longitude*
                                       distance)) 
                    #'> :key #'resource-rank))
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

(defun nearby-resources-top-tags (type &key (count 9) (more t) base (subtag-count 4) q)
  (multiple-value-bind (nearby items)
      (nearby-resources type :base base :subtag-count subtag-count :q q)
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
