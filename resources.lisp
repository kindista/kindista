(in-package :kindista)

(defun create-offer (&key by text)
  (insert-db (list :type :offer
                             :by by
                             :text text
                             :created (get-universal-time))))

(defun index-offer (id data)
  (let* ((by (getf data :by))
         (lat (or (getf data :lat) (getf (db by) :lat)))
         (long (or (getf data :long) (getf (db by) :long))))

    (timeline-insert (getf data :author) (getf data :created) id)
    
    (with-locked-hash-table (*offer-index*)
      (setf (gethash by *offer-index*)
            (push id (gethash by *offer-index*))))

    (with-locked-hash-table (*offer-stem-index*)
      (dolist (stem (stem-text (getf data :text)))
        (push (list (getf data :created)
                    lat
                    long
                    id)
              (gethash stem *offer-stem-index*))))

    (resource-geo-index-insert *offer-geo-index*
                               lat
                               long
                               id
                               (getf data :created)
                               (getf data :tags))
    (activity-geo-index-insert lat
                               long
                               id
                               (getf data :created)
                               (list by))))

(defun nearby-resources (index &key base (user *user*) (subtag-count 4))
  (let ((nearby (geo-index-query index
                                 (getf user :lat)
                                 (getf user :long)
                                 (or (getf user :distance) 50)))
        (items nil))
    (let ((tags (make-hash-table :test 'equalp)))
      (dolist (item nearby)
        (dolist (tag (fifth item))
          (push item (gethash tag tags))))

      (if base
        ; get each base tag's list of items
        ; get intersection of those lists
        ; remove base tags from hashtable
        ; set all remaining tags lists to be intersection of tag list and previous intersection
        (progn
          (setf items (iter (for tag in base)
                            (reducing (gethash tag tags) by #'intersection-fourth)
                            (remhash tag tags))) 
          (iter (for (tag tag-items) in-hashtable tags)
                (let ((new-items (intersection tag-items items :key #'fourth)))
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
                                                               (awhen (intersection tag-items subtag-items :key #'fourth)
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
                                                     

              (mapcar #'fourth (sort items #'> :key #'resource-rank))))))

(defun nearby-resources-top-tags (index &key (count 9) (more t) base (user *user*) (subtag-count 4))
  (multiple-value-bind (nearby items)
      (nearby-resources index :base base :user user :subtag-count subtag-count)
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
