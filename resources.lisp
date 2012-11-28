(in-package :kindista)

(defun create-offer (&key by text)
  (insert-db (list :type :offer
                             :by by
                             :text text
                             :created (get-universal-time))))

(defun index-offer (id data)
  (timeline-insert (getf data :author) (getf data :created) id)
  (when (and (getf data :lat) (getf data :long) (getf data :created))
    (activity-geo-index-insert (getf data :lat) (getf data :long) id (getf data :created))))

(defun intersection-fourth (list1 list2)
  (intersection list1 list2 :key #'fourth))

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
        
        (setf items (iter (for item in nearby)
                          (collect item))))
              

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
                                                     

              (mapcar #'fourth (sort items #'> :key #'first))))))

(defun nearby-resources-top-tags (index &key (count 9) (more t) base (user *user*) (subtag-count 4))
  (multiple-value-bind (nearby items)
      (nearby-resources index :base base :user user :subtag-count subtag-count)
    (let* ((tags (sort (remove-if-not #'top-tag-p nearby :key #'first) #'> :key #'second))
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
