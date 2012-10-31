(in-package :kindista)

(defun create-testimonial (&key author subjects text)
  (insert-db `(:type :testimonial
               :author ,author
               :subjects ,subjects
               :text ,text
               :created ,(get-universal-time))))

(defun index-testimonial-for-user (userid testimonialid created)
  (timeline-insert userid created testimonialid)
  (let ((user (db userid)))
    (geo-index-insert (getf user :lat)
                      (getf user :long)
                      testimonialid
                      created)))

(defun index-testimonial (id data)
  ; add to user's testimonials
  ; add to author's written testimonials?
  ; add to geo index for each subject's location and author's location
  ; add to author's activity?

  (index-testimonial-for-user (db (getf data :author)) id (getf data :created))

  (with-locked-hash-table (*testimonial-index*)
    (dolist (subject (getf data :subjects))
      (index-testimonial-for-user (db subject) id (getf data :created))
      (setf (gethash subject *testimonial-index*)
            (cons id (gethash subject *testimonial-index*))))))

