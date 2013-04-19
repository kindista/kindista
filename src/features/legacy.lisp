;;; Copyright 2012-2013 CommonGoods Network, Inc.
;;;
;;; This file is part of Kindista.
;;;
;;; Kindista is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Affero General Public License as published
;;; by the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Kindista is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public
;;; License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with Kindista.  If not, see <http://www.gnu.org/licenses/>.

(in-package :kindista)

(defun create-gift (&key giver recipients text (created (get-universal-time)))
  (insert-db `(:type :gift
               :giver ,giver
               :recipients ,recipients
               :text ,text
               :created ,created)))

(defun index-gift (id data)
  (let* ((giver (db (getf data :giver)))
         (created (getf data :created))
         (recipients (getf data :recipients))
         (people (cons (getf data :giver) recipients))
         (result (make-result :latitude (getf giver :lat)
                              :longitude (getf giver :long)
                              :people people
                              :time created
                              :type :gift
                              :id id)))
    
    (with-locked-hash-table (*db-results*)
      (setf (gethash id *db-results*) result))

    (geo-index-insert *activity-geo-index* result)

    (with-locked-hash-table (*activity-person-index*)
      (dolist (person people)
        (asetf (gethash person *activity-person-index*)
               (sort (push result it) #'> :key #'result-time))))

    (dolist (subject people)
      (let ((user (db subject)))
        (geo-index-insert *activity-geo-index* (make-result :latitude (getf user :lat)
                                                            :longitude (getf user :long)
                                                            :people people
                                                            :id id
                                                            :time created))))))

(defun delete-gift (id)
  (let* ((result (gethash id *db-results*))
         (data (db id))
         (people (cons (getf data :giver) (getf data :recipients))))

    (with-locked-hash-table (*db-results*)
      (remhash id *db-results*))

    (with-locked-hash-table (*activity-person-index*)
      (dolist (person people)
        (asetf (gethash person *activity-person-index*)
               (remove result it))))

    (geo-index-remove *activity-geo-index* result)
    (remove-from-db id)))


(defun get-gift (id)
  (setf id (parse-integer id))
  (let ((it (db id)))
    (if (eq (getf it :type) :gift)
      (require-user
        (standard-page
          "Gifts"
          (html
            (str (gift-activity-item (make-result :id id
                                                  :time (getf it :created)
                                                  :people (cons (getf it :giver) (getf it :recipients)))
                                                  :next-url (script-name*))))))
      (not-found))))

(defun post-gift (id)
  (require-active-user
    (setf id (parse-integer id)) 
    (aif (db id)
      (cond
        ((and (post-parameter "love")
              (eq (getf it :type) :gift))
         (love id)
         (see-other (or (post-parameter "next") (referer))))
        ((and (post-parameter "unlove")
              (eq (getf it :type) :gift))
         (unlove id)
         (see-other (or (post-parameter "next") (referer)))))
      (not-found))))

(defvar *legacy-identity-map* ())
(defvar *legacy-offer-map* ())
(defvar *legacy-request-map* ())
(defvar *legacy-gift-map* ())
(defvar *legacy-map-stream*)

(defun read-legacy-map ()
  (with-open-file (in (s+ +db-path+ "legacymap") :if-does-not-exist nil)
    (when in
      (with-standard-io-syntax
        (loop
          (handler-case
            (let ((item (read in)))
              (case (first item)
                (:gift
                  (push (cons (second item) (third item)) *legacy-gift-map*))
                (:offer
                  (push (cons (second item) (third item)) *legacy-offer-map*))
                (:request
                  (push (cons (second item) (third item)) *legacy-request-map*))
                (:identity
                  (push (cons (second item) (third item)) *legacy-identity-map*))))
          (end-of-file (e) (declare (ignore e)) (return)))))))
  (setf *legacy-map-stream* (open (s+ +db-path+ "legacymap") :if-exists :append :if-does-not-exist :create :direction :output)))

(defun write-legacy-map (type old-id new-id)
  (case type
    (:gift
      (push (cons old-id new-id) *legacy-gift-map*))
    (:offer
      (push (cons old-id new-id) *legacy-offer-map*))
    (:request
      (push (cons old-id new-id) *legacy-request-map*))
    (:identity
      (push (cons old-id new-id) *legacy-identity-map*))) 
  (with-standard-io-syntax
    (prin1 (list type old-id new-id) *legacy-map-stream*)
    (fresh-line *legacy-map-stream*))
  (fsync *legacy-map-stream*))

(defun import-identities (path)
  (let ((identities (sort (map-over-file #'copy-list path) #'< :key #'car)))
    (setf identities (cons (second identities) (remove 2 identities :key #'car)))
    (iter (for (id pw names emails (long lat) contacts bio) in identities)
          (let ((newid (insert-db `(:type :person
                                          :name ,(car names)
                                          :aliases ,(cdr names)
                                          :emails ,emails
                                          :bio-summary ,bio
                                          :lat ,lat
                                          :long ,long
                                          :k1 t
                                          :active t
                                          :location t
                                          :help t
                                          :pass ,pw
                                          :created 3540679452
                                          :notify-gratitude t
                                          :notify-message t
                                          :notify-kindista t))))
            (write-legacy-map :identity id newid)))

    (labels ((id-lookup (id)
               (cdr (assoc id *legacy-identity-map*))))
      (iter (for (id pw names emails (long lat) contacts bio) in identities)
            (modify-db (id-lookup id) :following (mapcar #'id-lookup contacts))))))

(defun import-gifts (path)
  (let ((gifts (map-over-file #'copy-list path)))

    (labels ((id-lookup (id)
               (cdr (assoc id *legacy-identity-map*))))
      (iter (for (id giver recipients time text comments) in gifts)
            (unless (member id *legacy-gift-map* :key #'car)
              (let ((newid (insert-db `(:type :gift
                                              :giver ,(id-lookup giver)
                                              :recipients ,(mapcar #'id-lookup recipients)
                                              :created ,time
                                              :text ,text))))
                (write-legacy-map :gift id newid)
                (iter (for (id time text) in comments)
                      (insert-db (list :type :comment
                                       :on newid
                                       :by (id-lookup id)
                                       :text text
                                       :created time)))))))))

(defun import-offers (path)
  (let ((offers (map-over-file #'copy-list path)))

    (labels ((id-lookup (id)
               (cdr (assoc id *legacy-identity-map*))))
      (iter (for (id user text time expires) in offers)
            (unless (member id *legacy-offer-map* :key #'car)
              (let ((newid (insert-db `(:type :offer
                                              :by ,(id-lookup user)
                                              :created ,time
                                              :expires ,expires
                                              :text ,text))))
                (write-legacy-map :offer id newid)))))))

(defun import-requests (path)
  (let ((requests (map-over-file #'copy-list path)))

    (labels ((id-lookup (id)
               (cdr (assoc id *legacy-identity-map*))))
      (iter (for (id user text time expires) in requests)
            (unless (member id *legacy-request-map* :key #'car)
              (let ((newid (insert-db `(:type :request
                                              :by ,(id-lookup user)
                                              :created ,time
                                              :expires ,expires
                                              :text ,text))))
                (write-legacy-map :request id newid)))))))

(defun import-messages (path)
  (let ((conversations (map-over-file #'copy-list path)))
    (labels ((id-lookup (id)
               (cdr (assoc id *legacy-identity-map*))))
      (iter (for (ids messages) in conversations)
            (let ((id (insert-db `(:type :conversation
                                         :people ,(mapcar #'list (mapcar #'id-lookup ids))
                                         :created ,(first (sort (mapcar #'second messages) #'<))
                                         :public t))))
              (iter (for (user time message) in (nreverse messages))
                    (create-comment :on id :by (id-lookup user) :text message :time time)))))))

(defun import-avatars (path)
  (dolist (pair *legacy-identity-map*)
    (let ((file (strcat path "/" (car pair) ".jpg")))
      (let ((r1 (run-program "/usr/bin/convert"
                             (list
                               file
                               "-scale"
                               "300x300"
                               (strcat +avatar-path+ (cdr pair) ".jpg"))))
            (r2 (run-program "/usr/bin/convert"
                             (list
                               file
                               "-scale"
                               "100x100"
                               (strcat +avatar-path+ (cdr pair) ".png")))))
           (when (and (eql 0 (process-exit-code r1))
                        (eql 0 (process-exit-code r2)))
             (modify-db (cdr pair) :avatar t))))))

(defun map-over-file (fn path)
  (with-open-file (in path :if-does-not-exist nil)
    (when in
      (with-standard-io-syntax
        (iter
          (handler-case
            (collect (funcall fn (read in)))
            (end-of-file (e) (declare (ignore e)) (finish))))))))
