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

(defun create-gift (&key giver recipients text (created (get-universal-time)) old-id)
  (insert-db `(:type :gift
               :giver ,giver
               :recipients ,recipients
               :text ,text
               :created ,created
               :old-id ,old-id)))

(defun index-gift (id data)
  (let* ((giver (db (getf data :giver)))
         (created (getf data :created))
         (recipients (getf data :recipients))
         (people (cons (getf data :giver) recipients))
         (result (make-result :latitude (getf author :lat)
                              :longitude (getf author :long)
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

    (dolist (subject subjects)
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
          "First few words... | Kindista"
          (html
            (str (gift-activity-item (make-result :id id
                                                  :time (getf it :created)
                                                  :people (cons (getf it :giver) (getf it :recipients)))
                                                  :next-url (script-name*))))))
      (standard-page "Not found" "not found"))))

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
      (standard-page "Not found" "not found"))))

(defun import-gifts (filename)
  (let ((existing ()))
    (iter (for (key item) in-hashtable *db*)
      (awhen (getf item :old-id)
        (push it existing)))

    (with-open-file (in filename :if-does-not-exist nil)
      (when in
        (with-standard-io-syntax
          (with-locked-hash-table (*db*)
            (loop
              (handler-case
                (let ((item (read in)))
                  (unless (member (parse-integer (first item)) existing)
                    (when (third item)
                      (create-gift :giver (parse-integer (first (second item)))
                                   :recipients (iter (for id in (third item) by #'cddr)
                                                     (collect (parse-integer id)))
                                   :text (second (second item))
                                   :created (parse-integer (third (second item)))
                                   :old-id (parse-integer (first item))))))
                (end-of-file (e) (declare (ignore e)) (return))))))))))
