;;; Copyright 2012-2016 CommonGoods Network, Inc.
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

(defun index-love (item-id userid)
  (with-locked-hash-table (*love-index*)
    (pushnew item-id (gethash userid *love-index*))))

(defun deindex-love (item-id userid)
  (with-locked-hash-table (*love-index*)
    (asetf (gethash userid *love-index*)
           (remove item-id it))))

(defun love (id &optional (userid *userid*))
  (when id
    (amodify-db id :loved-by (pushnew userid it))
    (index-love id userid)))

(defun unlove (id &optional (userid *userid*))
  (when id
    (amodify-db id :loved-by (remove userid it))
    (deindex-love id userid)))

(defun post-love (id)
  (require-active-user
    (let* ((id (safe-parse-integer id))
           (item (db id))
           (type (getf item :type)))
      (cond
        ((not (find type '(:offer :request :gratitude :event :feedback)))
         (setf (return-code*) +http-bad-request+))
        ((post-parameter "love")
         (love id))
        ((post-parameter "unlove")
         (unlove id))))
    (see-other (referer))))

(defun loves (userid)
  (gethash userid *love-index*))

(defun move-loves-to-objects ()
  (dolist (id (hash-table-keys *db-results*))
    (let ((data (db id)))
      (when (getf data :loves)
        (dolist (loved-id (getf data :loves))
          (amodify-db loved-id :loved-by (cons id it)))
        (modify-db id :loves nil))))
  (load-db))

(defun bulk-love-item
  (item-id
   &optional (count 20)
   &aux (test-data '(0 1 3 19885 6 20 25 98 101 105 110 115 117 124 129 135 147 162 171 180))
        (subset (subseq test-data 0 count)))
  (dolist (userid subset)
    (love item-id userid)))

(defun list-loved-by-html
  (userids
   &key (userid *userid*)
        (contact-count 5)
        (links t)
        (func #'person-link)
   &aux (self (find userid userids))
        (user (or *user* (db userid)))
        (others (remove self userids))
        (my-contacts (intersection others (getf user :following)))
        (named-contacts (subseq my-contacts 0 (min (length my-contacts) contact-count)))
        (unnamed-lovers (set-difference others named-contacts))
        (unnamed-lovers-count (awhen unnamed-lovers (strcat (length it) " others"))))
  "lists users who love an item.
   includes self as 'you', then the named-links of up to 5 of the user's contacts
   then the number of remaining users with a link to all"
  (flet ((format-function (id)
            (if links (apply func (list id))
                      (db id :name))))
  (html
    (:div :class "love-list"
      (str (format nil
                   *english-list*
                   (remove nil
                           (append (list (when self "You"))
                                   (mapcar #'format-function named-contacts)
                                   (list unnamed-lovers-count)))))))))
