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
    (pushnew userid (gethash item-id *love-index*))))

(defun deindex-love (item-id userid)
  (with-locked-hash-table (*love-index*)
    (asetf (gethash item-id *love-index*)
           (remove userid it))))

(defun love (id &optional (userid *userid*))
  (when id
    (amodify-db userid :loves (pushnew id it))
    (index-love id userid)))

(defun unlove (id &optional (userid *userid*))
  (when id
    (amodify-db userid :loves (remove id it))
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

(defun loves (item-id)
  (gethash item-id *love-index*))

(defun users-who-love-item-html
  (userids
   item-id
   item-url
   &key (userid *userid*)
        (contact-count 5)
        (links t)
        (func #'person-link)
   &aux (self (find userid userids))
        (user (or *user* (db userid)))
        (others (remove self userids))
        (my-contacts (intersection others (getf user :following)))
        (named-contacts (subseq my-contacts 0 (min (length my-contacts) contact-count)))
        (other-lovers (set-difference others named-contacts))
        (unnamed-lovers-count (length other-lovers))
        (show-all-names (eql (get-parameter-integer "show-loves") item-id))
        (other (when (and other-lovers
                          (> (length userids) unnamed-lovers-count))
                 " other"))
        (unnamed-lovers-text (when (> unnamed-lovers-count 0)
                              (if (> unnamed-lovers-count 1)
                                (strcat* unnamed-lovers-count other " people")
                                (strcat* "1" other  " person"))))
        (unnamed-lovers-link (awhen unnamed-lovers-text
                               (html
                                 (:a :href (url-compose item-url "show-loves" item-id)
                                   (str it))))))
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
                                   (if show-all-names
                                     (mapcar #'format-function other-lovers)
                                     (list unnamed-lovers-link))))))
      (str (if (and (= (length userids) 1) (not self))
             " loves this"
             " love this"))))))
