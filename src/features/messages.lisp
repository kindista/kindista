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

(defun ellipsis (text &optional (length 160))
  (let ((newtext (subseq text 0 (min (length text) length))))
    (if (> (length text) length)
      (s+ newtext "...")
      newtext)))

(defun result-gratitude-p (result)
  (eq (result-type result) :gratitude))

(defun inbox-items (&key (page 0) (count 20))
  (let ((start (* page count))
        (items (sort (append (gethash *userid* *person-conversation-index*)
                             (gethash *userid* *person-notification-index*)
                             (remove-if-not #'result-gratitude-p (gethash *userid* *activity-person-index*)))
                     #'> :key #'result-time)))
    (html
      (iter (for i from 0 to (+ start count))
            (cond
              ((< i start)
               (setf items (cdr items)))
              ((and (>= i start) items)
               (let* ((item (car items))
                      (item-data (db (result-id item))))
                 (case (result-type item)
                   (:conversation
                     (let* ((id (result-id item))
                            (latest (latest-comment id))
                            (latest-seen (cdr (assoc *userid* (getf item-data :people))))
                            (comment-data (db latest))
                            (comments (length (gethash id *comment-index*))))
                       (str
                         (card
                           (html
                             (str (h3-timestamp (result-time item)))
                             (:p :class "people"
                               (cond
                                 ((eql (getf comment-data :by) *userid*)
                                  (str "↪ "))
                                 ((not (eql latest latest-seen))
                                  (str "• ")))

                               (str (name-list (remove *userid*
                                                       (cons (getf comment-data :by)
                                                             (remove (getf comment-data :by)
                                                                     (mapcar #'car (getf item-data :people))))))))
                             (:p :class "text"
                               (:span :class "title"
                                 (:a :href (strcat "/conversations/" id) (str (ellipsis (getf item-data :subject) 30)))
                                 (when (> comments 1)
                                   (htm
                                     " (" (str comments) ")")))
                               " - "
                               (:a :href (strcat "/conversations/" id)
                                (str (ellipsis (getf comment-data :text))))))))))
                   (:contact-n
                     (str
                      (card
                        (html
                          (str (h3-timestamp (result-time item)))
                          (:p (str (person-link (getf item-data :subject))) " added you as a contact.")))))

                   (:gratitude
                     (unless (eql (getf item-data :author) *userid*)
                       (str
                        (card
                          (html
                            (str (h3-timestamp (result-time item)))
                            (:p (str (person-link (getf item-data :author))) " shared " (:a :href (strcat "/gratitude/" (result-id item)) "gratitude") " for you."))))))))
               (setf items (cdr items)))

              ((and (eql i start)
                    (not items))
               (htm
                 (:div :class "small card"
                   (:em "No results")))
               (finish)))
            
            (finally
              (when (or (> page 0) (cdr items))
                (htm
                  (:div :class "item"
                   (when (> page 0)
                     (htm
                       (:a :href (strcat "/messages?p=" (- page 1)) "< previous page")))
                   "&nbsp;"
                   (when (cdr items)
                     (htm
                       (:a :style "float: right;" :href (strcat "/messages?p=" (+ page 1)) "next page >")))))))))))

(defun get-messages ()
  (require-user
    (standard-page

      "Messages"

      (html
        (:div :class "card"
          (str (menu-horiz "actions"
                           (html (:a :href "/conversations/new" "start a new conversation")))))


        (str (inbox-items :page (if (scan +number-scanner+ (get-parameter "p"))
                                  (parse-integer (get-parameter "p"))
                                  0))))

      :selected "messages")))
