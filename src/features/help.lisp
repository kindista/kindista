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

(defun create-feedback (&key type (by *userid*) title text (time (get-universal-time)))
  (insert-db (list :type type
                   :title title
                   :text text
                   :by by
                   :created time)))

(defun index-feedback (id data)
  (let* ((by (getf data :by))
         (created (getf data :created))
         (type (getf data :type))
         (result (make-result :people (list by)
                              :time created
                              :type type
                              :id :id)))

    (with-locked-hash-table (*db-results*)
      (setf (gethash id *db-results*) result))))

(defun help-tabs-html (&key tab)
  (html
    (:menu :class "horiz"
     (:strong "actions")
     (:li (:a :href (strcat "/people/" +kindista-id+ "/reputation") "express gratitude"))
     (:li
       (:form :method "POST" :action "/conversations/new"
        (:input :type "hidden" :name "next" :value "/help/feedback")
        (:input :type "hidden" :name "single-recipient" :value "t")
        (:button :class "simple-link" :type "submit" :name "add" :value +kindista-id+ "contact kindista"))))
    (:menu :class "bar"
      (if (eql tab :feedback)
        (htm (:li :class "selected" "Feedback"))
        (htm (:li (:a :href "/help/feedback" "Feedback"))))
      (if (eql tab :faqs)
        (htm (:li :class "selected" "Frequent Questions"))
        (htm (:li (:a :href "/help/faqs" "Frequent Questions")))))))

(defun faqs-html ()
  (html
    (str (help-tabs-html :tab :faqs))
    (:div :class "legal faqs"
      (str (markdown-file (s+ +markdown-path+ "faq.md"))))))

(defun feedback-html ()
  (html
    (str (help-tabs-html :tab :feedback))
    (:div :class "item"
     (:h4 "Ask a question, report a problem, or suggest a new feature:")
     (:form :method "post" :action "/help/feedback/new"
       (:input :type "hidden" :name "next" :value (strcat *base-url* "/reputation"))
       (:table :class "post"
        (:tr
          (:td (:textarea :cols "1000" :rows "4" :name "text"))
          (:td
            (:button :class "yes" :type "submit" :class "submit" :name "create" "Post"))))))  
))

(defun go-help ()
  (see-other "/help/feedback"))

(defun get-faqs ()
  (standard-page
    "Frequently Asked Questions"
    (html
      (str (faqs-html)))
    :selected "help"
    :right (html
             (str (donate-sidebar))
             (str (invite-sidebar)))))

(defun get-feedback ()
  (standard-page
    "Feedback"
    (html
      (str (feedback-html)))
    :selected "help"
    :right (html
             (str (donate-sidebar))
             (str (invite-sidebar)))))

