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

(defun new-feedback-notice-handler ()
  (send-feedback-notification-email (getf (cddddr *notice*) :id)))

(defun create-feedback (&key (by *userid*) text (time (get-universal-time)))
  (let ((id (insert-db (list :type :feedback
                             :text text
                             :by by
                             :created time))))

    (notice :new-feedback :time time :id id)
    id))

(defun index-feedback (id data)
  (let* ((by (getf data :by))
         (created (getf data :created))
         (result (make-result :people (list by)
                              :time created
                              :type :feedback
                              :id id)))

    (with-mutex (*feedback-mutex*)
      (setf *feedback-index* (sort (cons result *feedback-index*)
                                   #'> :key #'result-time)))

    (with-locked-hash-table (*db-results*)
      (setf (gethash id *db-results*) result))))

(defun delete-feedback (id)
  (let ((result (gethash id *db-results*)))

    (delete-comments id)

    (with-locked-hash-table (*db-results*)
      (remhash id *db-results*))

    (with-mutex (*feedback-mutex*)
      (asetf *feedback-index* (remove result it)))

    (remove-from-db id)))

(defun help-tabs-html (&key tab)
  (html
    (:menu :class "horiz"
     (:strong "actions")
     (:li (:a :href (strcat "/people/" +kindista-id+ "/reputation") "express gratitude"))
     (:li
       (:form :method "POST" :action "/conversations/new"
        (:input :type "hidden" :name "next" :value "/feedback")
        (:input :type "hidden" :name "single-recipient" :value "t")
        (:button :class "simple-link" :type "submit" :name "add" :value +kindista-id+ "contact kindista"))))
    (:menu :class "bar"
      (if (eql tab :feedback)
        (htm (:li :class "selected" "Feedback"))
        (htm (:li (:a :href "/feedback" "Feedback"))))
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
     (:form :method "post" :action "/feedback"
       (:table :class "post"
        (:tr
          (:td (:textarea :cols "1000" :rows "4" :name "text"))
          (:td
            (:button :class "yes" :type "submit" :class "submit" :name "create" "Post")))))) 

     (dolist (result *feedback-index*)
       (str (feedback-card (result-id result))))))

(defun go-help ()
  (see-other "/feedback"))

(defun get-faqs ()
  (standard-page
    "Frequently Asked Questions"
    (html
      (str (faqs-html)))
    :selected "help"
    :right (html
             (str (donate-sidebar))
             (str (invite-sidebar)))))

(defun get-feedbacks ()
  (require-user
    (standard-page
      "Feedback"
      (html
        (str (feedback-html)))
      :selected "help"
      :right (html
               (str (donate-sidebar))
               (str (invite-sidebar))))))

(defun post-feedbacks ()
  (require-user
    (let ((text (post-parameter "text")))
      (cond
        ((and text (not (string= text "")))
         (create-feedback :text text))
        (t
         (flash "Please provide some text for your feedback." :error t))))
    (see-other "/feedback")))

(defun get-feedback (id)
  (require-user
    (let* ((id (parse-integer id))
           (data (db id)))
      (if (eq (getf data :type) :feedback)
        (standard-page
          "Feedback"
          (html
            (str (feedback-card id))))
        (not-found)))))

(defun post-feedback (id)
  (require-user
    (let* ((id (parse-integer id))
           (data (db id)))
      (if (eq (getf data :type) :feedback)
        (cond
          ((post-parameter "delete")
           (confirm-delete :url (script-name*)
                           :type "feedback"
                           :text (getf data :text)
                           :next-url (referer)))
          ((post-parameter "really-delete")
           (delete-feedback id)
           (flash "Your feedback has been deleted!")
           (see-other (or (post-parameter "next") "/feedback")))
          ((post-parameter "love")
           (love id)
           (see-other (or (post-parameter "next") (referer))))
          ((post-parameter "unlove")
           (unlove id)
           (see-other (or (post-parameter "next") (referer))))
          ((and (post-parameter "text")
                (getf *user* :admin))
           (create-comment :on id :text (post-parameter "text"))
           (see-other "/feedback"))
          (t
           (flash "WTF?" :error t)
           (see-other "/feedback")))))))
