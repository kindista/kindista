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

(defun sign-users-up-for-new-contact-notifications ()
  "To be run when we implement contact notifications"
  (dolist (id (hash-table-keys *db*))
    (let ((data (db id)))
      (when (or (eq (getf data :type) :person)
                (eq (getf data :type) :deleted-person-account))
        (modify-db id :notify-new-contact (and (getf data :notify-message)
                                               (getf data :active)))))))

(defun create-contact-notification (&key follower contact)
  (let* ((time (get-universal-time))
         (recipient-mailboxes (maphash #'list (mailbox-ids (list contact))))
         (id (insert-db (list :type :contact-n
                              :follower follower
                              :mailboxes (list :unread recipient-mailboxes)
                              :time time))))


    id))

;; this doesn't appear be used for anything yet
(defun index-contact-notification (id data)
  (let ((result (make-result :id id
                             :type :contact-n
                             :time (getf data :time))))
    (index-message id data)
    (push result (gethash (getf data :object) *person-notification-index*))))

(defun add-contact (new-contact-id userid)
  (unless (find new-contact-id (db userid :following))
    (amodify-db userid :following (cons new-contact-id it))
    (with-locked-hash-table (*followers-index*)
      (push userid (gethash new-contact-id *followers-index*)))
    (remove-suggestion new-contact-id userid)
    (remove-suggestion new-contact-id userid :hidden t)
    (unless (or (not *userid*) ;if done from the REPL
                (string= (strcat +base-url+
                                   "people/"
                                   (username-or-id new-contact-id))
                       (referer)))
      (flash (html (str (person-link new-contact-id))
                   " has been added to your contacts."))))

  ;;is this really how we want to implement contact notifications?
  ;(create-contact-notification :follower userid :contact new-contact-id)
  )

(defun remove-contact (contact-id userid)
  (amodify-db *userid* :following (remove contact-id it))
  (with-locked-hash-table (*followers-index*)
    (remove userid (gethash contact-id *followers-index*)))
  (amodify-db *userid* :hidden-suggested-contacts (push  (cons contact-id (quick-rank contact-id)) it)))

(defun post-contacts ()
  (require-user
    (let ((contacts (getf *user* :following)))
      (cond
        ((scan +number-scanner+ (post-parameter "add"))
         (let ((id (parse-integer (post-parameter "add"))))
           (unless (member id contacts)
             (add-contact id *userid*)))
         (see-other (or (post-parameter "next") "/home")))

        ((scan +number-scanner+ (post-parameter "remove"))
         (let ((id (parse-integer (post-parameter "remove"))))
           (when (member id contacts)
             (remove-contact id *userid*)))
         (see-other (or (post-parameter "next") "/home")))

        (t
         (flash "Sorry, couldn't make sense of that request.")
         (see-other "/home"))))))
