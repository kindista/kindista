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

(defun create-contact-notification (&key follower contact)
  (let* ((time (get-universal-time))
         (recipient-mailboxes (maphash #'list (mailbox-ids (list contact))))
         (id (insert-db (list :type :contact-n
                              :follower follower
                              :mailboxes (list :unread recipient-mailboxes)
                              :time time))))


    id))

(defun add-contact (new-contact-id userid)
  (amodify-db userid :following (cons new-contact-id it))
  (with-locked-hash-table (*followers-index*)
    (push userid (gethash new-contact-id *followers-index*)))
  (remove-suggestion new-contact-id userid)
  (remove-suggestion new-contact-id userid :hidden t)
  (unless (string= (strcat +base-url+ "people/"
                           (username-or-id new-contact-id))
                   (referer))
    (flash (html (:a :href (strcat +base-url+ "people/"
                                   (username-or-id new-contact-id))
                     (str (db new-contact-id :name)))
                 " has been added to your contacts.")))
   (create-contact-notification :follower userid :contact new-contact-id))

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
