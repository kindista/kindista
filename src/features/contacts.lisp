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

(defun new-contact-notice-handler ()
  (send-contact-notification (getf (cddddr *notice*) :from-id)
                             (getf (cddddr *notice*) :to-id)))

(defun sign-users-up-for-new-contact-notifications
  (&aux (modifications 0))
  "To be run when we implement contact notifications"
  (dolist (id (hash-table-keys *db*))
    (let* ((data (db id))
           (notify-new-contact (and (getf data :notify-message)
                                    (getf data :active))))
      (when (and (or (eq (getf data :type) :person)
                     (eq (getf data :type) :deleted-person-account))
                 (or (not (find :notify-new-contact data))
                     (and notify-new-contact
                          (not (getf data :notify-new-contact)))))
        (modify-db id :notify-new-contact notify-new-contact)
        (incf modifications))))
  modifications)

(defun add-contact
  (new-contact-id
   userid
   &aux (time (get-universal-time)))
  (unless (find new-contact-id (db userid :following))
    (amodify-db userid :following (cons new-contact-id it))
    (with-locked-hash-table (*followers-index*)
      (push userid (gethash new-contact-id *followers-index*)))
    (remove-suggestion new-contact-id userid)
    (remove-suggestion new-contact-id userid :hidden t)
    (notice :new-contact :time time
                         :from-id userid
                         :to-id new-contact-id)
    (unless (or (not *userid*) ;if done from the REPL
                (string= (strcat +base-url+
                                   "people/"
                                   (username-or-id new-contact-id))
                       (referer))
                ;; if adding userid to host's contacts when RSVP'ing to
                ;; an invitation
                (= *userid* new-contact-id))
      (flash (html (str (person-link new-contact-id))
                   " has been added to your contacts."))
      ))

  ;;is this really how we want to implement contact notifications?
  ;(create-contact-notification :follower userid :contact new-contact-id)
  )

(defun remove-contact (contact-id userid)
  (amodify-db *userid* :following (remove contact-id it))
  (with-locked-hash-table (*followers-index*)
    (remove userid (gethash contact-id *followers-index*)))
  (amodify-db *userid* :hidden-suggested-contacts (push  (cons contact-id (quick-rank contact-id)) it)))

(defun post-contacts ()
  (require-user (:require-active-user t :allow-test-user t)
    (let ((contacts (getf *user* :following)))
      (cond
        ((scan +number-scanner+ (post-parameter "add"))
         (let ((id (parse-integer (post-parameter "add"))))
           (if (and (getf *user* :test-user)
                    (not (db id :test-user)))
             (flash "Test users can only add other test users to their contacts."
                    :error t)
             (unless (member id contacts)
               (add-contact id *userid*))))
         (see-other (or (post-parameter "next") "/home")))

        ((scan +number-scanner+ (post-parameter "remove"))
         (let ((id (parse-integer (post-parameter "remove"))))
           (when (member id contacts)
             (remove-contact id *userid*)))
         (see-other (or (post-parameter "next") "/home")))

        (t
         (flash "Sorry, couldn't make sense of that request.")
         (see-other "/home"))))))
