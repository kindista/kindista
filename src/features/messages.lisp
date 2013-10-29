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

(defun result-gratitude-p (result)
  (eq (result-type result) :gratitude))

(defun users-with-new-mail ()
  (iter (for id in (hash-table-keys *db*))
        (let ((new-items (new-inbox-items id)))
          (when (and (eq (db id :type) :person)
                     (> new-items 0))
            (collect id)))))

(defun new-inbox-items (&optional (userid *userid*))
  (loop for item in (all-inbox-items :id userid)
        while (< (or (db userid :last-checked-mail) 0) (result-time item))
        unless (case (result-type item)
                 (:conversation
                   (eql (db (db (result-id item) :latest-comment) :by)
                        userid))
                 (:reply
                   (eql (db (db (result-id item) :latest-comment) :by)
                        userid))
                 (:gratitude
                   (eql (db (result-id item) :author) userid)))
        counting item into new-items
        finally (return new-items)))

(defun migrate-to-new-inboxes ()
  (dolist (id (hash-table-keys *db*))
    (let* ((data (db id))
           (type (getf data :type))
           (mailboxes (list :read (list) :unread (list))))
      (when (or (eq type :conversation)
                (eq type :reply)
                (eq type :gratitude))
        (case type
          ((or :conversation :reply)
           (dolist (person (getf data :people))
             (let ((comment (db (cdr person))))
               (if (and comment
                        (< (getf comment :created)
                           (+ (or (db (car person) :last-checked-mail)
                                  0)
                              1)))
                 (asetf (getf mailboxes :read)
                        (push (cons (list person)
                                    (or (cdr person) nil))
                              it))
                 (asetf (getf mailboxes :unread)
                        (push (cons (list person)
                                    (or (cdr person) nil))
                              it)))))
           (remove-db-property id :people))
          (:gratitude
           (setf (getf mailboxes :read)
                 (mapcar #'list (getf data :subjects)))))
        (modify-db id :mailboxes mailboxes)
        (setf mailboxes (list :read nil :unread nil))))))

(defun mailbox-ids (id-list)
"Takes a list of group/people ids and returns their mailboxes."
  (let (mailboxes)
    (dolist (id id-list)
      (let ((data (db id)))
        (case (getf data :type)
          (:person (asetf mailboxes (push (list id) it)))
          (:group (dolist (admin-id (getf data :admins))
                    (asetf mailboxes (push (cons admin-id id) it)))))))
    mailboxes))

(defun index-message-inbox-state (message-id message-mailboxes)
"*person-mailbox-index* is a hashtable whose key is a mailbox (personid . groupid) and whose value is a plist such as :read (message ids) :unread ((message-id . last-read-comment-id) ..."
  (with-locked-hash-table (*person-mailbox-index*)
    (doplist (state values message-mailboxes)
      (dolist (value values)
        (case state
          (:read (asetf (getf (gethash value *person-mailbox-index*) state)
                        (push message-id it)))
          (:unread (asetf (getf (gethash (car value) *person-mailbox-index*) state)
                        (push (cons message-id (cdr value)) it))))))))

(defun index-message (id data)
"The people field for a conversation is a p-list of the status of the conversation for each participant: (:unread ((personid . last-read-comment)) ... "
  (let* ((message (make-message :id id
                                :time (db (getf data :latest-comment) :created)
                                :mailboxes (getf data :mailboxes)
                                :type (getf data :type)))
         (mailboxes (message-mailboxes message)))
    (when (or (eql (message-type message) :conversation)
              (eql (message-type message) :reply))
      (with-locked-hash-table (*db-messages*)
        (setf (gethash id *db-messages*) message)))
    (index-message-inbox-state id mailboxes)))

(defun mark-message-read (id mailbox)
  (with-locked-hash-table (*db-messages*)
    (asetf (getf (gethash id *db-messages*) :read)
           (pushnew mailbox it :test #'equal))
    (asetf (getf (gethash id *db-messages*) :unread)
           (remove (assoc mailbox it :test #'equal) it :test #'equal))
    (asetf (getf (gethash id *db-messages*) :deleted)
           (remove (assoc mailbox it :test #'equal) it :test #'equal))
    (asetf (getf (gethash id *db-messages*) :archived)
           (remove (assoc mailbox it :test #'equal) it :test #'equal))))

(defun all-inbox-items (&key (id *userid*))
  (sort (append (gethash id *person-conversation-index*)
                (gethash id *person-notification-index*)
                (remove-if-not #'result-gratitude-p
                               (gethash id *activity-person-index*)))
    #'> :key #'result-time))

(defun inbox-items (&key (page 0) (count 20))
  (let ((start (* page count))
        (items (all-inbox-items)))
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
                            (comments (length (gethash id *comment-index*)))
                            (people (remove *userid*
                                            (cons (getf comment-data :by)
                                                  (remove (getf comment-data :by)
                                                          (mapcar #'car (getf item-data :people)))))))
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

                               (if people
                                 (str (name-list people))
                                 (htm (:span :class "nobody" "Empty conversation"))))

                             (:p :class "text"
                               (:span :class "title"
                                 (:a :href (strcat "/conversations/" id) (str (ellipsis (getf item-data :subject) 30)))
                                 (when (> comments 1)
                                   (htm
                                     " (" (str comments) ")")))
                               " - "
                               (:a :href (strcat "/conversations/" id)
                                (str (ellipsis (getf comment-data :text))))))))))
                   (:reply
                     (let* ((id (result-id item))
                            (latest (latest-comment id))
                            (latest-seen (cdr (assoc *userid* (getf item-data :people))))
                            (comment-data (db latest))
                            (original-item (db (getf item-data :on)))
                            (deleted-type (getf item-data :deleted-item-type))
                            (original-item-type (or (getf original-item :type)
                                                    deleted-type))
                            (people (mapcar #'car (db id :people)))
                            (with (or (getf original-item :by)
                                      (first (remove *userid* people))))
                            (comments (length (gethash id *comment-index*)))
                            (text (if (and (= comments 1)
                                           deleted-type)
                                    (deleted-invalid-item-reply-text
                                      (db (second people) :name)
                                      (db (first people) :name)
                                      (case deleted-type
                                        (:offer "offer")
                                        (:request "request"))
                                      (getf comment-data :text))
                                    (getf comment-data :text))))
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

                               (if (eql (db id :by) *userid*)
                                 (htm
                                   "You replied to "
                                   (str (person-link with))
                                   "'s "
                                   (case original-item-type
                                     (:offer
                                      (htm (:a :href (strcat "/offers/" (getf item-data :on)) "offer")))
                                     (:request
                                      (htm (:a :href (strcat "/requests/" (getf item-data :on)) "request")))
                                     (t (case deleted-type
                                          (:offer (htm "offer"))
                                          (:request (htm "request"))
                                          (t (htm (:span :class "none" "deleted offer or request")))))))
                                 (htm
                                   (str (person-link (getf item-data :by)))
                                   " replied to your "
                                   (case original-item-type
                                     (:offer
                                      (htm (:a :href (strcat "/offers/" (getf item-data :on)) "offer")))
                                     (:request
                                      (htm (:a :href (strcat "/requests/" (getf item-data :on)) "request")))
                                     (t (case deleted-type
                                          (:offer (htm "offer"))
                                          (:request (htm "request"))
                                          (t (htm (:span :class "none" "deleted offer or request")))))))))

                             (:p :class "text"
                               (:span :class "title"
                                 (:a :href (strcat "/conversations/" id)
                                   (str (ellipsis (getf original-item :text) 30)))
                                 (when (> comments 1)
                                   (htm
                                     " (" (str comments) ") "))
                                 " - ")
                               (:a :href (strcat "/conversations/" id)
                                (str (ellipsis text)))))))))
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
    (modify-db *userid* :last-checked-mail (get-universal-time))
    (send-metric* :checked-mailbox *userid*)
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
