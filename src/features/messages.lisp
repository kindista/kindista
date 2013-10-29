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
  (length (getf (if *userid*
                  *user-mailbox*
                  (gethash userid *person-mailbox-index*))
                :unread)))

(defun migrate-to-new-inboxes ()
  (dolist (id (hash-table-keys *db*))
    (let* ((data (db id))
           (type (getf data :type))
           (mailboxes (list :read nil :unread nil)))
      (when (or (eq type :conversation)
                (eq type :reply)
                (eq type :gratitude))
        (case type
          ((or :conversation :reply)
           (dolist (person (getf data :people))
             (if (or (and (cdr person)
                          (>= (cdr person) (getf data :latest-comment)))
                     (= (db (getf data :latest-comment) :by) (car person))
                     (<= (db (getf data :latest-comment) :created)
                         (or (db (car person) :last-checked-mail)
                             0)))
               (asetf (getf mailboxes :read)
                      (push (list (car person)) it))
               (asetf (getf mailboxes :unread)
                      (push (cons (list (car person))
                                  (or (cdr person) nil))
                            it))))
          ;(remove-db-property id :people)
           )
          (:gratitude
           (dolist (subject (getf data :subjects))
             (if (<= (getf data :created) (or (db subject :last-checked-mail) 0))
               (asetf (getf mailboxes :read)
                      (push (list subject) it))
               (asetf (getf mailboxes :unread)
                      (push (list subject) it))))))
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

(defun index-message-inbox-state (message)
"*person-mailbox-index* is a hashtable whose key is a mailbox (personid . groupid) and whose value is a plist such as :read (message ids) :unread ((message-id . last-read-comment-id) ..."
  (with-locked-hash-table (*person-mailbox-index*)
    (doplist (state values (message-mailboxes message))
      (dolist (value values)
        (case state
          (:read (asetf (getf (gethash value *person-mailbox-index*) state)
                        (push message it)))
          (:unread (asetf (getf (gethash (car value) *person-mailbox-index*) state)
                        (push message it))))))))

(defun index-message (id data)
"The people field for a conversation is a p-list of the status of the conversation for each participant: (:unread ((personid . last-read-comment)) ... "
  (let* ((time (case (getf data :type)
                 ((or :conversation :reply))
                  (db (getf data :latest-comment) :created)
                 (:gratitude (or (getf data :edited)
                                 (getf data :created)))))
         (message (make-message :id id
                                :time time
                                :mailboxes (getf data :mailboxes)
                                :type (getf data :type))))
    (with-locked-hash-table (*db-messages*)
      (setf (gethash id *db-messages*) message))
    (index-message-inbox-state message)))

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

(defun all-inbox-items (&key (id *userid*) filter)
  (let ((mailbox (if *userid*
                   *user-mailbox*
                   (gethash (list id) *person-mailbox-index*))))
   (sort
     (if filter
       (case filter
         (:unread (getf mailbox :unread)))
       (append (getf mailbox :read) (getf mailbox :unread)))
    #'> :key #'message-time)))

(defun inbox-items (&key (page 0) (count 20) filter)
  (let ((start (* page count))
        (items (all-inbox-items :filter filter)))
    (html
      (iter (for i from 0 to (+ start count))
            (cond
              ((< i start)
               (setf items (cdr items)))
              ((and (>= i start) items)
               (let* ((item (car items))
                      (item-data (db (message-id item))))
                 (case (message-type item)
                   (:conversation
                     (let* ((id (message-id item))
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
                             (str (h3-timestamp (message-time item)))
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
                     (let* ((id (message-id item))
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
                             (str (h3-timestamp (message-time item)))
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
                          (str (h3-timestamp (message-time item)))
                          (:p (str (person-link (getf item-data :subject))) " added you as a contact.")))))

                   (:gratitude
                     (unless (eql (getf item-data :author) *userid*)
                       (str
                        (card
                          (html
                            (str (h3-timestamp (message-time item)))
                            (:p (str (person-link (getf item-data :author))) " shared " (:a :href (strcat "/gratitude/" (message-id item)) "gratitude") " for you."))))))))
               (setf items (cdr items)))

              ((and (eql i start)
                    (not items))
               (htm
                 (:div :class "small card"
                   (:em "No messages")))
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
