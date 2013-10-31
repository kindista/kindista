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
                  (gethash (list userid) *person-mailbox-index*))
                :unread)))

(defun migrate-to-new-inboxes ()
  (dolist (id (hash-table-keys *db*))
    (let* ((data (db id))
           (type (getf data :type))
           (old-people (getf data :people))
           (new-people nil)
           (mailboxes (list :read nil :unread nil)))
      (when (or (eq type :conversation)
                (eq type :reply)
                (eq type :gratitude))
        (case type
          ((or :conversation :reply)
           (dolist (person old-people)
             (if (or (and (cdr person)
                          (>= (cdr person) (getf data :latest-comment)))
                     (= (db (getf data :latest-comment) :by) (car person))
                     (<= (db (getf data :latest-comment) :created)
                         (or (db (car person) :last-checked-mail)
                             0)))
               (asetf (getf mailboxes :read)
                      (push (cons (list (car person))
                                  (getf data :latest-comment))
                            it))
               (asetf (getf mailboxes :unread)
                      (push (cons (list (car person))
                                  (or (cdr person) nil))
                            it))))
           (setf new-people (mapcar #'car old-people))
           (modify-db id :mailboxes mailboxes
                         :people new-people))
          (:gratitude
           (dolist (subject (getf data :subjects))
             (if (<= (getf data :created) (or (db subject :last-checked-mail) 0))
               (asetf (getf mailboxes :read)
                      (push (list '(subject)) it))
               (asetf (getf mailboxes :unread)
                      (push (list '(subject)) it))))
           (modify-db id :mailboxes mailboxes)))
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

(defun update-mailbox-data (message-id mailbox new-state)
  (with-locked-hash-table (*db-messages*)
    (let* ((message (gethash message-id *db-messages*))
           (mailboxes (message-mailboxes message))
           (new-value-stored-p nil))
      (doplist (state mailboxes-in-state mailboxes)
        (cond
          ((eql state new-state)
           (progn
             (aif (cons-assoc mailbox (getf mailboxes state))
               (setf (getf mailboxes state)
                     (substitute (list mailbox)
                                 it
                                 (getf mailboxes state)
                                 :test #'equal))
               (asetf (getf mailboxes state)
                      (push (list mailbox) it)))
             (setf new-value-stored-p t)))
          (t
           (asetf (getf mailboxes state)
                  (remove (cons-assoc mailbox it) it :test #'equal)))))
      (unless new-value-stored-p
        (setf (message-mailboxes message)
              (append (list new-state (list mailbox)) mailboxes)))
      (modify-db message-id :mailboxes (message-mailboxes message)))))

(defun index-message (id data)
"The people field for a conversation is a p-list of the status of the conversation for each participant: (:unread ((personid . last-read-comment)) ... "
  (let* ((time (case (getf data :type)
                 ((or :conversation :reply)
                  (db (getf data :latest-comment) :created))
                 (:gratitude (or (getf data :edited)
                                 (getf data :created)))))
         (existing-message (gethash id *db-messages*))
         (new-message (unless existing-message
                        (make-message :id id
                                      :time time
                                      :mailboxes (getf data :mailboxes)
                                      :type (getf data :type)))))
    (aif new-message
      (with-locked-hash-table (*db-messages*)
        (setf (gethash id *db-messages*) it))
      (progn
        (setf (message-time existing-message) time)
        (setf (message-mailboxes existing-message) (getf data :mailboxes))))
    (index-message-inbox-state (or existing-message new-message))))

(defun index-message-inbox-state (message)
"*person-mailbox-index* is a hashtable whose key is a mailbox (personid . groupid) and whose value is a plist such as :read (message ids) :unread ((message-id . last-read-comment-id) ..."
  (with-locked-hash-table (*person-mailbox-index*)
    (let (all-mailboxes)
      (doplist (state mailboxes (message-mailboxes message))
      ;;get a list of all mailboxes for the message
        (asetf all-mailboxes
               (remove-duplicates (append (mapcar #'car mailboxes) it))))
      (doplist (state mailboxes-in-state (message-mailboxes message))
      ;;delete message from mailboxes not in each state
        (dolist (mailbox all-mailboxes)
          (unless (assoc mailbox mailboxes-in-state :test #'equal)
            (asetf (getf (gethash mailbox *person-mailbox-index*) state)
                   (remove message it))))
       (dolist (mailbox mailboxes-in-state)
         ;;add message to mailboxes in each given state
         (let ((mailbox-id (car mailbox)))
           (if (assoc mailbox-id mailboxes-in-state :test #'equal)
             (asetf (getf (gethash mailbox-id *person-mailbox-index*) state)
                    (pushnew message it)))))))))

(defun all-inbox-items (&key (id *userid*) filter)
  (let ((mailbox (if *userid*
                   *user-mailbox*
                   (gethash (list id) *person-mailbox-index*))))
   (sort
     (if filter
       (case filter
         (:unread (getf mailbox :unread)))
       (append (copy-list (getf mailbox :read))
               (copy-list (getf mailbox :unread))))
    #'> :key #'message-time)))

(defun inbox-items (&key (page 0) (count 20) filter)
  (let ((start (* page count))
        (mailbox (list *userid*))
        (items (all-inbox-items :filter filter)))
    (html
      (:form :method "post" :action "/messages"
        (:input :type "hidden" :name "mailbox" :value (str (cons-to-string mailbox)))
        (:div :class "mail card menu"
          (:button :class "cancel small" :type "submit" :name "mark-read" "mark read")
          (:button :class "cancel small" :type "submit" :name "mark-unread" "mark unread")
          (:button :class "cancel small" :type "submit" :name "delete" "delete"))
        (iter (for i from 0 to (+ start count))
          (cond
            ((< i start)
             (setf items (cdr items)))
            ((and (>= i start) items)
             (let* ((item (car items))
                    (item-data (db (message-id item)))
                    (mailboxes (message-mailboxes item))
                    (message-status (cond
                                      ((cons-assoc mailbox
                                                   (getf mailboxes :read))
                                       :read)
                                      ((cons-assoc mailbox
                                                   (getf mailboxes :unread))
                                       :unread))))
               (htm
                 (:div :class (s+ (case message-status
                                    (:read "read")
                                    (:unread "unread"))
                                  " mail card")
                   (:input :type "checkbox"
                           :name "message-id"
                           :value (message-id item))
                   (:div :class "message-content"
                     (case (message-type item)
                       (:conversation
                         (str (conversation-inbox-item item item-data message-status)))
                       (:reply
                         (str (reply-inbox-item item item-data message-status)))
                       (:contact-n
                         (htm
                           (str (h3-timestamp (message-time item)))
                           (:p (str (person-link (getf item-data :subject)))
                            " added you as a contact.")))

                       (:gratitude
                         (unless (eql (getf item-data :author) *userid*)
                           (htm
                             (str (h3-timestamp (message-time item)))
                             (:p (str (person-link (getf item-data :author)))
                                 " shared "
                                 (:a :href (strcat "/gratitude/"
                                                   (message-id item))
                                     "gratitude")
                                 " for you.")))))))))
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
                     (:a :style "float: right;" :href (strcat "/messages?p=" (+ page 1)) "next page >"))))))))))))

(defun conversation-inbox-item (message message-data message-status)
  (let* ((id (message-id message))
         (latest (latest-comment id))
         (latest-seen (cdr (assoc (list *userid*)
                                  (getf (message-mailboxes message)
                                        message-status))))
         (comment-data (db latest))
         (comments (length (gethash id *comment-index*)))
         (people (remove *userid*
                         (cons (getf comment-data :by)
                               (remove (getf comment-data :by)
                                       (getf message-data :people))))))
    (html
      (str (h3-timestamp (message-time message)))
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
          (:a :href (strcat "/conversations/" id) (str (ellipsis (getf message-data :subject) 30)))
          (when (> comments 1)
            (htm
              " (" (str comments) ")")))
        " - "
        (:a :href (strcat "/conversations/" id)
         (str (ellipsis (getf comment-data :text))))))))

(defun reply-inbox-item (message message-data message-status)
  (let* ((id (message-id message))
         (latest (latest-comment id))
         (latest-seen (cdr (assoc (list *userid*)
                                  (getf (message-mailboxes message)
                                        message-status))))
         (comment-data (db latest))
         (original-message (db (getf message-data :on)))
         (deleted-type (getf message-data :deleted-message-type))
         (original-message-type (or (getf original-message :type)
                                 deleted-type))
         (people (db id :people))
         (with (or (getf original-message :by)
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
    (html
      (str (h3-timestamp (message-time message)))
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
            (case original-message-type
              (:offer
               (htm (:a :href (strcat "/offers/" (getf message-data :on))
                     "offer")))
              (:request
               (htm (:a :href (strcat "/requests/" (getf message-data :on))
                     "request")))
              (t (case deleted-type
                   (:offer (htm "offer"))
                   (:request (htm "request"))
                   (t (htm (:span :class "none" "deleted offer or request")))))))
          (htm
            (str (person-link (getf message-data :by)))
            " replied to your "
            (case original-message-type
              (:offer
               (htm (:a :href (strcat "/offers/" (getf message-data :on))
                     "offer")))
              (:request
               (htm (:a :href (strcat "/requests/" (getf message-data :on))
                     "request")))
              (t (case deleted-type
                   (:offer (htm "offer"))
                   (:request (htm "request"))
                   (t (htm (:span :class "none" "deleted offer or request")))))))))

      (:p :class "text"
        (:span :class "title"
          (:a :href (strcat "/conversations/" id)
            (str (ellipsis (getf original-message :text) 30)))
          (when (> comments 1)
            (htm
              " (" (str comments) ") "))
          " - ")
        (:a :href (strcat "/conversations/" id)
         (str (ellipsis text)))))))

(defun get-messages ()
  (require-user
    (modify-db *userid* :last-checked-mail (get-universal-time))
    (send-metric* :checked-mailbox *userid*)
    (standard-page
      "Messages"
      (html
        (str (menu-horiz "actions"
                         (html (:a :href "/conversations/new" "start a new conversation"))))
        (str (inbox-items :page (if (scan +number-scanner+ (get-parameter "p"))
                                  (parse-integer (get-parameter "p"))
                                  0))))
      :selected "messages")))

(defun post-messages ()
  (require-user
    (let ((mailbox (or (parse-cons (post-parameter "mailbox"))
                       (list *userid*)))
          (messages (loop for pair in (post-parameters*)
                          when (and (string= (car pair) "message-id")
                                    (scan +number-scanner+ (cdr pair)))
                          collect (parse-integer (cdr pair))))
          (referer (aif (post-parameter "mailbox")
                     (url-compose "/messages" "mailbox" it)
                     "/messages")))
      (unless (= (car mailbox) *userid*) (permission-denied))

      (cond
        ((not (post-parameter "message-id"))
         (flash "Please select at least one message first")
         (see-other referer))

        ((post-parameter "mark-read")
         (dolist (id messages)
           (let ((message (gethash id *db-messages*))
                 (new-mailboxes nil))
             (doplist (state mailboxes-in-state (message-mailboxes message))
               )))) )
      )))
