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
           (latest-comment (getf data :latest-comment))
           (type (getf data :type))
           (people (getf data :people))
           (participants nil)
           (mailboxes nil)
           (status (list :read nil :unread nil)))
      (when (or (eq type :conversation)
                (eq type :reply)
                (eq type :gratitude))
        (case type
          ((or :conversation :reply)
           (dolist (person people)
             (let ((mailbox (list (car person))))
               (if (or (and (cdr person)
                           (= (cdr person) latest-comment))
                      (= (db latest-comment :by) (car person))
                      (<= (db latest-comment :created)
                          (or (db (car person) :last-checked-mail) 0)))
                 (progn
                   (asetf mailboxes
                         (push (cons mailbox latest-comment) it))
                   (asetf (getf status :read)
                          (push mailbox it)))
                 (progn
                   (asetf mailboxes
                          (push (cons mailbox (cdr person)) it))
                   (asetf (getf status :unread)
                          (push mailbox it))))))
           (setf participants (mapcar #'car people))
           (modify-db id :status status
                         :mailboxes mailboxes
                         :participants participants))
          (:gratitude
           (dolist (subject (getf data :subjects))
             (let ((mailbox (list subject)))
                (if (<= (getf data :created)
                        (or (db subject :last-checked-mail) 0))
                 (asetf (getf status :read)
                        (push mailbox it))
                 (asetf (getf status :unread)
                        (push mailbox it)))))
           (modify-db id :status status
                         :mailboxes mailboxes)))
        (setf status (list :read nil :unread nil)
              mailboxes nil
              participants nil)))))

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

(defun update-mailbox-data (message-id mailbox new-status &key last-read-comment)
"Update *db-messages* and (db message-id) when the state changes for a mailbox associated with that message."
  (with-locked-hash-table (*db-messages*)
    (let* ((message (gethash message-id *db-messages*))
           (statuses (message-status message))
           (new-value-stored-p nil)
           (valid-mailbox (cons-assoc mailbox (message-mailboxes message))))

      (when valid-mailbox

        (when last-read-comment
          (asetf (cdr valid-mailbox) last-read-comment))

        (doplist (status mailboxes-with-status statuses)
          (asetf (getf (message-status message) status)
                 (if (eql status new-status)
                   (progn
                     (setf new-value-stored-p t)
                     (pushnew mailbox it :test #'equal))
                   (remove mailbox it :test #'equal))))

        (unless new-value-stored-p
          (asetf (message-status message)
                 (append (list new-status (list mailbox)) it)))

        (modify-db message-id :mailboxes (message-mailboxes message)
                              :status (message-status message))
        (index-mailbox-status message)))))

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
                                      :status (getf data :status)
                                      :type (getf data :type)))))
    (aif new-message
      (with-locked-hash-table (*db-messages*)
        (setf (gethash id *db-messages*) it))
      (progn
        (setf (message-time existing-message) time)
        (setf (message-status existing-message) (getf data :status))))
    (index-mailbox-status (or existing-message new-message))))

(defun all-message-mailboxes (message)
"a list of all mailboxes for a given message"
  (let (all-mailboxes)
    (doplist (status mailboxes (message-status message))
      (asetf all-mailboxes
             (remove-duplicates (append mailboxes it))))
    all-mailboxes))

(defun index-mailbox-status (message)
"*person-mailbox-index* is a hashtable whose key is a mailbox (personid . groupid) and whose value is a plist such as :read (message ids) :unread ((message-id . last-read-comment-id) ..."
  (with-locked-hash-table (*person-mailbox-index*)
    (let ((all-mailboxes (all-message-mailboxes message)))
      (doplist (status mailboxes-with-status (message-status message))
      ;;delete message from mailboxes not in each state
        (dolist (mailbox all-mailboxes)
          (unless (member mailbox mailboxes-with-status :test #'equal)
            (asetf (getf (gethash mailbox *person-mailbox-index*) status)
                   (remove message it))))
        (dolist (mailbox mailboxes-with-status)
          ;;add message to mailboxes in each given state
          (asetf (getf (gethash mailbox *person-mailbox-index*) status)
                 (pushnew message it)))))))

(defun message-filter (&key (selected "all") mailbox)
  (html
    (:form :method "get" :action "/messages"
     (:label :for "filter" "display")
     (:input :type "hidden" :name "mailbox" :value (or mailbox
                                                       (list *userid*)))
     (:select :name "filter"
              :id "filter"
              :onchange "this.form.submit()"
       (:option :value "all" :selected (when (string= selected "all") "")
         "all mail")
       (:option :value "unread" :selected (when (string= selected "unread") "")
         "unread mail")
       (:option :value "compost" :selected (when (string= selected "compost") "")
         "compost"))
      (:input :type "submit" :class "no-js" :value "apply"))))

(defun all-inbox-items (&key (id *userid*) filter)
  (let ((mailbox (if *userid*
                   *user-mailbox*
                   (gethash (list id) *person-mailbox-index*))))
   (sort
     (cond
       ((string= "all" filter)
        (append (copy-list (getf mailbox :read))
                (copy-list (getf mailbox :unread))))
       ((string= "unread" filter)
        (getf mailbox :unread))
       ((string= "comboxt" filter)
        (getf mailbox :compost)))
    #'> :key #'message-time)))

(defun inbox-items (&key (page 0) (count 20))
  (let* ((start (* page count))
         (filter (or (get-parameter "filter") "all"))
         (mailbox (list *userid*))
         (string-mailbox (cons-to-string mailbox))
         (items (all-inbox-items :filter filter)))
    (html
      (str (message-filter :selected filter :mailbox string-mailbox))
      (:form :method "post" :action "/messages"
        (:input :type "hidden" :name "mailbox" :value string-mailbox)
        (:input :type "hidden" :name "filter" :value filter)
        (:div :class "mail card menu"
          (:button :class "cancel small" :type "submit" :name "mark-read" "mark read")
          (:button :class "cancel small" :type "submit" :name "mark-unread" "mark unread")
          (:button :class "cancel small" :type "submit" :name "discard" "discard"))
        (iter (for i from 0 to (+ start count))
          (cond
            ((< i start)
             (setf items (cdr items)))
            ((and (>= i start) items)
             (let* ((item (car items))
                    (item-data (db (message-id item)))
                    (mailboxes (message-status item))
                    (status (cond
                              ((member mailbox (getf mailboxes :read)
                                       :test #'equal)
                               :read)
                              ((member mailbox (getf mailboxes :unread)
                                       :test #'equal)
                               :unread))))
               (htm
                 (:div :class (s+ (case status
                                    (:read "read")
                                    (:unread "unread"))
                                  " mail card")
                   (:input :type "checkbox"
                           :name "message-id"
                           :value (message-id item))
                   (:div :class "message-content"
                     (case (message-type item)
                       (:conversation
                         (str (conversation-inbox-item item item-data)))
                       (:reply
                         (str (reply-inbox-item item item-data)))
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

(defun conversation-inbox-item (message message-data &key mailbox)
  (let* ((id (message-id message))
         (latest (latest-comment id))
         (latest-seen (cdr (assoc (list *userid*) (message-mailboxes message))))
         (comment-data (db latest))
         (comments (length (gethash id *comment-index*)))
         (people (remove *userid*
                         (cons (getf comment-data :by)
                               (remove (getf comment-data :by)
                                       (getf message-data :participants))))))
    (flet ((url (text)
             (html
               (:a :href (url-compose (strcat "/conversations/" id)
                                     "mailbox" mailbox)
                   (str text)))))
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
           (str (url (ellipsis (getf message-data :subject) 30)))
            (when (> comments 1)
              (htm
                " (" (str comments) ")")))
          " - "
          (str (url (ellipsis (getf comment-data :text)))))))))

(defun reply-inbox-item (message message-data &key mailbox)
  (let* ((id (message-id message))
         (latest (latest-comment id))
         (latest-seen (cdr (assoc (list *userid*) (message-mailboxes message))))
         (comment-data (db latest))
         (original-message (db (getf message-data :on)))
         (deleted-type (getf message-data :deleted-message-type))
         (original-message-type (or (getf original-message :type)
                                 deleted-type))
         (people (db id :participants))
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
    (flet ((inventory-url ()
             (html
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
                      (t (htm (:span :class "none" "deleted offer or request"))))))))
           (reply-url (text)
             (html (:a :href (url-compose (strcat "/conversations/" id)
                                          "mailbox" mailbox)
                     (str text)))))

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
              (str (inventory-url)))
            (htm
              (str (person-link (getf message-data :by)))
              " replied to your "
              (str (inventory-url)))))

        (:p :class "text"
          (:span :class "title"
            (str (reply-url (ellipsis (getf original-message :text) 30)))
            (when (> comments 1)
              (htm
                " (" (str comments) ") "))
            " - ")
          (str (reply-url (ellipsis text))))))))

(defun get-messages ()
  (require-user
    (let ((mailbox (or (awhen (get-parameter "mailbox")
                         (parse-cons it))
                       (list *userid*))))
      (unless (= (car mailbox) *userid*) (permission-denied))
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
        :selected "messages"))))

(defun post-messages ()
  (require-user
    (let ((mailbox (or (parse-cons (post-parameter "mailbox"))
                       (list *userid*)))
          (messages (loop for pair in (post-parameters*)
                          when (and (string= (car pair) "message-id")
                                    (scan +number-scanner+ (cdr pair)))
                          collect (parse-integer (cdr pair))))
          (next (or (post-parameter "next") (referer))))

      (unless (= (car mailbox) *userid*) (permission-denied))

      (cond
        ((not (post-parameter "message-id"))
         (flash "Please select at least one message first")
         (see-other next))

        ((post-parameter "mark-read")
         (dolist (id messages)
           (let ((data (db id)))
             (unless (member mailbox (getf (getf data :status) :read)
                            :test #'equal)
             (update-mailbox-data id mailbox :read :last-read-comment (getf data :latest-comment)))))
         (see-other next))

        ((post-parameter "mark-unread")
         (dolist (id messages)
           (let ((data (db id)))
             (unless (member mailbox (getf (getf data :status) :unread)
                             :test #'equal)
             (update-mailbox-data id mailbox :unread))))
         (see-other next))

        ((post-parameter "discard")
         (dolist (id messages)
           (let ((data (db id)))
             (unless (member mailbox (getf (getf data :status) :compost)
                             :test #'equal)
             (update-mailbox-data id mailbox :compost))))
         (see-other next))

        ((post-parameter "delete")
         (dolist (id messages)
           (let ((data (db id)))
             (unless (member mailbox (getf (getf data :status) :deleted)
                             :test #'equal)
             (update-mailbox-data id mailbox :deleted))))
         (see-other next))

        ))))
