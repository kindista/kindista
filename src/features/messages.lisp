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
  (iter (for id in *active-people-index*)
        (let ((new-items (new-inbox-items id)))
          (when (> new-items 0)
            (collect id)))))

(defun new-inbox-items (&optional (userid *userid*))
  (length (getf (if *userid*
                  *user-mailbox*
                  (gethash userid *person-mailbox-index*))
                :unread)))

;(defun migrate-to-new-inboxes ()
;  (dolist (id (hash-table-keys *db*))
;    (let* ((data (db id))
;           (latest-comment (getf data :latest-comment))
;           (type (getf data :type))
;           (old-people (getf data :people))
;           (new-people nil)
;           (participants nil)
;           (inbox nil))
;      (case type
;        ((or :conversation :reply)
;         (dolist (person old-people)
;           (let ((new-person (cons (list (car person)) (cdr person))))
;             (when (or (and (cdr person)
;                          (= (cdr person) latest-comment))
;                     (equal (db latest-comment :by) (car person))
;                     (equal (db latest-comment :by) (list (car person)))
;                     (<= (db latest-comment :created)
;                         (or (db (car person) :last-checked-mail) 0)))
;                 (setf (cdr new-person) latest-comment))
;             (asetf new-people (push new-person it))
;             (asetf participants (push (car person) it))
;             (asetf inbox (push (car person) it))))
;         (modify-db id :people new-people
;                       :message-folders (list :inbox inbox)
;                       :participants participants))
;        (:gratitude
;         (dolist (subject (getf data :subjects))
;           (let ((new-person (cons (list subject) nil)))
;              (if (<= (getf data :created)
;                      (or (db subject :last-checked-mail) 0))
;                (setf (cdr new-person) :read)
;                (setf (cdr new-person) :unread))
;              (asetf inbox (push subject it))
;              (asetf new-people (push new-person it))))
;         (modify-db id :people new-people
;                       :message-folders (list :inbox inbox)))
;       (:comment
;         (amodify-db id :by (list it)))
;
;       (:person
;         (modify-db id :notify-group-membership-invites t))))))

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

(defun update-folder-data (message new-status &key last-read-comment)
"Update *db-messages* and (db message-id) when the state changes for a mailbox associated with that message."
  (with-locked-hash-table (*db-messages*)
    (let* ((id (message-id message))
           (people (message-people message))
           (valid-participants (loop for person in people
                                     when (eql *userid* (caar person))
                                     collect person)))

      (when valid-participants
        (when last-read-comment
          (dolist (participant valid-participants)
            (asetf (cdr (assoc (car participant) people :test #'equal)) last-read-comment))))


      (flet ((remove-from-folders (folders)
               (dolist (folder folders)
                 (asetf (getf (message-folders message) folder)
                        (remove *userid* it))))
             (add-to-folder (folder)
               (asetf (getf (message-folders message) folder)
                      (pushnew *userid* it))))

        (case new-status
          (:inbox
            (add-to-folder :inbox)
            (remove-from-folders (list :compost)))
          (:read
            (remove-from-folders (list :unread)))
          (:unread
            (add-to-folder :unread))
          (:compost
            (case (message-type message)
              (:gratitude
                 (add-to-folder :deleted)
                 (remove-from-folders (list :inbox :compost :unread)))
              (t
               (remove-from-folders (list :inbox :unread))
               (add-to-folder :compost))))
          (:deleted
            (add-to-folder :deleted)
            (remove-from-folders (list :inbox :compost :unread)))))

  (modify-db id :message-folders (message-folders message)
                :people (message-people message))
  (index-message-folders message))))

(defun message-groups (message)
  (remove nil (mapcar #'cadadr (message-people message))))

(defun index-message (id data)
"The people field for a conversation is a p-list of the status of the conversation for each participant: (:unread ((personid . last-read-comment)) ... "
  (let* ((type (getf data :type))
         (time (case type
                 (:conversation
                  (db (getf data :latest-comment) :created))
                 (:transaction
                   (or (getf (car (getf data :log)) :time)
                       (getf data :latest-comment :created)))
                 ((or :group-membership-invitation
                      :group-membership-request)
                  (or (getf data :resent)
                      (getf data :created)))
                 (:gratitude (or (getf data :edited)
                                 (getf data :created)))))
         (latest-comment (getf data :latest-comment))
         (folders (getf data :message-folders))
         (people (case (getf data :type)
                   (:gratitude (remove (assoc (list (getf data :author))
                                              (getf data :people)
                                              :test #'equal)
                                       (getf data :people)
                                       :test #'equal))
                   (t (getf data :people))))
         (groups (unless (eql (getf data :type) :gratitude) ; index conversations only
                   (remove nil (mapcar #'cdar people))))
         (existing-message (gethash id *db-messages*))
         (pending-gratitude-p)
         (new-message (unless existing-message
                        (make-message :id id
                                      :time time
                                      :latest-comment latest-comment
                                      :people people
                                      :folders folders
                                      :type (getf data :type))))
         (message (or existing-message new-message)))

    (with-locked-hash-table (*db-messages*)
      (aif new-message
        (setf (gethash id *db-messages*) it)
        (progn
          (setf (message-time existing-message) time)
          (setf (message-latest-comment existing-message) latest-comment)

          (setf (message-people existing-message) people)
          (setf (message-folders existing-message) folders))))
    (with-locked-hash-table (*group-messages-index*)
      (dolist (group groups)
        (pushnew message (gethash group *group-messages-index*))))
    (index-message-folders message)

    (when (eql type :transaction)
      ;; see if there is a :given or :received action more recently than a 
      ;; :gratitude-posted
      (loop for event in (getf data :log)
            until (eq (getf event :action) :gratitude-posted)
            when (or (eq (getf event :action) :given)
                     (eq (getf event :action) :received))
            do (setf pending-gratitude-p t))

      (when pending-gratitude-p
        (let* ((item-id (getf data :on))
               (item (db item-id))
               (by (getf item :by))
               (result (when item ; prior to 6/3/2014 inventory items could be deleted
                         (inventory-item-result item-id
                                                :data item
                                                :by-id by))))

          (when result
            (with-locked-hash-table (*pending-gratitude-index*)
              (case (getf item :type)
                (:offer
                  (push (cons result id)
                        (getf (gethash (getf data :by) *pending-gratitude-index*)
                              :offers)))
                (:request
                  (push (cons result id)
                        (getf (gethash by *pending-gratitude-index*)
                              :requests)))))))))))

(defun all-message-people (message)
"a list of people with access to a given message"
  (mapcar #'caar (message-people message)))

(defun index-message-folders (message)
"*person-mailbox-index* is a hashtable whose key is a userid and whose value is a plist such as :inbox (messages) :unread (messages) ..."
  (with-locked-hash-table (*person-mailbox-index*)
    (dolist (person (message-people message))
      ;; add message to people's unread folder as necessary
      (asetf (getf (gethash (caar person) *person-mailbox-index*) :unread)
             (if (or (eql (cdr person) :read)
                     (eql (cdr person) (message-latest-comment message)))
               (remove message it)
               (pushnew message it))))
    (let ((all-people (all-message-people message)))
      (doplist (folder people-in-folder (message-folders message))
      ;;delete message from people's folders as necessary
        (dolist (person all-people)
          (unless (member person people-in-folder)
            (asetf (getf (gethash person *person-mailbox-index*) folder)
                   (remove message it))))
        (dolist (person people-in-folder)
          ;;add message to people's folders as necessary
          (asetf (getf (gethash person *person-mailbox-index*) folder)
                 (pushnew message it)))))))

(defun remove-message-from-indexes (id)
  (let ((message (gethash id *db-messages*)))
    (dolist (person (mapcar #'caar (message-people message)))
      (with-locked-hash-table (*person-mailbox-index*)
        (doplist (folder messages (gethash person *person-mailbox-index*))
          (asetf (getf (gethash person *person-mailbox-index*) folder)
              (remove message it))))))
  (with-locked-hash-table (*db-messages*)
    (remhash id *db-messages*)))

(defun message-filter (&optional (selected "all"))
  (html
    (:form :method "get" :action "/messages" :class "message-filter"
     (:label :for "filter" "show")
     (:select :name "filter"
              :class "message-filter"
              :id "filter"
              :onchange "this.form.submit()"
       (:option :value "all" :selected (when (string= selected "all") "")
         "all mail")
       (:option :value "unread" :selected (when (string= selected "unread") "")
         "unread mail")
       (:option :value "compost" :selected (when (string= selected "compost") "")
         "compost"))
      (:input :type "submit" :class "no-js" :value "apply"))))

(defun filter-inbox-items (filter &key (id *userid*))
  (let ((message-index (if *userid*
                         *user-mailbox*
                         (gethash id *person-mailbox-index*))))
  ;; copy-list is needed to prevent destructive sort operation on the index
   (safe-sort (cond
               ((string= "all" filter)
                (getf message-index :inbox))
               ((string= "unread" filter)
                (getf message-index :unread))
               ((string= "compost" filter)
                (getf message-index :compost)))
    #'> :key #'message-time)))

(defun inbox-items (&key (page 0) (count 20))
  (let* ((start (* page count))
         (groups (mapcar #'(lambda (id)
                             (cons id (db id :name)))
                         (getf *user-group-privileges* :admin)))
         (filter (or (get-parameter "filter") "all"))
         (items (filter-inbox-items filter)))
    (html
      (str (message-filter filter))
      (:form :method "post" :action "/messages"
        (:input :type "hidden" :name "filter" :value filter)
        (:div :class "mail card menu"
          (if (string= filter "compost")
            (htm
              (:button :class "cancel small" :type "submit" :name "move-to-inbox"
                "move to inbox")
              (:button :class "cancel small" :type "submit" :name "delete"
                "delete"))
            (htm
              (:button :class "cancel small" :type "submit" :name "mark-read"
                "mark read")
              (:button :class "cancel small" :type "submit" :name "mark-unread"
                "mark unread")
              (:button :class "cancel small" :type "submit" :name "discard"
                "discard"))))
        (iter (for i from 0 to (+ start count))
          (cond
            ((< i start)
             (setf items (cdr items)))
            ((and (>= i start) items)
             (let* ((item (car items))
                    (item-id (message-id item))
                    (folders *user-mailbox*)
                    (status (if (find item (getf folders :unread))
                               :unread
                               :read)))
               (htm
                 (:table :class "messages"
                   (:tr :class (s+ (case status
                                    (:read "read")
                                    (:unread "unread"))
                                  " mail card")
                     (:td :class "message-selector"
                       (:input :type "checkbox"
                               :name "message-id"
                               :value item-id))
                     (:td :class "message-content"
                       (case (message-type item)
                         (:conversation
                           (str (conversation-inbox-item item groups)))
                         (:transaction
                           (pprint item)
                           (terpri)
                           (str (transaction-inbox-item item groups)))
                         (:contact-n
                           (htm
                             (str (h3-timestamp (message-time item)))
                             (:p (str (person-link (getf (db item-id)
                                                         :subject)))
                              " added you as a contact.")))
                         (:group-membership-request
                           (str (group-membership-request-inbox-item item)))
                         (:group-membership-invitation
                           (str (group-membership-invitation-inbox-item item)))
                         (:gratitude
                           (htm
                             (str (gratitude-inbox-item item groups))))))))))
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

(defun group-message-indicator (message groups)
  (let ((my-groups (loop for person in (message-people message)
                         when (and (cdr (car person))
                                   (eq (caar person) *userid*))
                         collect (cdr (car person)))))
    (when my-groups
      (html
        (dolist (group my-groups)
          (htm (:span :class "group-indicator"
                  (:a :href (s+ "/groups/" (username-or-id group))
                    (str (or (cdr (assoc group groups))
                            ;in case they are no longer an admin for the group
                            (db group :name)))))))))))

(defun group-membership-request-inbox-item-old (message)
  (let* ((id (message-id message))
         (data (db id)))
    (html
      (str (h3-timestamp (message-time message)))
      (:table :class "membership-requests"
        (:tr
          (:td :class "request-name"
            (str (person-link (getf data :requested-by)))
            " is requesting to join "
            (str (group-link (getf data :group-id))))
          (:td :class "member-approval"
            (:input :type "hidden" :name "message-id" :value id)
            (:button :class "yes small" :type "submit" :name "approve-group-membership-request" "approve request")
            (:button :class "cancel small" :type "submit" :name "deny-group-membership-request" "deny request")))))))

(defun group-membership-request-inbox-item (message)
  (let* ((id (message-id message))
         (data (db id)))
    (html
      (str (h3-timestamp (message-time message)))
      (:div :class "membership-request"
        (:div :class "member-approval"
          (str
            (v-align-middle
              (:div :class "member-approval-ui"
                (:input :type "hidden" :name "message-id" :value id)
                (:button :class "yes small" :type "submit" :name "approve-group-membership-request" "approve request")
                (:button :class "cancel small" :type "submit" :name "deny-group-membership-request" "deny request")))))
        (:div :class "request-name"
          (str
            (v-align-middle
              (:p :class "member-request-details"
                (str (person-link (getf data :requested-by)))
                " is requesting to join "
                (str (group-link (getf data :group-id)))))))))))

(defun group-membership-invitation-inbox-item (message)
  (let* ((id (message-id message))
         (data (db id))
         (group-id (getf data :group-id)))
    (html
      (str (h3-timestamp (message-time message)))
      (:div :class "membership-request"
        (:div :class "member-approval"
          (str
            (v-align-middle
              (:div :class "member-approval-ui"
                (:input :type "hidden" :name "message-id" :value id)
                (:button :class "yes small" :type "submit" :name "accept-group-membership-invitation" "join group")
                (:button :class "cancel small" :type "submit" :name "decline-group-membership-invitation" "decline")))))
        (:div :class "request-name"
          (str
            (v-align-middle
              (:p :class "member-request-details"
                (str (person-link (getf data :invited-by)))
                " has invited you to join "
                (if (eq group-id +kindista-id+)
                  (htm "Kindista's " (:a :href "/groups/kindista"
                                         "group account") ".")
                  (htm "the group "
                       (str (group-link group-id))
                       " on Kindista."))))))))))

(defun gratitude-inbox-item (message groups)
  (let* ((id (message-id message))
         (mailboxes (loop for person in (message-people message)
                         when (eq (caar person) *userid*)
                         collect (car person)))
         (self (when (member (list *userid*) mailboxes :test #'equal) "you"))
         (my-groups (mapcar #'(lambda (mailbox) (db (cdr mailbox) :name))
                            mailboxes)))
    (html
      (str (h3-timestamp (message-time message)))
      (:p :class "people"
        (str (person-link (getf (db id) :author)))
        " shared "
        (:a :href (strcat "/gratitude/" id)
            "gratitude")
        " for "
        (str (format nil *english-list* (remove nil (push self my-groups))))
        (str (group-message-indicator message groups))))))

(defun conversation-inbox-item (message groups)
  (let* ((id (message-id message))
         (conversation (db id))
         (latest (message-latest-comment message))
         (comment-data (db latest))
         (comment-by (car (getf comment-data :by)))
         (comments (length (gethash id *comment-index*)))
         (participants (remove *userid*
                         (cons comment-by
                               (remove comment-by
                                       (getf conversation :participants))))))
    (flet ((url (text)
             (html
               (:a :href (strcat "/conversations/" id)
                   (str text)))))
     (html
        (str (h3-timestamp (message-time message)))
        (:p :class "people"
          (when (eql comment-by *userid*)
            (str "↪ "))

          (aif participants
            (str (name-list it))
            (htm (:span :class "nobody" "Empty conversation")))

          (str (group-message-indicator message groups)))

        (:p :class "text"
          (:span :class "title"
           (str (url (ellipsis (getf conversation :subject) :length 30)))
            (when (> comments 1)
              (htm
                " (" (str comments) ")")))
          " - "
          (str (url (ellipsis (getf comment-data :text)))))))))

(defun transaction-inbox-item (message groups)
  (let* ((id (message-id message))
         (transaction (db id))
         (latest (message-latest-comment message))
         (comment-data (db latest))
         (comment-by (car (getf comment-data :by)))
         (inventory-item (db (getf transaction :on)))
         (deleted-type (getf transaction :deleted-item-type))
         (inventory-item-type (or (getf inventory-item :type)
                                  deleted-type))
         (participants (db id :participants))
         (group-name (cdr (assoc (car participants) groups)))
         (with (or (getf inventory-item :by)
                   (first (remove *userid* participants))))
         (comments (length (getf transaction :log)))
         (text (if (and (= comments 1)
                        deleted-type)
                 (deleted-invalid-item-reply-text
                   (db (second participants) :name)
                   (db (first participants) :name)
                   (case deleted-type
                     (:offer "offer")
                     (:request "request"))
                   (getf comment-data :text))
                 (getf comment-data :text))))

    (flet ((inventory-url ()
             (html
               (case inventory-item-type
                 (:offer
                  (htm (:a :href (strcat "/offers/" (getf transaction :on))
                        "offer")))
                 (:request
                  (htm (:a :href (strcat "/requests/" (getf transaction :on))
                        "request")))
                 (t (case deleted-type
                      (:offer (htm "offer"))
                      (:request (htm "request"))
                      (t (htm (:span :class "none" "deleted offer or request"))))))))
           (transaction-url (text)
             (html (:a :href (strcat "/conversations/" id)
                     (str text)))))

      (html
        (str (h3-timestamp (message-time message)))
        (:p :class "people"
          (when (eql comment-by *userid*)
            (str "↪ "))

          (if (eql (getf transaction :by) *userid*)
            (htm
              "You replied to "
              (str (person-link with))
              "'s "
              (str (inventory-url)))
            (htm
              (str (person-link (getf transaction :by)))
              " replied to "
              (str (if group-name (s+ group-name "'s ") "your "))
              (str (inventory-url))))
          (str (group-message-indicator message groups)) )

        (:p :class "text"
          (:span :class "title"
            (str (transaction-url (ellipsis (or (getf inventory-item :title)
                                                (getf inventory-item :details))
                                             :length 30)))
            (when (> comments 1)
              (htm
                " (" (str comments) ") "))
            " - ")
          (str (transaction-url (ellipsis text))))))))

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
                           (html (:a :href "/conversations/new" "start a new conversation"))
                           (html (:a :href "/gratitude/new" "express gratitude"))))
          (str (inbox-items :page (if (scan +number-scanner+ (get-parameter "p"))
                                    (parse-integer (get-parameter "p"))
                                    0))))
        :selected "messages"))))

(defun post-messages ()
  (require-user
    (let ((messages (loop for pair in (post-parameters*)
                          when (and (string= (car pair) "message-id")
                                    (scan +number-scanner+ (cdr pair)))
                          collect (parse-integer (cdr pair))))
          (next (or (post-parameter "next") (referer))))

      (cond
        ((not (post-parameter "message-id"))
         (flash "Please select at least one message first")
         (see-other next))

        ((post-parameter "move-to-inbox")
         (dolist (id messages)
           (let ((message (gethash id *db-messages*)))
             (when (or (not (member *userid* (getf (message-folders message) :inbox)))
                       (member *userid* (getf (message-folders message) :compost)))
               (update-folder-data message :inbox))))
         (see-other next))

        ((post-parameter "mark-read")
         (dolist (id messages)
           (let ((message (gethash id *db-messages*)))
             (when (or (not (eql (message-latest-comment message)
                              (cdr (assoc-assoc *userid* (message-people message)))))
                       (member *userid* (getf (message-folders message) :unread)))
               (update-folder-data message :read :last-read-comment (message-latest-comment message)))))
         (see-other next))

        ((post-parameter "mark-unread")
         (dolist (id messages)
           (let ((message (gethash id *db-messages*)))
             (unless (member *userid* (getf (message-folders message) :unread))
               (update-folder-data message :unread :last-read-comment :unread))))
         (see-other next))

        ((post-parameter "discard")
         (dolist (id messages)
           (let ((message (gethash id *db-messages*)))
             (when (or (member *userid* (getf (message-folders message) :inbox))
                       (member *userid* (getf (message-folders message) :unread))
                       (not (member *userid* (getf (message-folders message) :compost))))
               (update-folder-data message :compost))))
         (see-other next))

        ((post-parameter "delete")
         (dolist (id messages)
           (let ((message (gethash id *db-messages*)))
             (when (or (member *userid* (getf (message-folders message) :inbox))
                       (member *userid* (getf (message-folders message) :unread))
                       (member *userid* (getf (message-folders message) :compost))
                       (not (member *userid* (getf (message-folders message) :deleted))))
               (update-folder-data message :deleted))))
         (see-other next))

        (t
         (let* ((message-id (parse-integer (post-parameter "message-id")))
                (message (db message-id))
                (group-id (getf message :group-id)))
           (cond
             ((post-parameter "deny-group-membership-request")
              (if (group-admin-p group-id)
                (progn
                  (delete-group-membership-request message-id)
                  (see-other next))
                (permission-denied)))

             ((post-parameter "approve-group-membership-request")
              (if (group-admin-p group-id)
                (progn
                  (approve-group-membership-request message-id)
                  (see-other next))
                (permission-denied)))

             ((post-parameter "decline-group-membership-invitation")
              (if (eq (caaar (getf message :people)) *userid*)
                (progn
                  (update-folder-data (gethash message-id *db-messages*)
                                      :deleted)
                  (see-other next))
                (permission-denied)))

             ((post-parameter "accept-group-membership-invitation")
              (if (eq (caaar (getf message :people)) *userid*)
                (progn
                  (accept-group-membership-invitation message-id)
                  (flash (s+ "You have joined " (db group-id :name)))
                  (see-other next))
                (permission-denied))))))))))



