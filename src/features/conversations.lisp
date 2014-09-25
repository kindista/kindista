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

(defun create-conversation (&key people subject text (user *userid*) public)
  (let* ((time (get-universal-time))
         (senders (mailbox-ids (list user)))
         (recipients (mailbox-ids people))
         (mailboxes (append senders recipients))
         (people-data (mapcar #'list mailboxes))
         (people-ids (remove-duplicates (mapcar #'car mailboxes)))
         (message-folders (list :inbox people-ids))
         (id (insert-db (list :type :conversation
                              :participants (cons user people)
                              :people people-data
                              :message-folders message-folders
                              :public public
                              :subject subject
                              :created time))))

    (create-comment :on id :by (list user) :text text)
    id))

(defun get-conversations ()
  (see-other "/messages"))

(defun new-conversation (&key people subject text next single-recipient)
  (if people
    (standard-page
     "New conversation"
     (html
       (:div :class "item"
        (:h2 "New Conversation")
       (:div :class "item"
        (:form :method "post"
               :action "/conversations/new"
               :class "recipients"
          (:div :class "recipients"
            (:label "With:")
            (:menu :type "toolbar" :class "recipients"
              (unless people
                (htm (:li (:em "nobody yet"))))
              (dolist (person people)
                (htm
                  (:li
                    (str (getf (db person) :name))
                    (unless single-recipient
                      (htm (:button :class "text large x-remove" :type "submit" :name "remove" :value person " ⨯ "))))))
              (unless single-recipient
                (htm (:li (:button :type "submit" :class "text" :name "add" :value "new" "+ Add someone"))))))

          (when people
            (htm (:input :type "hidden" :name "people" :value (format nil "~{~A~^,~}" people))))
          (when next
              (htm (:input :type "hidden" :name "next" :value next)))
          (:p (:label "Subject: ") (:input :type "text" :name "subject" :value subject))
          (:textarea :rows "8" :name "text" (str text))
          (:p
            (:button :type "submit" :class "cancel" :name "cancel" "Cancel")
            (:button :class "yes" :type "submit"
                     :name "send"
                     "Send"))))))
     :selected "messages")
    (conversation-add-person :text text :next next)))

(defun conversation-add-person (&key people subject text next (results 'none))
  (standard-page
    "Add person to conversation"
    (html
      (:div :class "item"
       (:form :method "post"
              :class "recipients"
              :action "/conversations/new"
         (:button :type "submit" :class "simple-link green" :name "cancel-add" "↩ go back")
         (:h2 "Who would you like to add to the conversation?")
         (:h3 "Search for a person")
         (:input :type "text" :name "name")
         (:button :type "submit" :class "yes input-height" :name "search" "Search")

         (if (eq results 'none)
           (progn
             (htm
               (:h3 "Or, select one of your contacts:")
               (:menu :type "toolbar"
                 (dolist (contact (contacts-alphabetically *user*))
                   (htm (:li (:button :class "text" :type "submit" :value (car contact) :name "add" (str (cadr contact)))))))))
           (progn
             (htm
               (:h3 "Search results")
               (dolist (person results)
                 (str (id-button (car person) "add" (cdr person)))))))

         (:input :type "submit" :class "cancel" :value "Back")

         (when people
           (htm (:input :type "hidden" :name "people" :value (format nil "~{~A~^,~}" people))))
         (when next
           (htm (:input :type "hidden" :name "next" :value next)))
         (when subject
           (htm (:input :type "hidden" :name "subject" :value subject)))
         (when text
           (htm (:input :type "hidden" :name "text" :value (escape-for-html text))))

         )))
    :selected "messages"))

(defun get-conversations-new ()
  (require-active-user
    (if (getf *user* :pending)
       (progn
         (pending-flash "contact other Kindista members")
         (see-other (or (referer) "/home")))
      (new-conversation :people (parse-subject-list (get-parameter "people"))))))

(defun post-conversations-new ()
  (require-active-user
    (cond
      ((getf *user* :pending)
       (pending-flash "contact other Kindista members")
       (see-other (or (post-parameter "next") "/home")))

      ((or (post-parameter "cancel")
           (and (post-parameter "cancel-add")
                (not (post-parameter "people"))))
       (see-other (or (post-parameter "next") "/messages")))

      ((post-parameter "send")
       (let ((people (parse-subject-list (post-parameter "people") :remove (write-to-string *userid*)))
             (text (post-parameter "text"))
             (subject (post-parameter "subject")))
         (when (string= subject "")
           (setf subject nil))
         (when (string= text "")
           (setf text nil))
         (cond
           ((and people text subject)
            (flash "Your message has been sent.")
            (contact-opt-out-flash (append (list *userid*) people))
            (see-other (format nil (or (post-parameter "next")
                                       "/conversations/~A")
                               (create-conversation :people people
                                                    :public nil
                                                    :subject subject
                                                    :text text))))
           ((and people subject)
            "no text")
           ((and people text)
            "no subject")
           ((and subject text)
            "no recipients")
           (text
             "no people OR subject")
           (people
             "no text OR subject")
           (subject
             "no text OR recipients")
           (t
            "totally blank"))))

      ((post-parameter "add")
       (if (string= (post-parameter "add") "new")
         (conversation-add-person :people (parse-subject-list (post-parameter "people"))
                                  :text (post-parameter "text")
                                  :subject (post-parameter "subject")
                                  :next (post-parameter "next"))
         (new-conversation
           :text (post-parameter "text")
           :subject (post-parameter "subject")
           :next (post-parameter "next")
           :single-recipient (post-parameter "single-recipient")
           :people (parse-subject-list
                       (format nil "~A,~A" (post-parameter "add") (post-parameter "people"))))))

      ((post-parameter "search")
       (conversation-add-person :people (parse-subject-list (post-parameter "people"))
                                :text (post-parameter "text")
                                :subject (post-parameter "subject")
                                :next (post-parameter "next")
                                :results (search-people (post-parameter "name"))))
      (t
       (new-conversation
         :text (post-parameter "text")
         :subject (post-parameter "subject")
         :people (parse-subject-list
                   (post-parameter "people")
                   :remove (post-parameter "remove")))))))


(defun go-conversation (id)
  (moved-permanently (strcat "/conversations/" id)))

(defun conversation-comments (conversation-id latest-seen)
  (html
    (dolist (comment-id (gethash conversation-id *comment-index*))
      (let* ((data (db comment-id))
             (by (car (getf data :by)))
             (for (cdr (getf data :by)))
             (text (getf data :text)))

        (when data
           (str (conversation-comment-html data
                                           by (db by :name)
                                           for
                                           (db for :name)
                                           text
                                           (when (>= (or latest-seen 0)
                                                     comment-id)))))))))

(defun conversation-comment-html (data by by-name for for-name text newp)
  (card
    (html
      (str (h3-timestamp (getf data :created)))
      (:p (:a :href (s+ "/people/" (username-or-id by))
           (str by-name))
            (when for
              (htm
                " for "
                (:a :href (s+ "/groups/" (username-or-id ))
                 (str for-name)))))

      (:p :class (when newp "new")
        (str (regex-replace-all "\\n" text "<br>"))))))

(defun conversation-html
  (data
   with
   comments-html
   &key error)

  (standard-page
    (aif (getf data :subject)
      (ellipsis it :length 24)
      "Conversation")
    (html
      (str (menu-horiz (html (:a :href "/messages" "back to messages"))
                       (html (:a :href "#reply" "reply")))
                      ;(when (eq type :conversation)
                      ;  (html (:a :href (strcat "/conversations/" id "/leave") "leave conversation")))
                      ;  removed until we add the ability for
                      ;  individual members of a group to leave the
                      ;  conversation and for the group to leave
                      ;  when its members have
                       )
      (str
        (card
          (html
            (when (getf data :subject)
              (htm
                (:h2 "Subject: " (str (getf data :subject)))))

              (if with
                (htm (:p "with " (str (name-list-all with))))
                (htm (:p :class "error" "Everybody else has left this conversation."))))))


      (:div :class "item" :id "reply"
        (:h4 "post a reply")

        (:form :method "post" :action (script-name*)
          (:textarea :cols "150" :rows "4" :name "text")

          (:div :class (when (eq error :no-reply-type)
                         "error-border"))

          (:button :class "yes" :type "submit" :class "submit" "Send")))

      (str comments-html))
    :selected "messages"))

(defun get-conversation (id)
"when called, (modify-db conversation-id :people '((userid . this-comment-id) (other-user-id . whatever)))"
  (require-user
    (setf id (parse-integer id))
    (let* ((message (gethash id *db-messages*))
           (people (message-people message))
           (valid-mailboxes (loop for person in people
                                  when (eql *userid* (caar person))
                                  collect person))
           (type (message-type message)))

      (case type
        (:conversation
          (if valid-mailboxes
            (let* ((conversation (db id))
                   (person (assoc-assoc *userid* people))
                   (latest-comment (getf conversation :latest-comment))
                   (latest-seen (or (when (numberp (cdr person))
                                      (cdr person))
                                    latest-comment))
                   (with (remove *userid* (getf conversation :participants))))

              (prog1
                (conversation-html conversation
                                   with
                                   (conversation-comments id
                                                          latest-seen))

                ; get most recent comment seen
                ; get comments for
                (when (or (not (eql (message-latest-comment message)
                                    (cdr (assoc-assoc *userid*
                                                      (message-people message)))))
                          (member *userid*
                                  (getf (message-folders message) :unread)))
                  (update-folder-data message
                                     :read
                                     :last-read-comment (message-latest-comment message)))))

          (permission-denied)))
      (:transaction
        (see-other (strcat "/transactions/" id)))
      (t (not-found))))))

(defun get-conversation-leave (id)
  (require-user
    (setf id (parse-integer id))
    (let ((it (db id)))
      (if (and it (eql (getf it :type) :conversation))
        (if (member *userid* (mapcar #'first (getf it :people)))
          (standard-page
            "Leave conversation"
            (html

              (:h2 "Are you sure you want to leave this conversation?")

              (:p "You won't be able to re-join the conversation after leaving.")

              (:p (:strong "Subject: ") (str (getf it :subject)))

              (:form :method "post" :action (strcat "/conversations/" id)
                (:a :href (strcat "/conversations/" id) "No, I didn't mean it!")
                (:button :class "yes" :type "submit" :class "submit" :name "leave" "Yes")))

            :selected "messages")

          (permission-denied))
        (not-found)))))

(defun post-conversation (id)
  (require-active-user
    (setf id (parse-integer id))
    (aif (db id)
      (let* ((people (getf it :people))
             (mailbox (assoc-assoc *userid* people))
             (party (car mailbox)))
        (if party
         (cond
           ((post-parameter "leave")
            (with-locked-hash-table (*person-conversation-index*)
              (asetf (gethash *userid* *person-conversation-index*)
                     (remove id it :key #'result-id)))
            (amodify-db id :people (remove *userid* it :key #'caar))
            (see-other "/messages"))

           ((post-parameter "text")
            (flash "Your message has been sent.")
            (contact-opt-out-flash (mapcar #'caar people))
            (create-comment :on id
                            :text (post-parameter "text")
                            :by party)
            (see-other (script-name*))))

         (permission-denied)))
      (not-found))))
