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
         (id (insert-db (list :type :conversation
                              :people people
                              :public public
                              :subject subject
                              :created time))))

    (create-comment :on id :by user :text text)

    (dolist (recipient (remove *userid* people))
      ; notification??
      )

    id))

(defun index-conversation (id data)
  (dolist (pair (getf data :people))
    (push (cons id (cdr pair)) (gethash (car pair) *person-conversation-index*))))

(defun conversation-card (uri time from-id &key (from-name (getf (db from-id) :name)))
  (card
    (html
      (str (h3-timestamp time))
      (:p (:a :href uri "A new message") " from " (:a :href (s+ "/people/" (username-or-id from-id)) (str from-name)))
      (:div :class "actions"
        (str (card-button "Hide" (s+ uri "/hide") :method "POST"))
        " &middot; "
        (str (card-button "Block" (s+ "/people/" (username-or-id from-id) "/block")))
        " &middot; "
        (str (card-button "Report" (s+ "/people/" (username-or-id from-id) "/report")))))))

(defun conversation-info (pair)
  (cons (latest-comment (car pair)) pair))

(defun get-conversations ()
  (require-user
    (standard-page

      "Conversations"

      (html
        (:h1 "Conversations") 
          (str (menu-horiz "actions"
                           (html (:a :href "/conversations/new" "start a new conversation"))))

          (let ((conversations (sort (mapcar #'conversation-info
                                             (gethash *userid* *person-conversation-index*))
                                     #'> :key #'first)))
            (dolist (conversation conversations)
              (htm
                (:p (:a :href (strcat "/conversations/" (cadr conversation))
                     (str (getf (db (cadr conversation)) :subject)))
                    " latest comment by: " (str (db (db (car conversation) :by) :name))
                    " at: " (str (db (car conversation) :created))
                    " id: " (str (car conversation))
                    " seen?: " (str (if (eql (or (car conversation) 0) (or (cddr conversation) 0)) "yes" "no")))))))

      :right (html
               (:div :class "item"
                (:a :href "/conversations/new" "start a new conversation")))

      :selected "conversations")))

(defun get-conversations ()
  (require-user
    (standard-page

      "Conversations"

      (html
        (:h1 "Conversations") 
          (str (menu-horiz "actions"
                           (html (:a :href "/conversations/new" "start a new conversation"))))

          ; get a list of unread, unhidden messages
          (str (new-message-card  "/message/123" 3570897552 1))
          (str (new-message-card  "/message/123" 3570897552 1))
          (str (new-message-card  "/message/123" 3570897552 1))
          (str (new-message-card  "/message/123" 3570897552 1)))

      :right (html
               (:div :class "item"
                (:a :href "/conversations/new" "start a new conversation")))

      :selected "conversations")))

(defun new-conversation (&key people subject text next)
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
          (:label "With:")
          (:menu :class "recipients"
           (unless people
             (htm (:li (:em "nobody yet"))))
           (dolist (person people)
             (htm
               (:li 
                 (str (getf (db person) :name)) 
                 (:button :class "text large" :type "submit" :name "remove" :value person " ⨯ ")))))
          (when people
            (htm (:input :type "hidden" :name "people" :value (format nil "~{~A~^,~}" people))))
          (when next
            (htm (:input :type "hidden" :name "next" :value next)))
          (:p (:button :type "submit" :class "text" :name "add" :value "new" "+ Add someone"))
          (:p (:label "Subject: ") (:input :type "text" :name "subject" :value subject))
          (:textarea :rows "8" :name "text" (str text))
          (:p  
            (:button :type "submit" :class "cancel" :name "cancel" "Cancel")
            (:button :class "yes" :type "submit" 
                     :name "send" 
                     "Send"))))))
     :selected "conversations")
    (conversation-add-person :text text :next next)))

(defun conversation-add-person (&key people subject text next (results 'none))
  (standard-page
    "Add person to conversation"
    (html
      (:div :class "item"
       (:h2 "Who would you like to add?")
       (:h3 "Search for a person")
       (:form :method "post" :action "/conversations/new"
         (:input :type "text" :name "name")
         (:input :type "submit" :class "submit" :name "search" :value "Search")

         (if (eq results 'none)
           (progn
             (htm
               (:h3 "Select one of your contacts")
               (:menu
                 (dolist (contact (contacts-alphabetically *user*))
                   (htm (:li (:button :class "text" :type "submit" :value (car contact) :name "add" (str (cadr contact)))))))))
           (progn
             (htm
               (:h3 "Search results")
               (dolist (person results)
                 (str (person-button (car person) (cdr person) "add"))))))

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
    :selected "conversations"))

(defun get-conversations-new ()
  (require-user
    (new-conversation :people (parse-subject-list (get-parameter "people")))))

(defun post-conversations-new ()
  (require-user
    (cond
      ((post-parameter "cancel")
       (see-other (or (post-parameter "next") "/home")))
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
            (see-other (format nil (or (post-parameter "next") 
                                       "/conversations/~A")
                               (create-conversation :people (mapcar #'list (cons *userid* people))
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

(defun get-conversation (id)
  (require-user
    (setf id (parse-integer id))
    (aif (db id)
      (if (member *userid* (mapcar #'first (getf it :people)))
        (standard-page
          (getf it :subject)
          (html
            (:p (:a :href "/conversations" "Back to conversations"))
            (:h2 "Subject: " (str (getf it :subject)))
            (:h3 "With: " (iter (for id in (mapcar #'first (getf it :people)))
                                (for i from (length (getf it :people)) downto 0)
                                (when (eql i 1)
                                  (htm "and "))  
                                (htm (:a :href (s+ "/people/" (username-or-id id)) (str (db id :name))))  
                                (unless (eql i 1)
                                  (htm ", "))))

            (dolist (comment-id (sort (gethash id *comment-index*)
                                      #'< :key #'created))
              (let* ((data (db comment-id))
                     (by (getf data :by))
                     (bydata (db by)))
                (htm
                  (:p :class (when (>= (or (cdr (assoc id (gethash *userid* *person-conversation-index*))) 0) comment-id) "new") 
                    (:strong (:a :href (s+ "/people/" (username-or-id by)) (str (getf bydata :name)))) 
                    ": "
                    (str (db comment-id :text))
                    (:div :class "timestamp"
                      (str (getf data :created)))))))

            (:form :action (script-name*) :method "post"
              (:textarea :name "text")
              (:button :type "submit" "Send")) 

            (with-locked-hash-table (*person-conversation-index*)
              (setf (cdr (assoc id (gethash *userid* *person-conversation-index*))) (latest-comment id)))

            ; get most recent comment seen
            ; get comments for 

          )
          :right (html
                   (:form :action (script-name*) :method "post"
                     (:input :type "submit" :name "leave" :value "Leave conversation"))))
        (permission-denied))
      (not-found))))

(defun post-conversation (id)
  (require-user
    (setf id (parse-integer id))
    (aif (db id)
      (if (member *userid* (mapcar #'first (getf it :people)))
        (cond
          ((post-parameter "leave")
           (with-locked-hash-table (*person-conversation-index*)
             (asetf (gethash *userid* *person-conversation-index*)
                    (remove id it :key #'first)))
           (amodify-db id :people (remove *userid* it))
           (see-other "/conversations"))

          ((post-parameter "text")
           (create-comment :on id :text (post-parameter "text"))
           (see-other (script-name*))))

        (permission-denied))
      (not-found)))
  
  )
