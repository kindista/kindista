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
    id))

(defun index-conversation (id data)
  (let ((result (make-result :id id
                             :time (db (getf data :latest-comment) :created)
                             :people (getf data :people)
                             :type :conversation)))
    (setf (gethash id *db-results*) result)
    (with-locked-hash-table (*person-conversation-index*)
      (dolist (pair (getf data :people))
        (push result (gethash (car pair) *person-conversation-index*))))))

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
            (:menu :class "recipients"
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
       (:h2 "Who would you like to add to the conversation?")
       (:form :method "post" :action "/conversations/new"
         (:button :type "submit" :class "cancel" :name "cancel-add" "Cancel")
         (:h3 "Search for a person")
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
    :selected "messages"))

(defun get-conversations-new ()
  (require-user
    (new-conversation :people (parse-subject-list (get-parameter "people")))))

(defun post-conversations-new ()
  (require-user
    (cond
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

(defun get-conversation (id)
  (require-user
    (setf id (parse-integer id))
    (let* ((it (db id))
           (people (mapcar #'car (getf it :people)))
           (type (getf it :type)))
      (if (or (eq type :reply)
              (eq type :conversation))
        (if (member *userid* people)
          (let ((latest-seen (cdr (assoc *userid* (getf it :people))))
                (with (remove *userid* people)))
            (standard-page
              (ellipsis (getf it :subject) 24)
              (html
                (str (menu-horiz "actions"
                                 (html (:a :href "/messages" "back to messages"))
                                 (html (:a :href "#reply" "reply"))
                                 (when (eq (getf it :type) :conversation)
                                   (html (:a :href (strcat "/conversations/" id "/leave") "leave conversation")))))
                (str
                  (card
                    (html
                      (when (getf it :subject)
                        (htm
                          (:h2 "Subject: " (str (getf it :subject)))))
                      (case type
                        (:conversation
                          (if with
                            (htm (:p "with " (str (name-list-all with))))
                            (htm (:p :class "error" "Everybody else has left this conversation."))))
                        (:reply
                          (let ((item (db (getf it :on))))
                            (if (eql (getf it :by) *userid*)
                              (htm
                                (:p
                                "You replied to "
                                (str (person-link (or (getf item :by)
                                                      (first with))))
                                "'s "
                                (case (getf item :type)
                                  (:offer
                                   (htm (:a :href (strcat "/offers/" (getf it :on)) "offer")))
                                  (:request
                                   (htm (:a :href (strcat "/requests/" (getf it :on)) "request")))
                                  (t (str "deleted offer or request"))) 
                                ":"))
                              (htm
                                (:p
                                  "A conversation with "
                                  (str (person-link (getf it :by))) 
                                  " about your "
                                  (case (getf item :type)
                                    (:offer
                                     (htm (:a :href (strcat "/offers/" (getf it :on)) "offer")))
                                    (:request
                                     (htm (:a :href (strcat "/requests/" (getf it :on)) "request")))
                                    (t (str "deleted offer or request")))
                                  ":")))
                            (htm (:blockquote (str (regex-replace-all "\\n" (getf item :text) "<br>"))))))))))

                (dolist (comment-id (gethash id *comment-index*))
                  (let* ((data (db comment-id))
                         (by (getf data :by))
                         (bydata (db by)))
                    (str
                      (card
                        (html
                          (str (h3-timestamp (getf data :created)))
                          (:p (:a :href (s+ "/people/" (username-or-id by)) (str (getf bydata :name)))) 
                          (:p :class (when (>= (or latest-seen 0) comment-id) "new") 
                            (str (regex-replace-all "\\n" (db comment-id :text) "<br>"))))))))

                (:div :class "item" :id "reply"
                  (:h4 "post a reply") 
                  (:form :method "post" :action (script-name*)
                    (:table :class "post"
                      (:tr
                        (:td (:textarea :cols "150" :rows "4" :name "text"))
                        (:td
                          (:button :class "yes" :type "submit" :class "submit" "Send")))))) 

                (unless (eql (cdr (assoc *userid* (db id :people))) (latest-comment id))
                  (amodify-db id :people (progn (setf (cdr (assoc *userid* it)) (latest-comment id)) it)))

                ; get most recent comment seen
                ; get comments for 

              )

              :selected "messages"))

          (permission-denied)) 
      (not-found)))))

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
      (if (member *userid* (mapcar #'first (getf it :people)))
        (cond
          ((post-parameter "leave")
           (with-locked-hash-table (*person-conversation-index*)
             (asetf (gethash *userid* *person-conversation-index*)
                    (remove id it :key #'result-id)))
           (amodify-db id :people (remove *userid* it :key #'car))
           (see-other "/messages"))

          ((post-parameter "text")
           (flash "Your message has been sent.")
           (create-comment :on id :text (post-parameter "text"))
           (see-other (script-name*))))

        (permission-denied))
      (not-found)))
  
  )
