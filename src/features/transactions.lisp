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

(defun migrate-to-new-transaction-format ()
  (dolist (id (hash-table-keys *db*))
    (let* ((data (db id))
           (type (getf data :type)))
      (when (eq type :reply)
        (modify-db id :type :transaction)))))

(defun create-transaction (&key on text action match-id pending-deletion (userid *userid*))
  (let* ((time (get-universal-time))
         (on-item (db on))
         (by (getf on-item :by))
         (participants (list userid by))
         (senders (mailbox-ids (list userid)))
         (bys (mailbox-ids (list by)))
         (sender-boxes (mapcar #'(lambda (mailbox)
                                   (cons mailbox :read))
                               senders))
         (by-boxes (mapcar #'(lambda (mailbox)
                                   (cons mailbox :unread))
                               bys))
         (people (append by-boxes sender-boxes))
         (people-ids (mapcar #'car (remove-duplicates (append senders bys))))
         (message-folders (list :inbox people-ids
                                :unread (remove userid people-ids)))
         (id (insert-db (if pending-deletion
                          (list :type :transaction
                                :on on
                                :deleted-item-text (getf on-item :text)
                                :deleted-item-details (getf on-item :details)
                                :deleted-item-title (getf on-item :title)
                                :deleted-item-type (getf on-item :type)
                                :by userid
                                :participants participants
                                :message-folders message-folders
                                :people people
                                :created time)
                          (list :type :transaction
                                :on on
                                :by userid
                                :participants participants
                                :message-folders message-folders
                                :people people
                                :log (list (list :time time :party (list userid) :action action))
                                :created time)))))

      (when text (create-comment :on id
                                 :by (list userid)
                                 :text text
                                 :time (+ time 1) ; if there is both text/action, they need separate times for sorting in transaction log UI display
                                 ))

    (when match-id
      (case (getf on-item :type)
        (:offer (hide-matching-offer match-id on))
        (:request (hide-matching-offer on match-id))))

    id))

(defun transaction-history
  (transaction-id
   on-type
   inventory-by
   latest-seen
   &optional (transaction (db transaction-id))
   &aux (actions (getf transaction :log))
        (comments (gethash transaction-id *comment-index*))
        (history))

  (dolist (action actions)
    (sort (cons action history) #'> :key :time))

  (dolist (comment-id comments)
    (let* ((data (db comment-id))
           (participants (getf transaction :participants))
           (by (car (getf data :by)))
           (for (cdr (getf data :by)))
           (bydata (db by))
           (text (if (and (equal event (first events))
                          (getf transaction :deleted-item-type))
                   (deleted-invalid-item-reply-text
                     (db (car (remove by participants)) :name)
                     (getf bydata :name)
                     (case (getf transaction :deleted-item-type)
                       (:offer "offer")
                       (:request "request"))
                     (getf data :text))
                   (getf data :text))))
      (sort (cons (list :time (getf data :created)
                        :by by
                        :by-name (getf bydata :name)
                        :for for
                        :for-name (db for :name)
                        :text text)
                  history)
            #'>
            :key :time)))

  (html
    (dolist (event events)
      (let* ((data (awhen (getf event :comment) (db it)))
             )

         (str (conversation-comment-html
                data
                by
                (db by :name)
                for
                (db for :name)
                text
                (when (>= (or latest-seen 0)
                          (getf event :comment)))))))))

(defun transaction-action-html (log-event on-type inventory-by transaction-initiator)
  (html
    (:strong
      (str (case on-type
             (:offer
               (case (getf log-event :action)
                 (:requested
                   (s+ " requested to recieve this offer from " (db inventory-by :name)) )
                 (:offered
                   (s+ " agreed to share this offer with " (db transaction-initiator :name)))
                 ))))
      (awhen (cdr (getf log-event :party))
        (str (s+ " on behalf of " (db it :name))))
      "."
      )))

(defun transaction-comments (transaction-id latest-seen)
  (html
    (dolist (comment-id (gethash transaction-id *comment-index*))
      (let* ((data (db comment-id))
             (by (car (getf data :by)))
             (for (cdr (getf data :by)))
             (text (getf data :text)))

        (when data
           (str (conversation-comment-html data
                                          by
                                          (db by :name)
                                          for
                                          (db for :name)
                                          text
                                          (when (>= (or latest-seen 0)
                                                    comment-id)))))))))
(defun transaction-html
  (data
   with
   on-item
   comments-html
   &key deleted-type
        on-type
        other-party-name
        other-party-link
        transaction-options
        speaking-for
        error
   &aux (on-type-string (string-downcase (symbol-name on-type)))
        (inventory-url))

   (setf inventory-url
         (case on-type
           (:offer
            (html (:a :href (strcat "/offers/" (getf data :on)) "offer")))
           (:request
            (html (:a :href (strcat "/requests/" (getf data :on)) "request")))
           (t (case deleted-type
                (:offer "offer")
                (:request "request")
                (t (html
                     (:span :class "none" "deleted offer or request")))))))

  (standard-page
    "Transaction"
    (html
      (str (menu-horiz "actions"
                       (html (:a :href "/messages" "back to messages"))
                       (html (:a :href "#reply" "reply"))
                       (when (and (eql (getf data :by) *userid*)
                                  (eql on-type :offer))
                         (html
                           (:a :href
                               (str (url-compose "/gratitude/new"
                                                 "subject"
                                                 (getf on-item :by)))
                                "express gratitude")))))
      (str
        (card
          (html
            (if (eql (getf data :by) *userid*)
              (htm
                (:p
                "You replied to "
                (str other-party-link)
                "'s "
                (str inventory-url)
                ":"))
              (htm
                (:p
                  "A conversation with "
                  (str other-party-link)
                  " about "
                  (if (eq (getf on-item :by) *userid*)
                    (str "your ")
                    (str (s+ (db (getf on-item :by) :name)
                             "'s ")))
                  (str inventory-url)
                  ":")))

            (htm (:blockquote :class "review-text"
                   (awhen (getf on-item :title)
                     (htm
                       (:strong (str it))
                       (:br)
                       (:br)))
                   (str (html-text (or (getf on-item :details)
                                       (getf data :deleted-item-text)))))))))

      (str comments-html)

      (:form :method "post" :action (script-name*) :class "item"
       (flet ((transaction-button (status icon request-text offer-text)
                (when (find status transaction-options :test #'string=)
                  (html
                    (str icon)
                    (:button :type "submit"
                     :class "yes"
                     :name "transaction-action"
                     :value status
                     (str (if (eq on-type :request)
                            request-text offer-text)))
                    (:br)))))

         (str (transaction-button
                "will-give"
                (icon "offers")
                (s+ "I want to fulfill " other-party-name "'s request.")
                (s+ "I want to share this offering with " other-party-name ".")))

         (str (transaction-button
                "withhold"
                nil
                (s+ "I can't fulfill " other-party-name "'s request at this time.")
                (s+ "I can't share this offering with " other-party-name " at this time.")))

         (str (transaction-button
                "already-gave"
                (icon "share")
                (s+ "I have fulfilled " other-party-name "'s request.")
                (s+ "I have shared this offering with " other-party-name ".")))

         (str (transaction-button
                "want"
                (icon "requests")
                (s+ "I want to recieve what " other-party-name " is offering me.")
                (s+ "I want to recieve this offer from " other-party-name ".")))

         (str (transaction-button
                "refuse"
                nil
                (s+ "I don't want what " other-party-name " has offered me.")
                (s+ "I don't want this offer from " other-party-name ".")))

         (str (transaction-button
                "already-received"
                (icon "share")
                (s+ "I have received what " other-party-name " has offered me.")
                (s+ "I have received this offer from " other-party-name ".")))

         (str (transaction-button
                "dispute"
                nil
                (s+ "I have not yet received what " other-party-name " has offered me.")
                (s+ "I have not yet received this offer from " other-party-name "."))))
        )

      (:div :class "item" :id "reply"
        (:h4 "post a reply")
        (flet ((radio-selector (status request-text offer-text)
                 (when (find status transaction-options :test #'string=)
                   (html
                     (:div ;:class "inline-block"
                       (:input :type "radio"
                               :name "action-type"
                               :value status)
                               (str (if (eq on-type :request)
                                      request-text offer-text)))))))

          (htm
            (:form :method "post" :action (script-name*)
              (:textarea :cols "150" :rows "4" :name "text")

              (:div :class (when (eq error :no-reply-type)
                             "error-border")

               )

          (:button :class "yes" :type "submit" :class "submit" "Send"))))))
    :selected "messages"))

(defun transaction-options-for-user
  (transaction-id
   &key (userid *userid*)
        (transaction (db transaction-id))
   &aux (transaction-mailboxes (mapcar #'car (getf transaction :people)))
        (inventory-item (db (getf transaction :on)))
        (inventory-type (getf inventory-item :type))
        (inventory-by (getf inventory-item :by))
        (inventory-by-self-p (or (eql userid inventory-by)
                                 (eql inventory-by
                                      (cdr (assoc userid transaction-mailboxes)))))
        (role (case inventory-type
                (:offer (if inventory-by-self-p :giver :reciever))
                (:request (if inventory-by-self-p :reciever :giver ))))
        (representing (if (or (eql userid inventory-by)
                              (eql userid (getf transaction :by)))
                        userid
                        (cdr (assoc userid transaction-mailboxes))))
        (give (list "already-gave"))
        (receive (list "already-received")))

  "Returns (1) a list of actions the user can take on a given transaction id and (2) the entity the user is representing (i.e. *userid* or a groupid)"

      (loop for event in (getf transaction :log)
           ;when (eql representing (car (getf event :party)))
            do (case (getf event :action)
                 (:offered (unless (find-string "will-give" give)
                             (pushnew "withhold" give :test #'string=)))
                 (:requested (unless (find-string "want" receive)
                               (pushnew "refuse" receive :test #'string=)))
                 (:given
                   (pushnew "dispute" receive :test #'string=)
                   (pushnew "already-recieved" receive :test #'string=))
                 (:gratitude-posted
                   (when (getf inventory-item :active)
                     (pushnew "gave-again" give :test #'string=)
                     (pushnew "received-again" receive :test #'string=)
                     (loop-finish))))
            finally (case role
                      (:giver (unless (find-string "withhold" give)
                                (push "will-give" give)))
                      (:reciever (unless (find-string "refuse" receive)
                                   (push "want" receive)))))

  (values (if inventory-by-self-p
            (case inventory-type (:offer give) (:request receive))
            (case inventory-type (:offer receive) (:request give)))
          representing))

(defun get-transaction (id)
"when called, (modify-db conversation-id :people '((userid . this-comment-id) (other-user-id . whatever)))"
  (require-user
    (setf id (parse-integer id))
    (let* ((message (gethash id *db-messages*))
           (people (message-people message))
           (valid-mailboxes (loop for person in people
                                  when (eql *userid* (caar person))
                                  collect person))
           (type (message-type message)))

      (if (eq type :transaction)
        (if valid-mailboxes
          (let* ((transaction (db id))
                 (person (assoc-assoc *userid* people))
                 (latest-comment (getf transaction :latest-comment))
                 (latest-seen (or (when (numberp (cdr person))
                                    (cdr person))
                                  latest-comment))
                 (on-item (db (getf transaction :on)))
                 (inventory-by (getf on-item :by))
                 (transaction-options)
                 (speaking-for)
                 (other-party)
                 (with (remove *userid* (getf transaction :participants)))
                 (deleted-type (getf transaction :deleted-item-type))
                 (on-type (getf on-item :type)))

            (multiple-value-bind (options for)
              (transaction-options-for-user id :transaction transaction)
              (setf transaction-options options)
              (setf speaking-for for))
            (setf other-party (car (remove speaking-for with)))

            (prog1
              (transaction-html transaction
                                with
                                on-item
                                (transaction-history id
                                                     on-type
                                                     inventory-by
                                                     latest-seen
                                                     transaction)
                                :other-party-name (db other-party :name)
                                :other-party-link (person-link other-party)
                                :speaking-for speaking-for
                                :transaction-options transaction-options
                                :deleted-type deleted-type
                                :on-type on-type)

              ; get most recent comment seen
              ; get comments for
              (when (or (not (eql (message-latest-comment message)
                                  (cdr (assoc-assoc *userid*
                                                    (message-people message)))))
                        (member *userid*
                                (getf (message-folders message) :unread)))
                (update-folder-data message :read :last-read-comment (message-latest-comment message)))))

          (permission-denied))
      (not-found)))))


(defun post-transaction (id)
  (require-active-user
    (setf id (parse-integer id))
    (aif (db id)
      (let* ((people (getf it :people))
             (mailbox (assoc-assoc *userid* people))
             (party (car mailbox)))
        (if party
         (cond
           ((post-parameter "text")
            (flash "Your message has been sent.")
            (contact-opt-out-flash (mapcar #'caar people))
            (let* ((time (get-universal-time))
                   (new-comment-id (create-comment :on id
                                                   :text (post-parameter "text")
                                                   :time time
                                                   :by party)))
              (send-metric* :message-sent new-comment-id)
              (amodify-db id :log (append
                                    it
                                    (list
                                      (list :time time
                                            :party party
                                            :action (post-parameter "transaction-action")
                                            :comment new-comment-id)))))
            (see-other (script-name*))))

         (permission-denied)))
      (not-found))))
