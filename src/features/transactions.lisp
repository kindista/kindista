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
         (log (when action (list (list :time time :party (list userid) :action action))))
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
                                :log log
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
        (inventory-by-name (db inventory-by :name))
        (comments (gethash transaction-id *comment-index*))
        (history))

  (dolist (action actions)
    (when (getf action :action)
      (sort (push action history) #'> :key #'(lambda (entry)
                                               (getf entry :time)))))

  (dolist (comment-id comments)
    (let* ((data (db comment-id))
           (participants (getf transaction :participants))
           (by (car (getf data :by)))
           (for (cdr (getf data :by)))
           (bydata (db by))
           (text (if (and (equal comment-id (first comments))
                          (getf transaction :deleted-item-type))
                   (deleted-invalid-item-reply-text
                     (db (car (remove by participants)) :name)
                     (getf bydata :name)
                     (case (getf transaction :deleted-item-type)
                       (:offer "offer")
                       (:request "request"))
                     (getf data :text))
                   (getf data :text))))

      (sort (push (list :time (getf data :created)
                        :id comment-id
                        :data data
                        :by by
                        :by-name (getf bydata :name)
                        :for for
                        :for-name (db for :name)
                        :text text)
                  history)
            #'>
            :key #'(lambda (entry) (getf entry :time)))))

  (html
    (dolist (event history)
      (pprint event)
      (terpri)
      (acond
       ((getf event :action)
        (str (transaction-action-html event
                                    on-type
                                    inventory-by-name
                                    (db (getf transaction :by) :name))))

       ((getf event :text)
        (str (conversation-comment-html
                (getf event :data)
                (getf event :by)
                (getf event :by-name)
                (getf event :for)
                (getf event :for-name)
                (getf event :text)
                (when (>= (or latest-seen 0)
                          (getf event :id))))))))))

(defun transaction-action-html (log-event on-type inventory-by-name transaction-initiator-name)
  (case (getf log-event :action)
    (:gratitude-posted
      (gratitude-activity-item (gethash (getf log-event :comment) *db-results*)))
    (t
      (card
        (html
          (str (h3-timestamp (getf log-event :time)))
          (:p
            (:strong
              (str (person-link (car (getf log-event :party))))
              (awhen (cdr (getf log-event :party))
                (htm " (on behalf of "
                     (str (group-link it)) ")" ))
              (str (case on-type
                     (:offer
                       (case (getf log-event :action)
                         (:requested
                           (s+ " requested to recieve this offer from "
                               inventory-by-name) )
                         (:offered
                           (s+ " agreed to share this offer with "
                               transaction-initiator-name))
                         (:declined
                           (s+ " no longer wishes to receive this offer from "
                               inventory-by-name))
                         (:gave
                           (s+ " has shared this offer with "
                               transaction-initiator-name))
                         (:received
                           (s+ " has received this offer from "
                               inventory-by-name))
                         (:disputed
                           (s+ " claims that they have not yet received this offer from "
                               inventory-by-name))
                         ))
                     (:request
                       (case (getf log-event :action)
                         (:requested
                           (s+ " wants to receive what "
                               transaction-initiator-name
                               " is offering") )
                         (:offered
                           (s+ " agreed to fulfill this request from "
                               inventory-by-name))
                         (:declined
                           (s+ " no longer wishes to receive this request from "
                               transaction-initiator-name))
                         (:gave
                           (s+ " has fulfilled this request from "
                               inventory-by-name))
                         (:received
                           (s+ " has received this request from "
                               transaction-initiator-name))
                         (:disputed
                           (s+ " claims that they have not yet received this request from "
                               transaction-initiator-name))
                         ))
                     ))
              "."
              )))))))

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

(defun transaction-comment-input (transaction-id &key error)
  (html
    (:div :class "item" :id "reply"
      (:h4 "post a reply")
      (:form :method "post" :action (strcat "/transactions/" transaction-id)
        (:textarea :cols "150" :rows "4" :name "text")
        (:div :class (when (eq error :no-reply-type)
                       "error-border"))
        (:button :class "cancel" :type "submit" :name "cancel" "Cancel")
        (:button :class "yes" :type "submit" :name "submit" "Send")))))

(defun transaction-buttons-html
  (transaction-options
   other-party-name
   on-type
   url)
  (html
    (:div :class "transaction-options item"

     (when (find "post-gratitude" transaction-options :test #'string=)
       (htm
         (:a :href (url-compose url "post-gratitude" "t")
          (str (icon "checkmark"))
          (str (s+ "I have gratitude to share about "
                   other-party-name
                   " for this gift.")))))

     (:form :method "post" :action url
      (flet ((transaction-button (status icon request-text offer-text)
               (when (find status transaction-options :test #'string=)
                 (html
                   (:div :class "transaction-option"
                     (:button :type "submit"
                              :class "green simple-link"
                              :name "transaction-action"
                              :value status
                      (str icon)
                      (str (if (eq on-type :request)
                             request-text offer-text))))))))

        (str (transaction-button
               "will-give"
               (icon "offers")
               (s+ "I want to fulfill " other-party-name "'s request.")
               (s+ "I want to share this offering with " other-party-name ".")))

        (str (transaction-button
               "withhold"
               (icon "withhold")
               (s+ "I can't fulfill " other-party-name "'s request at this time.")
               (s+ "I can't share this offer with " other-party-name " at this time.")))

        (str (transaction-button
               "already-given"
               (icon "checkmark")
               (s+ "I have fulfilled " other-party-name "'s request.")
               (s+ "I have shared this offer with " other-party-name ".")))

        (str (transaction-button
               "want"
               (icon "requests")
               (s+ "I want to recieve what " other-party-name " is offering me.")
               (s+ "I want to recieve this offer from " other-party-name ".")))

        (str (transaction-button
               "already-received"
               (icon "checkmark")
               (s+ "I have received what " other-party-name " has offered me.")
               (s+ "I have received this offer from " other-party-name ".")))

        (str (transaction-button
               "decline"
               (icon "decline")
               (s+ "I don't want what " other-party-name " has offered me.")
               (s+ "I no longer want this offer from " other-party-name ".")))

        (str (transaction-button
               "dispute"
               (icon "caution")
               (s+ "I have <strong>not</strong> yet received what "
                   other-party-name " has offered me.")
               (s+ "I have <strong>not</strong> yet received this offer from " other-party-name ".")))))

       (:a :href (url-compose url "add-comment" "t")
        (str (icon "comment"))
        (str (s+ "I have a question or comment for " other-party-name ".")))) ))

(defun transaction-html
  (transaction-id
   on-item
   history-html
   form-elements-html
   &key (data (db transaction-id))
        deleted-type
        on-type
        other-party-link
        error
   &aux (inventory-url))

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
                (str other-party-link)
                " has responded to "
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
                                     (getf data :deleted-item-text))))))))

      (str form-elements-html)
      (str history-html))

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
                (:offer (if inventory-by-self-p :giver :receiver))
                (:request (if inventory-by-self-p :receiver :giver ))))
        (representing (if (or (eql userid inventory-by)
                              (eql userid (getf transaction :by)))
                        userid
                        (cdr (assoc userid transaction-mailboxes))))
        (current-event (find userid
                             (getf transaction :log)
                             :test #'eql
                             :key #'(lambda (event)
                                       (car (getf event :party)))
                             :from-end t))
        (other-party-event (find-if-not
                             #'(lambda (event)
                                 (if representing
                                   (eql representing (cdr (getf event :party)))
                                   (eql userid (car (getf event :party)))))
                             (getf transaction :log)
                             :from-end t))
        (options))

  "Returns (1) a list of actions the user can take on a given transaction id and (2) the entity the user is representing (i.e. *userid* or a groupid)"

  (setf options
        (case role
          (:receiver
             (case (getf current-event :action)
               (:requested
                 (case (getf other-party-event :action)
                   (:gave '("post-gratitude" "dispute"))
                   (t '("decline" "already-received"))))
               (:declined '("want" "already-received"))
               (:disputed '("already-received"))
               (:received '("post-gratitude"))
               (t (case (getf other-party-event :action)
                    (:gave '("already-received" "dispute"))
                    (t '("want" "already-received"))))))
          (:giver
            (case (getf current-event :action)
              (:offered
                (case (getf other-party-event :action)
                  (:received nil)
                  (t '("withheld" "already-given"))))
              (:gave
                (when (and (eql (getf other-party-event :action)
                              :gratitude-posted)
                           (getf inventory-item :active))
                  '("will-give" "already-given")))
              (t '("will-give" "already-given"))))))

  (values options
          representing
          role
          current-event
          other-party-event))

(defun get-transaction (id)
"when called, (modify-db conversation-id :people '((userid . this-comment-id) (other-user-id . whatever)))"
  (require-user
    (setf id (parse-integer id))
    (let* ((message (gethash id *db-messages*))
           (people (message-people message))
           (valid-mailboxes (loop for person in people
                                  when (eql *userid* (caar person))
                                  collect person))
           (add-comment (string= (get-parameter "add-comment") "t"))
           (type (message-type message)))

      (if (eq type :transaction)
        (if valid-mailboxes
          (let* ((transaction (db id))
                 (url (strcat "/transactions/" id))
                 (person (assoc-assoc *userid* people))
                 (latest-comment (getf transaction :latest-comment))
                 (latest-seen (or (when (numberp (cdr person))
                                    (cdr person))
                                  latest-comment))
                 (on-id (getf transaction :on))
                 (on-item (db on-id))
                 (inventory-by (getf on-item :by))
                 (transaction-options)
                 (speaking-for)
                 (other-party)
                 (other-party-name)
                 (post-gratitude-p (get-parameter-string "post-gratitude"))
                 (with (remove *userid* (getf transaction :participants)))
                 (deleted-type (getf transaction :deleted-item-type))
                 (on-type (getf on-item :type)))

            (multiple-value-bind (options for)
              (transaction-options-for-user id :transaction transaction)
              (setf transaction-options options)
              (setf speaking-for for))
            (setf other-party (car (remove speaking-for with)))
            (setf other-party-name (db other-party :name))

            (prog1
              (transaction-html id
                                on-item
                                (transaction-history id
                                                     on-type
                                                     inventory-by
                                                     latest-seen
                                                     transaction)
                                (cond
                                  (post-gratitude-p
                                    (simple-gratitude-compose
                                      other-party
                                      :next url
                                      :transaction-id id
                                      :post-as speaking-for
                                      :on-id on-id
                                      :submit-name "create"
                                      :button-location :bottom))
                                  (add-comment
                                    (transaction-comment-input id))
                                  (t (transaction-buttons-html
                                       transaction-options
                                       other-party-name
                                       on-type
                                       url)))
                                :data transaction
                                :other-party-link (person-link other-party)
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
    (let ((transaction (db id)))
      (if (eq (getf transaction :type) :transaction)
        (let* ((people (getf transaction :people))
               (mailbox (assoc-assoc *userid* people))
               (party (car mailbox))
               (action-string (post-parameter-string "transaction-action"))
               (action)
               (url (strcat "/transactions/" id)))
          (setf action
                (cond
                  ((string= action-string "want")
                   :requested)
                  ((string= action-string "will-give")
                   :offered)
                  ((string= action-string "decline")
                   :declined)
                  ((string= action-string "withhold")
                   :withheld)
                  ((string= action-string "already-given")
                   :gave)
                  ((string= action-string "already-received")
                   :received)
                  ((string= action-string "dispute")
                   :disputed)))
          (if party
            (cond
              ((post-parameter "cancel")
               (see-other url))
              ((post-parameter "text")
               (flash "Your message has been sent.")
               (contact-opt-out-flash (mapcar #'caar people))
               (let* ((time (get-universal-time))
                      (new-comment-id (create-comment :on id
                                                      :text (post-parameter "text")
                                                      :time time
                                                      :by party)))
                 (send-metric* :message-sent new-comment-id))
               (see-other url))
              (action
                 (amodify-db id :log (append
                                       it
                                       (list
                                         (list :time (get-universal-time)
                                               :party party
                                               :action action))))
                 (see-other url))
              )
            (permission-denied)))

      (not-found)))))
