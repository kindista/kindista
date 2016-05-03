;;; Copyright 2012-2016 CommonGoods Network, Inc.
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

(defun new-transaction-action-notice-handler ()
  (let* ((log-event (getf (cddddr *notice*) :log-event))
         (text (getf (cddddr *notice*) :text)))
    (send-transaction-action-notifications (getf (cddddr *notice*)
                                                      :transaction-id)
                                                log-event
                                                text)))

(defun create-transaction (&key on text action match-id pending-deletion (userid *userid*))
  (let* ((time (get-universal-time))
         (on-item (db on))
         (by (getf on-item :by))
         (item-violates-terms-p (and (getf *user* :admin) pending-deletion))
         (participants (list (if item-violates-terms-p +kindista-id+ userid) by))
         (senders (if item-violates-terms-p
                    (mailbox-ids (list +kindista-id+))
                    (mailbox-ids (list userid))))
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
         (id (insert-db (if item-violates-terms-p
                          (list :type :transaction
                                :on on
                                :deleted-item-details (getf on-item :details)
                                :deleted-item-title (getf on-item :title)
                                :deleted-item-type (getf on-item :type)
                                :by +kindista-id+
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
                               :by (if item-violates-terms-p
                                     (cons userid +kindista-id+)
                                     (list userid))
                               :text text
                               :send-email-p nil
                               :time (+ time 1) ; if there is both text/action, they need separate times for sorting in transaction log UI display
                               ))
    (when match-id
      (case (getf on-item :type)
        (:offer (hide-matching-offer match-id on))
        (:request (hide-matching-offer on match-id))))

    (notice :new-transaction-action :time time
                                    :transaction-id id
                                    :log-event (car log)
                                    :text text)
    id))

(defun transactions-pending-gratitude-for-account (account-id)
  (let* ((all-pending (gethash account-id *pending-gratitude-index*)))
    (mapcar #'cdr
            (append (getf all-pending :offers)
                    (getf all-pending :requests)))))

(defun transactions-pending-gratitude-for-user
  (&optional (userid *userid*)
   &aux (groups (mapcar #'car (groups-with-user-as-admin userid)))
        (transactions))
  (dolist (account (cons userid groups))
    (asetf transactions
           (append (transactions-pending-gratitude-for-account account)
                   it)))
  transactions)

(defun transaction-pending-gratitude-p
  (transaction-id &optional (data (db transaction-id))
                  &aux (pending-gratitude-p))

  (loop for event in (getf data :log)
        when (eq (getf event :action) :gratitude-posted)
        do (progn (setf pending-gratitude-p nil)
                  (loop-finish))
        when (or (eq (getf event :action) :withheld)
                 (eq (getf event :action) :disputed)
                 (eq (getf event :action) :decline))
        do (progn (setf pending-gratitude-p nil)
                  (loop-finish))
        when (or (eq (getf event :action) :gave)
                 (eq (getf event :action) :received))
        do (progn (setf pending-gratitude-p t)
                  (loop-finish)))
  pending-gratitude-p)

(defun sitewide-transaction-gratitude (&aux (completed 0) (pending 0))
  (setf completed (length *completed-transactions-index*))

  (flet ((count-pending (account-id pending-plist)
           (declare (ignore account-id))
           (asetf pending
                  (+ it
                     (length (append (getf pending-plist :offers)
                                     (getf pending-plist :requests)))))))
    (maphash #'count-pending *pending-gratitude-index*))

  (list :completed completed :pending pending))

(defun index-transaction (id data)
  (with-locked-hash-table (*inventory-transactions-index*)
    (pushnew id (gethash (getf data :on) *inventory-transactions-index*)))
  (index-message id data) )

(defun index-pending-transactions ()
"Populates *pending-gratitude-index*. Must be run after indexing all results since no guarantee can be made that results are created in numeric order."
  (flet ((index-transaction (transaction-id message)
           (when (eq (message-type message) :transaction)
             (let ((transaction (db transaction-id)))
               (when (transaction-pending-gratitude-p
                       transaction-id
                       transaction)
                 (let* ((item-id (getf transaction :on))
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
                           (push (cons result transaction-id)
                                 (getf (gethash (getf transaction :by) *pending-gratitude-index*)
                                       :offers)))
                         (:request
                           (push (cons result transaction-id)
                                 (getf (gethash by *pending-gratitude-index*)
                                       :requests))))))))))))
    (with-locked-hash-table (*pending-gratitude-index*)
      (maphash #'index-transaction *db-messages*))))

(defun transaction-history
  (transaction-id
   inventory-item-id
   latest-seen
   &key (transaction (db transaction-id))
        (inventory-item (db inventory-item-id))
   &aux (actions (getf transaction :log))
        (on-type (getf inventory-item :type))
        (on-type-string (case on-type
                          (:offer "offer")
                          (:request "request")))
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
           (text (if (and (equal comment-id (car (last comments)))
                          (getf transaction :deleted-item-type))
                   (deleted-invalid-item-reply-text
                     (db (car (remove +kindista-id+ participants)) :name)
                     (getf bydata :name)
                     (case (getf transaction :deleted-item-type)
                       (:offer "offer")
                       (:request "request"))
                     (when (scan +text-scanner+ (getf data :text))
                       (getf data :text)))
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
    (:h2 "History:")
    (:div :id "transaction-history"
      (dolist (event history)
        (acond
         ((getf event :action)
          (str (transaction-action-html
                 event
                 transaction
                 inventory-item
                 (strcat* "this " on-type-string))))

         ((getf event :text)
          (str (conversation-comment-html
                  (getf event :data)
                  (getf event :by)
                  (if (eq (getf event :by) *userid*)
                    "I"
                    (getf event :by-name))
                  (getf event :for)
                  (getf event :for-name)
                  (getf event :text)
                  (when (>= (or latest-seen 0)
                            (getf event :id)))
                  :id (getf event :id)
                  :transaction-p t))))))))

(defun transaction-other-party
  (transaction-id
   &optional (userid *userid*)
             (transaction (db transaction-id))
   &aux (initiator (getf transaction :by)))
  (if (or (eql initiator userid)
          (group-admin-p initiator userid))
    (db (getf transaction :on) :by)

    initiator))

(defun transaction-action-text
  (log-event
   transaction
   inventory-item
   &key inventory-descriptor
        (inventory-by-id (getf inventory-item :by))
        (inventory-by-name (db inventory-by-id :name))
        (transaction-by-id (getf transaction :by))
        (transaction-by-name (db transaction-by-id :name))
        ;; basic-action means to show the group as the actor
        ;; and not include a link for the actor
        basic-action
        (punctuation ".")
        (userid *userid*)
        name-links-p
   &aux (on-type (getf inventory-item :type))
        (action-party-id (car (getf log-event :party)))
        (action-party-group-id (cdr (getf log-event :party)))
        (action-party-group (db action-party-group-id))
        (action-party (db action-party-id))
        (self (eql action-party-id userid)) )

  (flet ((indirect-object (initiator)
           (case initiator
             (:transaction
               (if (eql transaction-by-id userid)
                 "you"
                 transaction-by-name))
             (:inventory
               (if (eql inventory-by-id userid)
                 "you"
                 inventory-by-name)))))
  (strcat*
    (cond
      (self "I")
      ((and name-links-p (not basic-action))
       (person-link action-party-id))
      ((and basic-action action-party-group)
       (getf action-party-group :name))
      (t (getf action-party :name)))

    (when (and action-party-group
               (not basic-action))
      (strcat* " (on behalf of "
               (getf action-party-group :name)
               ")" ))
    (case on-type
      (:offer
        (case (getf log-event :action)
          (:requested
            (strcat " requested to receive "
                inventory-descriptor
                " from "
                (indirect-object :inventory)))
          (:offered
            (strcat " agreed to share " inventory-descriptor " with "
                (indirect-object :transaction)))
          (:declined
            (strcat* " declined to receive a gift from "
                     (indirect-object :inventory)))
          (:withheld
            (strcat* " indicated that "
                     (if self "you" "they")
                     " can no longer share "
                     inventory-descriptor
                     " with "
                     (indirect-object :transaction)))
          (:gave
            (strcat " shared " inventory-descriptor " with "
                (indirect-object :transaction)))
          (:received
            (strcat " received a gift from "
                (indirect-object :inventory)))
          (:disputed
            (strcat " disputed having received a gift from "
                    (indirect-object :inventory)))
          (:deactivated
            (if action-party-id
              (strcat " deactivated " inventory-descriptor)
              (strcat "This "
                      (string-downcase (symbol-name on-type))
                      " has been deactivated")))
          (:gratitude-posted
            (strcat " posted a statement of gratitude about "
                    inventory-by-name
                    " for a gift "
                    (if self "you" "they")
                    " received from "
                    (if self "them" "you")))))
      (:request
        (case (getf log-event :action)
          (:requested
            (strcat* " want" (unless self "s")
                     " to receive what "
                     (indirect-object :transaction)
                     (if (string= (indirect-object :transaction) "you")
                       " are" " is")
                     " offering") )
          (:offered
            (strcat " agreed to fulfill " inventory-descriptor " made by "
                (indirect-object :inventory)))
          (:declined
            (strcat* " declined to receive a gift from "
                     (indirect-object :transaction)))
          (:withheld
            (strcat* " indicated that "

                     (if self "you" "they")
                     " can no longer share "
                     inventory-descriptor
                     " with "
                (indirect-object :inventory)))
          (:gave
            (strcat " fulfilled " inventory-descriptor " from "
                (indirect-object :inventory)))
          (:received
            (strcat " received a gift from "
                (indirect-object :transaction)))
          (:disputed
            (strcat " disputed having received a gift from "
                (indirect-object :transaction)))
          (:deactivated
            (if action-party-id
              (strcat " deactivated " inventory-descriptor)
              (strcat "This "
                      (string-downcase (symbol-name on-type))
                      " has been deactivated")))
          (:gratitude-posted
            (strcat " posted a statement of gratitude about "
                    (indirect-object :transaction)
                    " for "
                    inventory-descriptor)))))
    punctuation)))

(defun transaction-action-html
  (log-event
   transaction
   inventory-item
   inventory-descriptor
   )

  (case (getf log-event :action)
    (:gratitude-posted
      (gratitude-activity-item (gethash (getf log-event :comment) *db-results*)
                               :show-on-item nil))
    (t
      (card nil
        (html
          (str (h3-timestamp (getf log-event :time)))
          (:p
            (:strong
              (str (transaction-action-text
                     log-event
                     transaction
                     inventory-item
                     :inventory-descriptor inventory-descriptor)))))))))

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
   on-url
   url
   &key (comment-p t)
        representing-group-p
        violates-terms
   &aux (subject (if representing-group-p "We" "I")))
  (html
    (:h2 "Options:")
    (:div :class "transaction-options"

      (when (and (find "post-gratitude" transaction-options :test #'string=)
                 (not violates-terms))
        (htm
          (:div :class "transaction-option"
            (:a :href (url-compose url "post-gratitude" "t")
              (:div (str (icon "heart-person")))
              (:div (str (s+ subject " have gratitude to share about "
                             other-party-name
                             " for this gift")))))))

      (flet ((transaction-button
               (status icon request-text offer-text &optional (value status) (name "transaction-action"))
               (when (find status transaction-options :test #'string=)
                 (html
                   (:div :class "transaction-option"
                     (:button :type "submit"
                      :class "simple-link"
                      :name name
                      :value value
                      (:div (str icon))
                      (:div (str (if (eq on-type :request)
                                     request-text offer-text)))))))))

        (unless violates-terms
          (htm
            (:form :method "post" :action url

              (str (transaction-button
                     "will-give"
                     (icon "offers")
                     (s+ subject " want to fulfill this request")
                     (s+ subject " want to share this")))

              (str (transaction-button
                     "already-given"
                     (icon "gift")
                     (s+ subject " have fulfilled this request for " other-party-name)
                     (s+ subject " have shared this with " other-party-name)))

              (str (transaction-button
                     "want"
                     (icon "requests")
                     (s+ subject " want what " other-party-name " is offering")
                     (s+ subject " want to receive this")))

              (str (transaction-button
                     "already-received"
                     (icon "gift")
                     (s+ subject " have received this from " other-party-name)
                     (s+ subject " have received this from " other-party-name)))

              (str (transaction-button
                     "withhold"
                     (icon "withhold")
                     (s+ subject " can't fulfill this request now")
                     (s+ subject " can't share this now")))

              (str (transaction-button
                     "decline"
                     (icon "decline")
                     (s+ subject " don't want what " other-party-name " is offering")
                     (s+ subject " no longer want this")))

              (str (transaction-button
                     "dispute"
                     (icon "caution")
                     (s+ subject " have <strong>not</strong> yet received this")
                     (s+ subject " have <strong>not</strong> yet received this"))))))

        (when (and (find "deactivate" transaction-options :test #'string=)
                   (not violates-terms))
          (htm
            (:form :method "post" :action on-url
             (:input :type "hidden" :name "next" :value url)
             (str (transaction-button
                    "deactivate"
                    (icon (if (eql on-type :offer)
                            "empty-giving-hand"
                            "empty-receiving-hand"))
                    (s+ subject " no longer request this.  Please deactivate it.")
                    (s+ subject " no longer offer this.  Please deactivate it.")
                    t
                    "deactivate"))))))

       (when comment-p
         (htm (:div :class "transaction-option"
                 (:a :href (url-compose url "add-comment" "t")
                   (:div (str (icon "comment")))
                   (:div (str (s+ "Reply to "
                                  other-party-name
                                  " about this "
                                  (string-downcase (aif on-type
                                                     (symbol-name it)
                                                     "item"))))))))))))

(defun transaction-html
  (transaction-id
   on-item
   role
   other-party-id
   history-html
   form-elements-html
   &key (data (db transaction-id))
        deleted-type
   &aux (on-type (getf on-item :type))
        (other-party (db other-party-id))
        (inventory-url (case on-type
                         (:offer (strcat "/offers/" (getf data :on)))
                         (:request (strcat "/requests/" (getf data :on)))))
        (inventory-by-id (getf on-item :by))
        (inventory-by (db inventory-by-id))
        (inventory-by-link (case (getf inventory-by :type)
                             (:person (person-link inventory-by-id))
                             (:group (group-link inventory-by-id))))
        (inventory-description
          (case on-type
            ((or :offer :request)
             (string-downcase (symbol-name (getf on-item :type))))
            (t (case deleted-type
                 (:offer "offer")
                 (:request "request")
                 (t (html
                      (:span :class "none" "deleted offer or request")))))))
        (offer-p (eql on-type :offer))
        (most-recent-log-event (first (getf data :log)))
        (status (getf most-recent-log-event :action)))

  (standard-page
    "Transaction"
    (html
      (when (getf on-item :violates-terms)
        (flash (s+ "This "
                   (string-downcase (symbol-name (getf on-item :type)))
                   " violated Kindista's Terms of Use and has been deactivated.")
               :error t))
      (:h2 "A "
           (str (if status "transaction" "conversation"))
           " with "
           (str (case (getf other-party :type)
                  (:group (group-link other-party-id))
                  (:person (person-link other-party-id))))
           " regarding "
           (str (cond
                  ((eql (getf on-item :by) *userid*)
                   "your ")
                  (inventory-by
                    (strcat* inventory-by-link "'s "))
                  ;; for old inventory items that got deleted before we switched to
                  ;; deactivating them instead
                  (t "a ")))
           (str inventory-description)
           ;; some old ones may have been deleted
           (when on-item (htm ":")))

     (when on-item
       (htm
         (:blockquote
           (:strong
             (:a :href inventory-url
                 (str (ellipsis (or (getf on-item :title)
                                    (getf on-item :details)))))))))

     (when  (and (or (eq status :offered)
                     (eq status :requested)
                     (eq status :gave)
                     (eq status :received)
                     (eq status :disputed)
                     (eq status :gratitude-posted))
                 (not (getf on-item :violates-terms)))

       (flet ((find-status (status) (find status (getf data :log)
                                          :key #'(lambda (event)
                                                   (getf event :action)))))
         (htm
           (:h2 "Progress:")
           (:table :class "transaction-progress"
             (:tr :class "steps"
               (:td :class "done"
                 (:div "1. ")
                 (:div (str (if offer-p "Requested" "Offered"))))
               (:td :class (when (or (and offer-p (find-status :offered))
                                     (and (not offer-p)
                                          (find-status :requested))
                                     (eq status :gave)
                                     (eq status :received)
                                     (eq status :gratitude-posted))
                             "done")
                 (:div "2. ")
                 (:div (str (if offer-p "Committed" "Accepted"))))
               (:td :class (when (or (eq status :gratitude-posted)
                                     (eq status :gave)
                                     (eq status :received))
                             "done")
                 (:div "3. ")
                 (:div (str (case role (:giver "Given") (:receiver "Received")))))
               (:td :class (when (eq status :gratitude-posted)
                             "done")
                 (:div "4. ")
                 (:div :class "gratitude-step" "Gratitude Posted " (when (eq status :gratitude-posted) (str (icon "white-checkmark"))))))))))

     (when status
       (htm (:h2 "Status:")
            (case (getf most-recent-log-event :action)
              (:gratitude-posted
                (str (gratitude-activity-item
                       (gethash (getf most-recent-log-event :comment)
                                *db-results*)
                       :show-when nil
                       :show-actions nil
                       :show-on-item nil)))
              (t
                (htm
                  (:strong
                    (str (transaction-action-text
                           most-recent-log-event
                           data
                           on-item
                           :inventory-descriptor
                             (s+ " this " inventory-description)))))))))

      (str form-elements-html)
      (str history-html))

    :selected "messages"
    :class "transaction"
    ))

(defun fix-transaction-logs (&aux (count 0) example)
  (dolist (id (hash-table-keys *db*))
    (let* ((data (db id))
           (type (getf data :type))
           (log (getf data :log)))
      (when (and (eql type :transaction)
                 log
                 (not (apply #'>= (mapcar (lambda (event) (getf event :time))
                                          log))))
        (modify-db id :log (sort log
                                 #'>
                                 :key (lambda (event) (getf event :time))))
        (incf count)
        (setf example id)
        )))
  (values count example)
  )

(defun transaction-options-for-user
  (transaction-id
   &key (userid *userid*)
        (transaction (db transaction-id))
   &aux (transaction-mailboxes (mapcar #'car (getf transaction :people)))
        ;; some transactions in the database ordered logs because of a change
        ;; introduced in commit: 9f6c08e89768862774b8dc5ba79f8af52189f468
        ;; until that is fixed, we need to sort the log here
        (log (sort (getf transaction :log)
                   #'>
                   :key #'(lambda (event) (getf event :time))))
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
        (gratitude-expressed-p (find :gratitude-posted
                                     log
                                     :key #'(lambda (event)
                                              (getf event :action))
                                     :from-end t))
        (current-event (find representing
                             log
                             :test #'eql
                             :key #'(lambda (event)
                                       (if (eql representing userid)
                                         (car (getf event :party))
                                         (cdr (getf event :party))))))
        (other-party-event (find-if-not
                             #'(lambda (event)
                                 (if (eql representing userid)
                                   (eql userid (car (getf event :party)))
                                   (eql representing (cdr (getf event :party)))))
                             log))
        (options ()))

  "Returns (1) a list of actions the user can take on a given transaction id and (2) the entity the user is representing (i.e. *userid* or a groupid)"

  (setf options
        (case role
          (:receiver
             (case (getf current-event :action)
               (:requested
                 (case (getf other-party-event :action)
                   (:gave '("post-gratitude" "dispute"))
                   (t '("already-received" "decline"))))
               (:declined '("want" "already-received"))
               (:disputed '("already-received"))
               (:received '("post-gratitude"))
               (t (cond
                    (gratitude-expressed-p nil)
                    ((eq (getf other-party-event :action) :gave)
                     '("already-received" "dispute"))
                    (t '("want" "already-received"))))))
          (:giver
            (unless gratitude-expressed-p
              (case (getf current-event :action)
                (:offered
                  (unless (eq (getf other-party-event :action) :received)
                    '("already-given" "withhold")))
                (:withheld '("will-give"))
                (t (unless (eq (getf current-event :action) :gave)
                     '("will-give" "already-given"))))))))

  (when (and inventory-item inventory-by-self-p)
    (case role
      (:giver
        (if (getf inventory-item :active)
          (when (not (find "withhold" options :test #'string=))
            (push "deactivate" options))
          (progn (push "reactivate" options)
                 (remove "will-give" options :test #'string=))))
      (:receiver
        (if (getf inventory-item :active)
          (unless (or (find "decline" options :test #'string=)
                      (and (eql (getf other-party-event :action)
                                 :gave)
                           (find transaction-id
                                 (getf (gethash representing
                                                *pending-gratitude-index*)
                                       (if (eq inventory-type :request)
                                         :requests
                                         :offers))
                                 :key #'cdr)))
            (push "deactivate" options))
          (progn (push "reactivate" options)
                 (remove "want" options :test #'string=))))))

  (when gratitude-expressed-p
    (asetf options (remove "want"
                     (remove "will-give" it :test #'string=) :test #'string=)))

  (values options
          representing
          role
          current-event
          other-party-event))

(defun get-transaction (id)
"when called, (modify-db conversation-id :people '((userid . this-comment-id) (other-user-id . whatever)))"
  (require-user ()
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
                 (transaction-options)
                 (speaking-for)
                 (other-party)
                 (other-party-name)
                 (user-role)
                 (enable-comment-option-p t)
                 (post-gratitude-p (get-parameter-string "post-gratitude"))
                 (gratitude-posted-p (find :gratitude-posted
                                           (getf transaction :log)
                                           :key #'(lambda (event)
                                                    (getf event :action))))
                 (with (remove *userid* (getf transaction :participants)))
                 (deleted-type (getf transaction :deleted-item-type))
                 (on-type (getf on-item :type))
                 (on-type-string (case on-type
                           (:offer "offer")
                           (:request "request")))
                 (on-url (strcat "/" on-type-string "s/" on-id)))

            (multiple-value-bind
              (options for role current-event other-party-event)
              (transaction-options-for-user id :transaction transaction)
              (setf transaction-options options)
              (setf speaking-for for)
              (setf user-role role)
              (setf other-party (car (remove speaking-for with)))
              (setf other-party-name (db other-party :name))
              (when (and (eq user-role :receiver)
                         (eq (getf current-event :action) :received))
                (setf enable-comment-option-p nil))
              (when (and
                      (eq user-role :receiver)
                      (not gratitude-posted-p)
                      (or (eq (getf current-event :action) :received)
                          (and (eq (getf other-party-event :action) :gave)
                               (not (eq (getf current-event :action) :disputed)))))
                (flash (strcat "Please post a "
                               "<a href=\"/transactions/"
                               id
                               "?post-gratitude=t\">"

                               "statement of gratitude"
                               "</a>"
                               " about "
                               other-party-name
                                " (below)."))))

            (progn
              ; get most recent comment seen
              ; get comments for
              (when (or (not (eql (message-latest-comment message)
                                  (cdr (assoc-assoc *userid*
                                                    (message-people message)))))
                        (member *userid*
                                (getf (message-folders message) :unread)))

                (update-folder-data message :read :last-read-comment (message-latest-comment message)))

              (transaction-html
                id
                on-item
                user-role
                other-party
                (transaction-history id
                                     on-id
                                     latest-seen)
                (cond
                  (post-gratitude-p
                    (simple-gratitude-compose
                      other-party
                      :next url
                      :transaction-id id
                      :post-as speaking-for
                      :on-id on-id
                      :submit-name "create"
                      :autofocus-p t
                      :cancel-button t))
                  (add-comment
                    (transaction-comment-input id))
                  (t (transaction-buttons-html
                       transaction-options
                       other-party-name
                       on-type
                       on-url
                       url
                       :representing-group-p (eq (db speaking-for :type)
                                                 :group)
                       :violates-terms (getf on-item :violates-terms)
                       :comment-p enable-comment-option-p)))
                :data transaction
                :deleted-type deleted-type)))

          (permission-denied))
      (not-found)))))

(defun modify-transaction-log
  (transaction-id
   new-action
   &key (transaction (db transaction-id))
        party
        (message (gethash transaction-id *db-messages*))
   &aux (time (get-universal-time))
        (people (getf transaction :people))
        (mailbox (assoc-assoc *userid* people))
        (party (unless party (car mailbox)))
        (people-list (all-message-people message))
        (folders (list :inbox people-list
                       :unread (remove *userid*
                                       people-list)
                       :compost nil
                       :deleted nil))
        (log-event (list :time time
                         :party party
                         :action new-action)))
  (index-message
    transaction-id
    (if (eql new-action :deactivated)
      (amodify-db transaction-id :log (cons log-event it))
      (amodify-db transaction-id :message-folders folders
                                 :log (cons log-event it)))))

(defun post-transaction (id)
  (require-active-user
    (setf id (safe-parse-integer id))
    (let ((transaction (db id)))
      (if (eq (getf transaction :type) :transaction)
        (let* ((people (getf transaction :people))
               (message (gethash id *db-messages*))
               (message-text (or (post-parameter-string "reply-text")
                                 (post-parameter-string "text")))
               (mailbox (assoc-assoc *userid* people))
               (party (car mailbox))
               (participants (getf transaction :participants))
               (action-string (post-parameter-string "transaction-action"))
               (action)
               (other-party-name)
               (inventory-item (db (getf transaction :on)))
               (url (strcat "/transactions/" id)))
          (setf action
                (cond
                  ((string= action-string "want")
                   :requested)
                  ((string= action-string "request")
                   :requested)
                  ((string= action-string "will-give")
                   :offered)
                  ((string= action-string "offer")
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
            (flet ((modify-log (new-action)
                     (let* ((time (get-universal-time))
                             (log-event (list :time time
                                              :party party
                                              :action new-action)))
                       (modify-transaction-log id
                                               new-action
                                               :transaction transaction
                                               :message message)

                       (unless (or (eq action :received) message-text)
                         (flash "Your action has been recorded and the other party will be notified.")
                         (contact-opt-out-flash participants
                                                :item-type "transaction"))

                       (unless (eql action :received)
                         (notice :new-transaction-action :time time
                                 :transaction-id id
                                 :log-event log-event))
                       (see-other url))))

              (setf other-party-name (db (transaction-other-party id) :name))

              (cond
                ((post-parameter "cancel")
                 (see-other url))

                (message-text
                 (let* ((time (get-universal-time))
                        (new-comment-id (create-comment :on id
                                                        :text message-text
                                                        :time time
                                                        :by party)))
                   (send-metric* :message-sent new-comment-id))
                 (awhen action (modify-log it))
                 (flash "Your message has been sent.")
                 (contact-opt-out-flash participants)
                 (see-other url))

                ((eq action :declined)
                 (confirm-action
                   "Decline Gift"
                   (strcat "Please confirm that you no longer wish to receive this gift from "
                           (case (getf inventory-item :type)
                             (:offer (db (getf inventory-item :by) :name))
                             (:request (db (getf transaction :by) :name)))
                           ":")
                   :url url
                   :next-url url
                   :details (or (getf inventory-item :title)
                                (getf inventory-item :details))
                   :post-parameter "confirm-decline"))

                ((post-parameter "confirm-decline")
                  (modify-log :declined))

                ((eq action :withheld)
                 (confirm-action
                   "Cancel Transaction"
                   (strcat "Please confirm that you no longer intend to give this gift to "
                           (case (getf inventory-item :type)
                             (:request (db (getf inventory-item :by) :name))
                             (:offer (db (getf transaction :by) :name)))
                           ":")
                   :class "cancel-transaction"
                   :url url
                   :next-url url
                   :details (or (getf inventory-item :title)
                                (getf inventory-item :details))
                   :post-parameter "confirm-withhold"))

                ((post-parameter "confirm-withhold")
                  (modify-log :withheld))

                ((eql action :gave)
                 (confirm-action
                   "Confirm your gift"
                   (s+ "Please confirm that you have given this to "
                       other-party-name
                       ":")
                   :url url
                   :next-url url
                   :details (html
                              (:blockquote
                                (str
                                  (ellipsis
                                    (or (getf inventory-item :title)
                                        (getf inventory-item :details))))))
                   :fine-print (s+ "Important: This action cannot be undone. "
                                   "Click \"Yes\" only after you have given this to "
                                   other-party-name
                                   ".")
                   :class "confirm-transaction"
                   :post-parameter "confirm-given"
                   :button-text "Yes, I have given this"))

                ((post-parameter "confirm-given")
                 (modify-log :gave))

                ((eql action :received)
                 (confirm-action
                   "Confirm your gift"
                   (s+ "Please confirm that you have received this from "
                       other-party-name
                       ":")
                   :url url
                   :next-url url
                   :details (html
                              (:blockquote
                                (str
                                  (ellipsis
                                    (or (getf inventory-item :title)
                                        (getf inventory-item :details))))))
                   :fine-print (s+ "Important: This action cannot be undone. "
                                   "Click \"Yes\" only after you have received this from "
                                   other-party-name
                                   ".")
                   :class "confirm-transaction"
                   :post-parameter "confirm-received"
                   :button-text "Yes, I have received this"))

                ((post-parameter "confirm-received")
                 (setf action :received)
                 (modify-log :received))

                (action
                  (modify-log action))))

            (permission-denied)))

      (not-found)))))
