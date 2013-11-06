# Migration steps

(stop *acceptor*)
(save-db)
,c on (migrate-to-new-inboxes)
(ql:quickload :kindista)
(load-db)

## Data Needed

# Unread count
-  http/main.lisp for new message count
-  view selecter

# Latest message in conversation

# Latest message read by person

# Whether message has been read by person

# Mailboxes associated with a group


-  

# Message DB items need:
- participants
- mailboxes
- last-message seen for each mailbox
- people with message in:
    :inbox
    :archive
    :compost
    :deleted

New format
  :participants (ids of people and groups in conversation)
  :people ((personid . groupid) . last-read-comment)
  :folder (:inbox (people ids) :compost (people ids) :deleted (peopleids)


Comment data change:
- :by personid -> (personid .groupid)

# Inbox needs:
- list of:
    :all inbox items
    :unread inbox items
    :read status for each message
    :compost items


