# To do

- email templates
  - group membership invitation
  - group membership request
  - edit conversation/reply template to reflect groups
  - gratitude notification

- remove duplicates from group activity views

- update item privacies when a user joins/leaves a group


# Testing needed

- sending gratitude/message notifications
- test on IE
- test permissions when people are leave admin/member roles
- w3c validation

# Migration steps

(stop *acceptor*)
(save-db)
back-up db
,d on (migrate-to-new-inboxes)
(migrate-to-new-inboxes)
(ql:quickload :kindista)
(load-db)


## Data Needed

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
  :by personid -> (personid .groupid)

# Inbox needs:
- list of:
    :all inbox items
    :unread inbox items
    :read status for each message
    :compost items
