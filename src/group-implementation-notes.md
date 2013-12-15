# To do

- prevent people from signing in as a group (group username/password)

- messages
  - change wording for gratitudes in message box

- group-type for new groups and group settings 
  - dropdown
    - community organization
    - nonprofit
    - business
    - intentional community
    - church/spiritual community
    - other ... (this.form.submit)
      - hidden field for other specificatoin

- how can members join dialog
  - anyone can ask to join; admins can invite people to join and  approve/deny membership requests
  - by invitation only

- privacy dialog for offers, requests, and gratitudes
  - new slot in results struct for privacy
  - in search, add an unless clause before calculating search rank

- email templates
  - group membership invitation
  - group membership request
  - edit conversation/reply template to reflect groups

- comments
  - as a group administrator

- debug avatars for new account (maybe just rsync with server images first)

# Testing needed

- sending gratitude/message notifications
- test on IE


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
