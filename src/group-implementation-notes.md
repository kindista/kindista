# To do

- prevent people from signing in as a group (group username/password)

- messages
  - change wording for gratitudes in message box


# Testing needed

- sending gratitude/message notifications



# Migration steps

(stop *acceptor*)
(save-db)
,c on (migrate-to-new-inboxes)
(migrate-to-new-inboxes)
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

      ADMIN-ID = 1
      COMMENT = :<NOT-AVAILABLE>
      COMPLETED = (#S(MESSAGE :ID 10083 :LATEST-COMMENT 10084 :PEOPLE ((# . 10084)) :FOLDERS (:INBOX (1)) :TIME 3592840624 :TYPE :CONVERSATION) ..)
      FOLDERS = (:INBOX (2 253))
      #:G13 = :<NOT-AVAILABLE>
      #:G15 = :<NOT-AVAILABLE>
      #:G159 = :<NOT-AVAILABLE>
      #:G16 = :<NOT-AVAILABLE>
      #:G29 = :<NOT-AVAILABLE>
      #:G31 = :<NOT-AVAILABLE>
      #:G32 = :<NOT-AVAILABLE>
      #:G41 = :<NOT-AVAILABLE>
      #:G43 = :<NOT-AVAILABLE>
      #:G45 = :<NOT-AVAILABLE>
      #:G47 = :<NOT-AVAILABLE>
      #:G7 = NIL
      GROUPID = 2
      ID = 10052
      MAILBOXES = (((1 . 2) . 10054) ((253) . 10055))
      MESSAGE = #S(MESSAGE :ID 10052 :LATEST-COMMENT 10055 :PEOPLE (((1 . 2) . 10054) ((253) . 10055)) :FOLDERS (:INBOX (2 253)) :TIME 3592264024 :TYPE :REPLY)
      MESSAGES = (#S(MESSAGE :ID 10089 :LATEST-COMMENT 10090 :PEOPLE ((# . 10090)) :FOLDERS (:INBOX (1) :UNREAD (1)) :TIME 3592841551 :TYPE :REPLY) ..)
      #:N-LIST24 = :<NOT-AVAILABLE>
      #:N-LIST3 = (#S(MESSAGE :ID 10016 :LATEST-COMMENT NIL :PEOPLE ((# . :READ)) :FOLDERS (:INBOX (2)) :TIME 3592219415 :TYPE :GRATITUDE) ..)
      #:N-X20 = :<NOT-AVAILABLE>
      #:N-X36 = :<NOT-AVAILABLE>
      NEW-MAILBOX = (1 . 2)
      #:NEW11 = :<NOT-AVAILABLE>
      #:NEW23 = :<NOT-AVAILABLE>
      #:NEW26 = :<NOT-AVAILABLE>
      #:NEW39 = :<NOT-AVAILABLE>
      #:NEW40 = :<NOT-AVAILABLE>
      #:NEW42 = :<NOT-AVAILABLE>
      #:NEW44 = :<NOT-AVAILABLE>
      #:NEW46 = :<NOT-AVAILABLE>
      #:NEW9 = :<NOT-AVAILABLE>
      PEOPLE = (1 253)
      #:TAIL0 = (:UNREAD NIL)
