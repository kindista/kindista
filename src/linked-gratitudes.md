## When posting new gratitude
[x] select whether it's for an existing offer/request or for something not on kindista
[x] when it's for an existing item, display potential items that could be associated with the gratitude

## Pending gratitude index
- key user/group id
- value: plist (:offers (results) :requests (results))

## Tracking inventory item transaction conversations
[x] combine top 2 options when replying to an inventory item (comment or question)
[ ] change text in inbox for transaction items (is requesting/offering or has a question/comment)
[x] change replies to transactions
[x] create a transaction log property for each transaction
     - (:time time :party (personid . groupid) :action :value :gratitude gratitudeid)
[x] write a function for data migration
[ ] edit transaction inbox item, conversation view, email notification as necessary (even when no comment has occured)
[ ] what about gratitudes on inventory items w/o transactions (need a way to index for inbox)
[ ] test new email notification
[ ] choose options and language to display based on current data structure (current state of participant role)
[ ] change :log as necessary with each new comment
[ ] prompt statement of gratitude as necessary (in inbox, through email, through (see-other...) etc.
[ ] test deleting inappropriate offer/request/pending-account

## TEST before server migration

## Server migration
[ ] stop-acceptor
[ ] ,d on migrate-to-new-transaction-format
[ ] quickload
[ ] load-db
[ ] pray

