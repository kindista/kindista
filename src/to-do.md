## web apps
  - test iphone webapp
  - !change api key when ready to push to live
  - change manifest file when ready to push to live
  - check if account is active when sending notification
  - test messages for phones
  - inline login subscription update
  - remove facebook debugging blocks
  - javascript error handeling to lisp server
  - add sending push notification on comments
  - catch when error not registered when sending to chrome
    - decode-json-octets (first chrome-api-status))
    (a list to plist => results)
    delete registration id from user db
  - add helper function add and remove registration id from user db
  - group message not being marked as read in message folder
  # web app live server
    - test push subscriptions on different computers
## split up database
  - if we create different directories for datatypes, make sure to get a comprehensive list of all datatypes in the database (including "deleted" types)
## accountability with user flaky-ness
  - possible survey in transaction items to provide feedback re: experience
    with the other party
  - timeliness, enjoyment of interaction, etc.
  - ability to see constructive feedback if others can see yours
## post offers/requests to facebook
## notify of contact adds
## better matching of search results
## group discussions
## email preferences for deactivated accounts
  - check gratitude emails sent to people with (db id :active nil)
  - if they want to be notified when gratitudes are posted to their account
    let them know that they won't be shown on their profile until they
    reactivate? (if this is so, then email should remind of this detail)
## enable gratitude recipients to hide gratitudes
## allow people to offer or request items anonymously
## add gratitude option for standard invitations
## allow kindista offers tagged with "proposed-feature" to get a tab in the feedback section
  - create an email that requests feedback on proposed feedback from current users with (eq (getf *user* :notify-kindista) t)
## invite facebook friends
## invite gmail/hotmail/yahoo contacts
## flag items
## reply by email (maybe)
