## web apps
 [x] hide button for anything but chrome and mobile chrome
 [x] check if the encryption keys are there before encryption
 [x] create specific push notification log file
 [x] cache busting for service worker js
 [x] write function to migrate data from registration id string to reg-id list
 [x] default message goes to message inbox
 ## on server
   [ ] look at people already subscribed to push notifications to send message
   [ ] push encryption path (encrypt.sh) in settings on live server
   [ ] migrate push registration db with helper function
   [ ] touch push-log file in data (?)
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
