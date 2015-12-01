[ ] show country for profiles in other countries
[ ] don't create new transaction when replying to same inventory item twice
    - post to the existing transaction conversation
[ ] group invite notification for new accounts
[ ] fix group notifications for new group admins
[ ] make sure blog/kindista/etc mailings don't go out to (db id :active nil) users
    [ ] if they want to be notified when gratitudes are posted to their account
        let them know that they won't be shown on their profile until they
        reactivate? (if this is so, then email should remind of this detail)
[ ] check functionality of (token-last-seen token)
[ ] top color borders for activity items
[ ] expiration/renewals for inventory items
[ ] loves
   - #links
   - post to /love
   - show list of people who love an item
[ ] split up database
   - if we create different directories for datatypes, make sure to get a comprehensive list of all datatypes in the database (including "deleted" types)
[ ] accountability with user flaky-ness
    - possible survey in transaction items to provide feedback re: experience
      with the other party
      - timeliness, enjoyment of interaction, etc.
    - ability to see constructive feedback if others can see yours
[ ] post offers/requests to facebook
[ ] notify of contact adds
[ ] enable gratitude recipients to hide gratitudes
[ ] allow people to offer or request items anonymously
[ ] better matching of search results
[ ] add gratitude option for standard invitations
[ ] email preferences for deactivated accounts
[ ] allow kindista offers tagged with "proposed-feature" to get a tab in the feedback section
    [ ] create an email that requests feedback on proposed feedback from current users with (eq (getf *user* :notify-kindista) t)
[ ] invite facebook friends
[ ] invite gmail/hotmail/yahoo contacts
[ ] flag items
[ ] reply by email (maybe)
