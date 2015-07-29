[ ] statement of gratitude by email
    [ ] determine if gratitude should be visible before recipient signs up
       - If yes or maybe...
          - if author already invited recipient, add gratitude to author's
            invitation for recipient, otherwise create a new invitation
          - create gratitude with invitation ID as recipient
          - when recipient signs up, reassign ID
       - If yes...
          - need to change profile pages, gratitude-items, and name-link to
            allow for view of a person who hasn't yet signed up
       - If no...
          - add gratitude text as property of the invitation
          - create statement of gratitude when the person signs up

    [ ] what happens when a person adds the email to an existing account?
       - features/invitations (defun add-alt-email...
    [ ] allow settings to display more than 5 aliases
    [ ] test sending multiple gratitudes to same email address
    [ ] what about posting to a deactivated account?
    [ ] include the person's name
    [ ] gratitude is "on" an invite id
    [ ] when invitee signs up, remove aliases contained within name
    [ ] test with friends and contacts
[ ] make sure blog/kindista/etc mailings don't go out to (db id :active nil) users
[ ] merge duplicate accounts
[ ] top color borders for activity items
[ ] allow kindista offers tagged with "proposed-feature" to get a tab in the feedback section
    [ ] create an email that requests feedback on proposed feedback from current users with (eq (getf *user* :notify-kindista) t)
[ ] post offers/requests to facebook
[ ] make people go to the site to read their messages/gratitudes
[ ] expiration/renewals for inventory items
[ ] invite facebook friends
[ ] loves
   - #links
   - post to /love
   - show list of people who love an item
[ ] invite gmail/hotmail/yahoo contacts
[ ] flag items
[ ] reply by email (maybe)
[ ] t-shirts
[ ] promote at unity, friends, unitarians, etc

