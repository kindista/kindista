https://developers.facebook.com/docs/sharing/opengraph/custom
https://developers.facebook.com/docs/sharing/opengraph/custom-open-graph-deprecation
https://developers.facebook.com/docs/sharing/web
https://developers.facebook.com/docs/plugins/share-button/
https://developers.facebook.com/tools/debug/sharing
https://developers.facebook.com/docs/facebook-login/permissions/v2.3
https://developers.facebook.com/docs/facebook-login/manually-build-a-login-flow


[ ] check v3.0 get-facebook-kindista-friends

#Migrate to generic FB sharing method
[ ] debug *fb-share-dialog-on-page-load*
[ ] trigger *fb-share-dialog-on-page-load* after posting a new item with the "share-fb" checkbox checked
[ ] review how blog is posting
[ ] replace activity-item facebook shares with facebook-share-dialog [ ] after posting to facebook, go back where you came from
[ ] remove "shared a link" from the facebook activity
[ ] is there a way to find out what was shared?
[ ] change (remove?) new-facebook-action-notice handler and any calls to it
[ ] facebook-item-meta-content
[ ] maybe change :fb-publishing-in-process parameter
[ ] remove code for posting to FB while creating offer/request/gratitude
[ ] remove facebook js and junky share-button code if not necessary
[ ] *enable-facebook-posting* t on the server

#Settings Page
 - List of permissions possible to select:
    - user_managed_groups, manage_pages, publish_pages (under-development)
    - read_custom_friendslist? (maybe)

#Offers and Requests
https://developers.facebook.com/docs/sharing/reference/share-dialog
https://developers.facebook.com/docs/sharing/opengraph/custom
https://developers.facebook.com/docs/sharing/opengraph/using-actions
[x] if facebook token has expired/deactivatd, invite user to get a new one and let them know that if they don't, changes will not be reflected on FB
[x] check fb auth when adding/modifying inventory, gratitude
[x] recieve new token/expiration and update db
[x] redirects in the fb-renew function
[x] add a flash that item has been updated/published on facebook
[x] create a notice to actually do the publishing
[x] test
[x] test that redirect urls work
[x] test "share on facebook" links under items both when fb token is inactive and active

#Gratitude
[x] taggable friends:
https://developers.facebook.com/docs/graph-api/reference/user/taggable_friends
https://developers.facebook.com/docs/sharing/opengraph/using-actions#tags
[x] test: https://kindista.org/gratitude/36509
[x] verify: https://www.facebook.com/benjamin.crandall/posts/10153497078451331:0
[x] when response is ((:SUCCESS . T)) modify-db with tagged-friends
[x] remove tagged friends from options of contatcts who can be tagged
[x] no indefinite article (og:determiner=""):
https://developers.facebook.com/docs/sharing/opengraph/object-properties

#Group-Accounts
[ ] permissions:
    manage_pages, publish_pages

#Invite Friends
https://developers.facebook.com/docs/sharing/reference/send-dialog 
[ ] create an invitation with a special token
[ ] use the token in the signup (enroll) url passed to the send-dialog
[ ] when signing a person up, if they are FB friends with the inviter, add them to kindista friends as host
[ ] invitation via gratitude

#FB login
[x] add button on home screen to get registerred with facebook
https://developers.facebook.com/docs/facebook-login/multiple-providers
[x] make sure to grab fb-profile-pic when no k-avatar (esp when user relogs through the settings page)
[x] make sure button displays right when signing up through settings page

#Merge accounts (fb-id fbtoken)
[ ] review code

#Clean-up
[ ] uncomment facebook-signup in features/login and features/signup
[ ] Remove *productionp* restriction from signup page

#Deploy on server
[ ] quickload
[ ] reload routes
[ ] redefine with-user with-token
[ ] redefine defparamater *faq-html*
[ ] load notice handlers
[ ] (setf *facebook-app-token* (get-facebook-app-token))

#Depreciate FB Open Graph Stories
