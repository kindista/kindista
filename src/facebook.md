https://developers.facebook.com/tools/debug/sharing

https://developers.facebook.com/docs/facebook-login/permissions/v2.3
https://developers.facebook.com/docs/facebook-login/manually-build-a-login-flow

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
[ ] test "share on facebook" links under items both when fb token is inactive and active

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

#FAQs
[ ] how do I share my offers/req/grat on facebook?
[ ] how do I tag my fb friends in gratitude?

#FB login
https://developers.facebook.com/docs/facebook-login/multiple-providers
[ ] steps above "adding manual login info to a Facebook Login created account" are done
[ ] make sure to grab fb-profile-pic when no k-avatar (esp when user relogs through the settings page)
[ ] make sure button displays right when signing up through settings page


#Review
[ ] take screen shots
https://developers.facebook.com/docs/apps/review
[ ] submit to facebook for review

#Merge accounts (fb-id fbtoken)
[ ] review code

#Clean-up
[ ] Remove admin restriction for :share-url in inventory-activity-item
[ ] Remove *productionp* restriction from signup and login pages
[ ] Create a tab for settings/social
[ ] change facebook app setting/advanced deauthorize callback
[ ] change "privacy" setting in publish-facebook-action
[ ] remove all facebook-debugging blocks

#Deploy on server
[ ] quickload
[ ] reload routes
[ ] redefine with-user with-token
[ ] load notice handlers
[ ] (setf *facebook-app-token* (get-facebook-app-token))
