https://developers.facebook.com/tools/debug/sharing

https://developers.facebook.com/docs/facebook-login/permissions/v2.3
https://developers.facebook.com/docs/facebook-login/manually-build-a-login-flow
https://developers.facebook.com/docs/facebook-login/manually-build-a-login-flow#logout

#Settings Page
 - List of permissions possible to select:
   - publish_actions
    - user_managed_groups, manage_pages, publish_pages (under-development)
    - read_custom_friendslist? (maybe)
    - email - "we will only contact you via your 'primary email address',
               this permission helps us ensure that your kindista account
               is associated with the correct facebook account."

#List of friends using Kindista app
https://graph.facebook.com/{user-id}/friends?fields=installed

#Offers and Requests
https://developers.facebook.com/docs/sharing/reference/share-dialog
https://developers.facebook.com/docs/sharing/opengraph/custom
https://developers.facebook.com/docs/sharing/opengraph/using-actions
[ ] when editing item (enter-inventory-item-details)  make sure :publish-facebook defaults to whether it's already been posted to FB
[ ] edit facebook story when editing inventory item
[ ] delete facebook story when deleting inventory item
[ ] if facebook token has expired/deactivatd, invite user to get a new one and let them know that if they don't, changes will not be reflected on FB

#Gratitude
  - taggable friends:
https://developers.facebook.com/docs/graph-api/reference/user/taggable_friends
https://developers.facebook.com/docs/sharing/opengraph/using-actions#tags
  - if gratitude-recipient is tagable, but user hasn't given friends-list permission
    - ask for permission
    - ask if they want to tag the friend
- make sure final piece is a post request
- modify-db with tagged-friends
- remove tagged friends from options of contatcts who can be tagged

#Group-Accounts
- permissions:
    manage_pages, publish_pages

#Invite Friends?

#FB login
https://developers.facebook.com/docs/facebook-login/multiple-providers
 - steps above "adding manual login info to a Facebook Login created account" are done
 - make sure to grab fb-profile-pic when no k-avatar
 - make sure button displays right when signing up through settings page

#Review
[ ] take screen shots
https://developers.facebook.com/docs/apps/review
[ ] submit to facebook for review

#Merge accounts (fb-id fbtoken)
- Remove admin restriction for :share-url in inventory-activity-item
- Remove *productionp* restriction from signup and login pages
- Create a tab for settings/social
- change facebook app setting/advanced deauthorize callback
- change "privacy" setting in publish-facebook-action
- remove all facebook-debubbing blocks


#Deploy on server
- load notice handlers
- (setf *facebook-app-token* (get-facebook-app-token))
