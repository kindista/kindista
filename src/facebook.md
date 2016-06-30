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
[ ] if facebook token has expired/deactivatd, invite user to get a new one and let them know that if they don't, changes will not be reflected on FB

#Gratitude
  - taggable friends:
https://developers.facebook.com/docs/graph-api/reference/user/taggable_friends
https://developers.facebook.com/docs/sharing/opengraph/using-actions#tags
- modify-db with tagged-friends
- remove tagged friends from options of contatcts who can be tagged
- no indefinite article (og:determiner=""):
https://developers.facebook.com/docs/sharing/opengraph/object-properties

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
