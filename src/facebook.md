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

#Privacy setting
 [ ] get and display current privacy setting
 [ ] link to facebook page to change setting:
     https://www.facebook.com/settings?tab=applications

#List of friends using Kindista app
https://graph.facebook.com/{user-id}/friends?fields=installed

#Offers and Requests
https://developers.facebook.com/docs/sharing/reference/share-dialog
https://developers.facebook.com/docs/graph-api/using-graph-api/v2.3
https://developers.facebook.com/docs/sharing/opengraph/custom
https://developers.facebook.com/docs/sharing/opengraph/using-actions
[ ] create facebook story w/ new inventory items
    - get caption to display correctly
[ ] in the inventory item, indicate whether the item has been posted to FB
[ ] when editing item (enter-inventory-item-details)  make sure :publish-facebook defaults to whether it's already been posted to FB
[ ] edit facebook story when editing inventory item
[ ] delete facebook story when deleting inventory item
[ ] if facebook token has expired/deactivatd, invite user to get a new one and let them know that if they don't, changes will not be reflected on FB

#Gratitude
  - taggable friends:
https://developers.facebook.com/docs/graph-api/reference/user/taggable_friends
https://developers.facebook.com/docs/sharing/opengraph/using-actions#tags


#Group-Accounts
- permissions:
    manage_pages, publish_pages

#Invite Friends?

#FB login
https://developers.facebook.com/docs/facebook-login/multiple-providers

signup: "sign up with facebook" "already have an account? login!"
  - if signup via facebook and user has existing Kindista-ID, log in

#FP profile pic

#Review
[ ] take screen shots
https://developers.facebook.com/docs/apps/review
[ ] submit to facebook for review

- Remove admin restriction for :share-url in inventory-activity-item

#Merge accounts (fb-id fbtoken)

