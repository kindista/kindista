;;; Copyright 2012-2015 CommonGoods Network, Inc.
;;;
;;; This file is part of Kindista.
;;;
;;; Kindista is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Affero General Public License as published
;;; by the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Kindista is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public
;;; License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with Kindista.  If not, see <http://www.gnu.org/licenses/>.

(in-package :kindista)

(defun not-implemented ()
  (flash "That page hasn't been implemented yet.")
  (go-home))

;; no longer compiling kindista-js.asd becase paren-files' ability to 
;; interface with asdf is currently busted
;(defun get-js ()
;  (with-output-to-string (s)
;    (paren-files:compile-script-system :kindista-js :output-stream s)))

(routes
  ("/"
    :get get-splash)

  ("/home"
    :get get-home)

  ("/<int:id>"
    :get get-item-by-id)

  ("/blog"
    :get get-blog)

  ("/blog/new"
    :get get-blog-new
    :post post-blog-new)

  ("/blog/<int:year>/<int:month>/<int:day>/<str:title>"
    :get get-blog-post)

  ("/blog/<int:id>"
    :post post-blog-post)

  ("/search"
    :get get-search)

  ("/signup"
    :get get-signup
    :post post-signup)

  ("/logout"
    :get get-logout)

  ("/login"
    :get get-login
    :post post-login)

  ("/love/<int:id>"
    :post post-love)

  ("/reset"
    :get get-reset-password
    :post post-reset-password)

  ("/invite"
    :get get-invite
    :post post-invite)

  ("/contacts"
    :get go-home
    :post post-contacts)

  ("/contact-us"
    :get go-contact-us)

  ("/image/new"
    :post post-new-image)

  ("/image/<int:id>"
    :post post-existing-image)

  ("/offers"
   :get get-offers)

  ("/offers/all"
   :get get-offers-all)

  ("/offers/new"
   :get get-offers-new
   :post post-offers-new)

  ("/offers/<int:id>"
    :get get-offer
    :post post-offer)

  ("/offers/<int:id>/reply"
    :get get-offer-reply)

  ("/requests"
   :get get-requests)

  ("/requests/all"
   :get get-requests-all)

  ("/requests/new"
   :get get-requests-new
   :post post-requests-new)

  ("/requests/<int:id>"
    :get get-request
    :post post-request)

  ("/requests/<int:id>/matchmaker"
    :post post-matchmaker)

  ("/requests/<int:id>/reply"
    :get get-request-reply)

  ("/gratitude/new"
   :get get-gratitudes-new
   :post post-gratitudes-new)

  ("/gratitude/<int:id>"
    :get get-gratitude
    :post post-gratitude)

  ("/gratitude/<int:id>/edit"
    :get get-gratitude-edit
    :post post-gratitude-edit)

  ("/gifts/<int:id>"
    :get get-gift
    :post post-gift)

  ("/events/new"
   :get get-events-new
   :post post-event)

  ("/events/<id>"
    :get get-event
    :post post-event)

  ("/events"
   :get get-events-all)

  ("/comments/<int>"
    ;:get get-comment
    :post post-comment)

  ("/comments/<int>/delete"
    :get get-comment-delete)

  ("/people"
    :get get-people)

  ("/people/contacts"
    :get get-people-contacts)

  ("/people/nearby"
    :get get-people-nearby)

  ("/people/suggested"
    :get get-people-suggested
    :post post-people-suggested)

  ("/people/invited"
    :get get-people-invited
    :post post-people-invited)

  ("/people/<id>"
    :get get-person)

  ("/people/<id>/about"
    :get get-person-about)

  ("/people/<id>/activity"
    :get get-person-activity)

  ("/people/<id>/reputation"
    :get get-person-reputation)

  ("/people/<id>/offers"
    :get get-person-offers)

  ("/people/<id>/requests"
    :get get-person-requests)

  ("/people/<id>/connections"
    :get get-person-mutual-connections)

  ("/groups"
    :get get-groups)

  ("/groups/new"
    :get get-groups-new
    :post post-groups-new)

  ("/groups/my-groups"
    :get get-my-groups)

  ("/groups/contacts"
    :get get-groups-contacts)

  ("/groups/nearby"
    :get get-groups-nearby)

  ("/groups/<id>"
    :get get-group
    :post post-existing-group)

  ("/groups/<id>/about"
    :get get-group-about)

  ("/groups/<id>/activity"
    :get get-group-activity)

  ("/groups/<id>/reputation"
    :get get-group-reputation)

  ("/groups/<id>/offers"
    :get get-group-offers)

  ("/groups/<id>/requests"
    :get get-group-requests)

  ("/groups/<id>/members"
    :get get-group-members
    :post post-group-members)

  ("/groups/<id>/invite-members"
    :get get-invite-group-members
    :post post-invite-group-members)

  ("/messages"
    :get get-messages
    :post post-messages)

  ("/settings"
    :post post-settings
    :get go-settings)

  ("/settings/personal"
    :get get-settings)

  ("/settings/public"
    :get get-settings)

  ("/settings/error"
    :get get-settings-error)

  ("/settings/verify-address"
    :get get-verify-address)

  ("/settings/location"
    :post post-settings-location)

  ("/settings/ccard"
    :post post-settings-ccard)

  ("/settings/communication"
    :get get-settings-communication)

  ("/settings/social"
    :get get-settings-social
    :post post-settings-social)

  ("/settings/notifications"
    :post post-settings-notification)

  ("/settings/admin-roles"
    :get get-settings-admin-roles)

  ("/conversations"
    :get get-conversations)

  ("/conversations/new"
    :get get-conversations-new
    :post post-conversations-new)

  ("/conversations/<int:id>"
    :get get-conversation
    :post post-conversation)

  ("/conversation/<int:id>"
    :get go-conversation)

  ("/conversations/<int:id>/leave"
    :get get-conversation-leave)

  ("/transactions"
    :get get-transactions)

  ("/transactions/new"
    :get get-transactions-new
    :post post-transactions-new)

  ("/transactions/<int:id>"
    :get get-transaction
    :post post-transaction)

  ("/messages"
    :get get-messages)

  ("/deactivate-account"
    :get confirm-deactivation)

  ("/help"
    :get go-help)

  ("/faq"
    :get get-faq)

  ("/about"
    :get get-about)

  ("/help/about"
    :get go-about)

  ("/feedback"
    :post post-feedbacks
    :get get-feedbacks)

  ("/feedback/<id>"
    :post post-feedback
    :get get-feedback)

  ("/terms"
    :get get-terms)

  ("/privacy"
    :get get-privacy)

  ("/donate"
    :get get-donate
    :post post-donate)

  ("/donate/2"
    :get get-donate-2)

  ("/donate/3"
    :get get-donate-3)

  ("/donate/4"
    :get get-donate-4)

  ("/donate/once"
    :get get-donate-once)

  ("/admin"
    :get get-admin)

  ("/admin/mail-system"
    :get get-admin-mail-system
    :post post-admin-mail-system)

  ("/admin/invite-requests"
    :get get-admin-invite-requests)

  ("/admin/matchmaker"
    :get get-admin-matchmaker)

  ("/admin/pending-accounts"
    :get get-admin-pending-accounts)

  ("/admin/pending-accounts/<id>"
    :post post-admin-pending-account)

  ("/admin/metrics"
    :get get-admin-metrics)

  ("/admin/metrics/active-accounts.png"
    :get get-admin-active-accounts-png)

  ("/admin/metrics/metrics-chart.png"
    :get get-admin-metrics-chart-png)

  ("/admin/recent"
    :get get-admin-recent)

  ("/admin/sendmail"
    :get get-admin-sendmail
    :post post-admin-sendmail)

  ("/publish-facebook"
   :post post-new-facebook-data)

  ("/uninstall-facebook"
    :post post-uninstall-facebook)

  ; redirects

  ("/people/"
    :get go-people)

  ("/home/"
    :get go-home)

  ;cron-jobs

  ("/inventory-expiration-reminders"
   :get get-inventory-expiration-reminders)

  ("/inventory-refresh"
   :get get-inventory-refresh)

  ("/schedule-metric-system-timer"
   :get get-schedule-metric-system-timer)

  ("/send-all-reminders"
   :get get-send-all-reminders)

  ("/send-inventory-digest"
   :get get-daily-inventory-digest-mailer)

  ("/automatic-invitation-reminders"
   :get get-automatic-invitation-reminders)

  ;js

 ;("/kindista.js"
 ; :get get-js)
  )

