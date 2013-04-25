;;; Copyright 2012-2013 CommonGoods Network, Inc.
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

(defun get-js ()
  (setf (ps::ps-package-prefix :kindista-js) "K")
  (with-output-to-string (s)
    (paren-files:compile-script-system :kindista-js :output-stream s)))

(routes
  ("/"
    :get go-home)

  ("/home"
    :get get-home)

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

  ("/reset"
    :get get-reset-password
    :post post-reset-password)

  ("/invite"
    :get get-invite
    :post post-invite)

  ("/contacts"
    :get go-home
    :post post-contacts)

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

  ("/comments/<int>"
    ;:get get-comment
    :post post-comment)

  ("/comments/<int>/delete"
    :get get-comment-delete)

  ("/people"
    :get get-people)

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

  ("/conversations"
    :get get-conversations)

  ("/conversations/new"
    :get get-conversations-new
    :post post-conversations-new)

  ("/conversations/<int:id>"
    :get get-conversation
    :post post-conversation)

  ("/conversations/<int:id>/leave"
    :get get-conversation-leave)

  ("/messages"
    :get get-messages)

  ("/settings"
    :post post-settings
    :get go-settings)

  ("/settings/personal"
    :get get-settings)

  ("/settings/error"
    :get get-settings-error)


  ("/settings/verify-address"
    :get get-verify-address)

  ("/settings/communication"
    :get get-settings-communication)

  ("/deactivate-account"
    :get confirm-deactivation)

  ("/about"
    :get get-about)

  ("/help"
    :get go-help)

  ("/help/faqs"
    :get get-faqs)

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

  ("/admin/recent"
    :get get-admin-recent)

  ("/admin/old-inventory"
    :get get-admin-old-inventory)

  ; redirects

  ("/people/"
    :get go-people)

  ("/home/"
    :get go-home)

  ;js

  ("/kindista.js"
   :get get-js))

