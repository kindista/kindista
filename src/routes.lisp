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

  ("/invite"
    :get get-invite
    :post post-invite)

  ("/contacts"
    :get go-home
    :post post-contacts)

  ("/resources"
   :get get-resources)

  ("/resources/all"
   :get get-resources-all)

  ("/resources/new"
   :get get-resources-new
   :post post-resources-new)
   
  ("/resources/<int:id>"
    :get get-resource
    :post post-resource)

  ("/resources/<int:id>/edit"
    :get get-resource-edit
    :post post-resource-edit)

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

  ("/requests/<int:id>/edit"
    :get get-request-edit
    :post post-request-edit)

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

  ("/people"
    :get get-people)

  ("/people/<int:id>"
    :get get-person)

  ("/people/<int:id>/about"
    :get get-person-about)

  ("/people/<int:id>/activity"
    :get get-person-activity)

  ("/people/<int:id>/reputation"
    :get get-person-reputation)

  ("/people/<int:id>/resources"
    :get get-person-resources)

  ("/discuss"
    :get get-discuss)

  ("/inbox"
    :get get-inbox)

  ("/messages"
    :get get-messages)

  ("/settings"
    :post post-settings 
    :get go-settings)

  ("/settings/personal"
    :get get-settings)

  ("/settings/verify-address"
    :get get-verify-address)

  ("/settings/communication"
    :get get-settings-communication)

  ("/about"
    :get get-about)

  ("/help"
    :get get-help)

  ("/help/faqs"
    :get get-faqs)

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

  ("/donate/once"
    :get get-donate-once)

  ("/admin"
    :get get-admin)

  ; redirects

  ("/people/"
    :get go-people) 

  ("/home/"
    :get go-home))

