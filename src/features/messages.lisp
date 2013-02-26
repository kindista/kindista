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

(defroute "/messages" ()
  (:get
    (require-user
      (standard-page
        "Messages"
        (html
          (:div :class "conversation-list"
            (:h4 :class "timestamp"
              "Yesterday at 2:30PM")
            (:div :class "reply"
              (:span :class "recipient" "↩ " (:a :href "/people/ben" "Benjamin Crandall"))
              "This is a sample reply yada")
            (:div :class "message"
              (:span :class "author" "&lt;" (:a :href "/people/ben" "Benjamin Crandall") "&gt;")
              "This is a sample message.. yada")
           
           )
          (:h2 "Notifications")
          (:ul
            (:li "separate pages for message-only inbox and notification-only inbox?"))

          (:h2 "Mail")
          
          )
        :right (html
                 (:div :class "item"
                  (:a :href "/messages/new" "compose a message")))
        :selected "mail"))))
