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

(defroute "/about" () 
  (:get
    (with-user
      (standard-page
        "About"
        (html
          (:h1 "About Kindista")
          (:p "We are Kindista: we practice kindness.")
          (:p "We co-operate to live well, sharing and asking for what we need.
               What could better ensure our future than gratitude from our community?")
          (:p "We trust ourselves, our communities, and the world. Revealing and
               directly addressing what we need to live well, we liberate ourselves
               from the taxations of a complex and indirect social structure and become
               wealthier in our most precious resource: time. Sharing with people, we
               increase the currency of love. What could feed our future selves more
               fully than the love of our community?")
          (:p "We know that no accumulation or strategy will give us more life. Life is
               happening right now, and living simply and acting directly we experience
               every moment more fully.")

          (:p "Kindista is a social network supporting the development of community. We
               are an open source project and our code is available on " (:a :href "https://github.com/kindista/kindista" "GitHub")))
        :selected "about"
        :right (html
                 (str (donate-sidebar))
                 (str (invite-sidebar)))))))
             
