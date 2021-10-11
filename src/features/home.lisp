;;; Copyright 2012-2021 CommonGoods Network, Inc.
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

(defun go-home ()
  (see-other "/home"))

(defun home-rightbar ()
  (html
    #|
    (:div :style "padding-bottom: 1em;
                  border-bottom: 1px solid #ddd;
                  margin-bottom: 1em;"
      (:div :style "font-size: 2.5em; font-weight: bold" "2,000")
      (:div :style "font-size: 1em; font-weight: bold" "monthly donations")
      (:div :style "font-size: 2.5em; font-weight: bold" "$999,999")
      (:div :style "font-size: 1em; font-weight: bold" "per month of $99,999,999 " (:a :href "/donate" "goal"))
      (:button :style "font-size: 1.5em;
                       font-weight: bold;
                       background: #375497;
                       color: #fff;
                       border: 0;
                       border-radius: 4px;
                       margin-top: 0.5em;
                       padding: 0.25em 0.6em" "Donate to Kindista")
      (:div :style "font-size: 0.9em; margin-top: 1em;" (:a :href "#" "How does Kindista use the money?"))
      )
      |#

    (str (facebook-signup-sidebar))

    (str (donate-sidebar))

    (str (invite-sidebar :mobile t))

    (str (suggestion-sidebar))

    (str (events-sidebar))))

(defun standard-home ()
  (standard-page
    "Home"
    (html
      (:div :class "activity"
        (str (buttons-horiz
               '("/offers/new" "offer-button" "Post an Offer")
               '("/requests/new" "request-button" "Make a Request")
               '("/gratitude/new" "heart-person"  "Express Gratitude")
               ;(:a :href "/announcements/new" "post announcement")
                          ))

      (let ((page (if (scan +number-scanner+ (get-parameter "p"))
                   (parse-integer (get-parameter "p"))
                   0)))
        (when *user*
          (unless (> page 0)
            ;; get one of the user's matching offers at random
            ;; note:  we used to get either matching offers or requests
            ;;        now we only present matching offers and leave it to the 
            ;;        potential recipient to contact the potential giver
            (let ((random-item (rand-from-list
                                 (second (assoc :offers
                                                (matching-inventory-items-by-user))))))
              (awhen random-item
                (htm
                  (:div :class "suggested-items card"
                    (str (featured-offer-match-html (getf it :offer)
                                                    (getf it :request)
                                                    :featured t))
                    (str (featured-request-match-html (getf it :request)
                                                      :featured t)))))))

          (str (distance-selection-html "/home"
                                        :text "show activity within "
                                        :class "item")))
        (with-location
          (str (local-activity-items :page page))))))
    :class "home"
    :selected "home"
    :top (cond
           ((not *user*)
            (welcome-bar
              (html
                (:h2 "Kindista Demo")
                (:p "This is what Kindista looks like to someone living in Eugene, Oregon. "
                    "If you were logged in now, you'd see what's going on around you. "
                    "Use the menu "
                    (:span :class "menu-button" " (click the button on the header) ")
                    (:span :class "menu-showing" " on the left ")
                    " to explore the site.")
                (:p "Because we don't have the resources to fight spam, "
                 "we prefer that you sign up using an invitation from an "
                 "existing Kindista member. "
                 "If you don't know anyone currently on Kindista, go ahead and "
                 (:a :href "/signup" (:strong "sign up"))
                 ". "
                 "Please be aware that some features may not be available to "
                 "you until you post your first offer to show us that "
                 "you understand  our "
                 (:a :href "/terms" "Terms of Use")
                 " and that you intend to be a contributing member "
                 "of our community."))
              nil))
           ((getf *user* :help)
            (welcome-bar
              (html
                (:h2 "Getting started")
                (:p "We're so happy to have you join us! Here are some things you can do to get started:")
                (:ul
                  (unless (getf *user* :avatar)
                    (htm (:li (:a :href "/settings/personal" "Upload a picture") " so that other people can recognize you.")))
                  (:li (:a :href "/gratitude/new" "Express gratitude") " for someone who has affected your life.")
                  (:li (:a :href "/people" "Make a connection") " to say that you know someone.")
                  (if (eql (getf *user* :host) +kindista-id+)
                    (htm (:li (:a :href "/offers/new" "Post an offer") " to contribute to the community.")) 
                    (htm(:li (:a :href "/requests/new" "Post a request") " to the community for something you need.")))
                  )
                (:p "On this page you can see what's going on around you and with people you have made
                     a connection with. Use the menu "
                    (:span :class "menu-button" " (click the button on the header) ")
                    (:span :class "menu-showing" " on the left ")
                    " to explore the site.")))))
  :selected "home"
  :right (home-rightbar))) 
(defun newuser-home ()
  (header-page
    "Welcome"
    nil
    (html
      (:div :id "body"
        (:div :class "setup"
          (if (getf *user* :pending)
            (htm 
              (:h2 "Complete your account")
              (:h3 "Step 3 of 3: Enter your location"))
            (htm (:h2 "Welcome to Kindista!")))
          (:p "Kindista is a social network for " (:strong "building and supporting real community") ".
               We use your location to help you find " (:strong "local people, offers, and events") ".
               To get started, we need to know where you call home.")
          (:p "We will never share your exact location with anyone else. If you would like to know more about how we use the information you share with us,
               please read our " (:a :href "/privacy" "privacy policy") ".")
          (:h2 "Where do you call home?")
          (:em "If you are travelling or do not have a permanent address, you may enter the city or location where you will be sleeping tonight. You can change your address at any time on the settings page.")
          (:p :class "small help-text"
            "Enter a street address and click \"Next\". We'll show you a map to confirm the location.")
          (:form :method "post" :action "/settings/location"
            (:input :type "hidden"
                     :name "next"
                     :value "/home")
            (:input :type "text"
                    :name "address"
                    :placeholder "1600 Pennsylvania Avenue NW, Washington, DC"
                    :value (getf *user* :address))
            (:button :type "submit" :class "submit yes input-height" "Next"))
          )))
    :hide-menu t))

(defun get-home ()
  (cond
    ((or (confirmed-location) (not *user*))
     (when (and (getf *user* :fb-id)
                (not (getf *user* :pass)))
       (let ((password (random-password 11)))
         (modify-db *userid* :pass (new-password password))
         (delete-all-but-current-token-cookie)
         (flash
           (strcat* "<p>Your automatically generated password is:</p>"
                    "<p><strong>" password "</strong></p>"
                    "<p>You can change it on the settings page or by clicking "
                    "\"Forgot your password?\" when signing in.</p>")
           :additional-class "auto-fb-password")))
     (notice :home)
     (standard-home))

    ((and (getf *user* :lat)
          (getf *user* :long))
     (notice :home-verify-location)
     (get-verify-address :next-url "/home"))

    (t
     (notice :home-setup)
     (newuser-home))))
