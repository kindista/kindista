(asdf:defsystem #:kindista
  :name "Kindista"
  :description "A social network for local sharing"
  :license "GNU Affero General Public License Version 3 (see file COPYING)"
  :maintainer "Benjamin J. Crandall"
  :serial t
  :depends-on (:alexandria
               :anaphora
               ;:cl-gd
               :cl-json
               :cl-markdown
               :cl-fad
               :cl-ppcre
               :cl-smtp
               :cl-who
               :cl-stripe
               ;:css-lite
               :chronicity
               :vecto
               :double-metaphone
               :drakma
               :dexador
               :flexi-streams
               :hunchentoot
               :ironclad
               :iterate
               :paren-files
               :sb-concurrency
               :stem
              ;:adw-charting-vecto
               :kindista-js
               :quri)
  :components ((:module src
                :serial t
                :components ((:file "package")
                             (:file "helpers")
                             (:file "settings")
                             (:module db
                              :serial t
                              :components ((:file "indexes")
                                           (:file "main")))
                             (:module log
                              :serial t
                              :components ((:file "main")
                                           (:file "events")))
                             (:module analytics
                                      :serial t
                                      :components ((:file "utilities")
                                                   (:file "metric-system")))
                             (:module http
                              :serial t
                              :components ((:file "main")))
                             (:module templates
                              :serial t
                              :components ((:file "sidebar")
                                           (:file "timestamp")
                                           (:file "menu-horiz")
                                           (:file "card")
                                           (:file "group-card")
                                           (:file "reciprocity")
                                           (:file "person-card")))
                             (:module shared
                              :serial t
                              :components ((:file "broadcasts")
                                           (:file "facebook")
                                           (:file "form-elements")
                                           (:file "inventory")
                                           (:file "images")
                                           (:file "activity")
                                           (:file "geo")
                                           (:file "tags")
                                           (:file "timeline")
                                           (:file "paginate")
                                           (:file "profiles")
                                           (:file "matchmaker")
                                           (:file "verify-location")
                                           (:file "time")
                                           (:file "suggested-connections")))
                             (:module features
                              :serial t
                              :components ((:file "about")
                                           (:file "admin")
                                           (:file "blog")
                                           (:file "comments")
                                           (:file "contacts")
                                           (:file "invitations")
                                           (:file "donate")
                                           (:file "events")
                                           (:file "expirations")
                                           (:file "gratitude")
                                           (:file "groups")
                                           (:file "help")
                                           (:file "home")
                                           (:file "legacy")
                                           (:file "login")
                                           (:file "love")
                                           (:file "messages")
                                           (:file "conversations")
                                           (:file "offers")
                                           (:file "people")
                                           (:file "privacy")
                                           (:file "push-notifications")
                                           (:file "requests")
                                           (:file "request-invitation")
                                           (:file "reset-password")
                                           (:file "search")
                                           (:file "settings")
                                           (:file "signup")
                                           (:file "splash")
                                           (:file "terms")
                                           (:file "transactions")))
                             (:file "routes")
                             (:file "scheduler")
                             (:file "main")
                             (:module email
                              :serial t
                              :components ((:file "helpers")
                                           (:file "blog-comment-notification")
                                           (:file "contact-notification")
                                           (:file "email-verification")
                                           (:file "error-notifications")
                                           (:file "expired-inventory-notice")
                                           (:file "inventory-digest")
                                           (:file "feedback-notification")
                                           (:file "feedback-reply-notification")
                                           (:file "gratitude-notification")
                                           (:file "message-notification")
                                           (:file "pending-offer-notification")
                                           (:file "pending-account-approval")
                                           (:file "reminders")
                                           (:file "reset-password")
                                           (:file "transaction-actions")
                                           (:file "group-membership-invitation-notification")
                                           (:file "group-membership-request-notification")
                                           (:file "unsubscribe")
                                           (:file "matching-offer-notification")
                                           (:module invitations
                                                    :serial t
                                                    :components ((:file "standard-invite")
                                                                 (:file "requested-invite")
                                                                 (:file "expired-reminder")))))
                                                                 ))))
