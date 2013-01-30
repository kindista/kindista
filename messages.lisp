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
