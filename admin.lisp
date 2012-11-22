(in-package :kindista)

(defroute "/admin" ()
  (:get
    (require-admin
      (standard-page
        "Events"
        (html
          (:h1 "Admin")
          (:h2 "Ideas")
          (:ul
            (:li "create a new event")
            (:li "upcoming events")
            (:li "events friends are going to")
            ))
        :selected "admin"))))

