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

(defun create-person (&key name email password host)
  (insert-db `(:type :person
               :name ,name
               :emails ,(list email)
               :host ,host
               :help t
               :pass ,(new-password password)
               :created ,(get-universal-time)
               :notify-gratitude t
               :notify-message t
               :notify-kindista t)))

(defun index-person (id data)
  (let ((result (make-result :id id
                             :latitude (getf data :lat)
                             :longitude (getf data :long)
                             :type :person
                             :people (list id)
                             :time (getf data :created)))
        (names (cons (getf data :name)
                     (getf data :aliases))))

    (awhen (getf data :emails) 
      (dolist (email it) 
        (setf (gethash email *email-index*) id)))

    (setf (gethash (getf data :username) *username-index*) id)

    (awhen (getf data :following)
      (with-locked-hash-table (*followers-index*)
        (dolist (person it)
          (push id (gethash person *followers-index*)))))

    (when (getf data :loves)
      (with-locked-hash-table (*love-index*)
        (dolist (item (getf data :loves))
          (unless (member id (gethash item *love-index*))
            (push id (gethash item *love-index*))))))

    (with-locked-hash-table (*activity-person-index*)
      (asetf (gethash id *activity-person-index*)
             (sort (push result it) #'> :key #'result-time)))

    (when (and (getf data :lat) (getf data :long) (getf data :created))
      ;people that don't give a location don't get indexed
      (metaphone-index-insert names result)
      (geo-index-insert *people-geo-index* result)
      (geo-index-insert *activity-geo-index* result))))

(defun username-or-id (&optional (id *userid*))
  (or (getf (db id) :username)
      (write-to-string id)))

(defun profile-activity-items (&key (userid *userid*) (page 0) (count 20) next-url type)
  (let ((items (gethash userid *activity-person-index*))
        (start (* page 20)))
    (html
      (iter 
        (for i from 0 to (+ start count))
          (if (and (>= i start) items)
            (let ((item (car items)))
              (when (or (not type) (eql type (result-type item)))
                (case (result-type item)
                  (:gratitude 
                    (str (unless (and (eql type :gratitude) 
                                      (eql userid (first (result-people item))))
                           (gratitude-activity-item item
                                                    :next-url next-url))))
                  (:person 
                    (str (joined-activity-item item)))
                  (:resource
                    (str (inventory-activity-item "resource" item
                                              :show-what (unless (eql type :resource) t)
                                              :next-url next-url)))   
                  (:request
                    (str (inventory-activity-item "request" item
                                                :show-what (unless (eql type :request) t)
                                                :next-url next-url))))))
            (finish))

          (setf items (cdr items))

          (finally
            (when (or (> page 0) (cdr items))
              (htm
                (:div :class "item"
                 (when (> page 0)
                   (htm
                     (:a :href (strcat "/home?p=" (- page 1)) "< previous page")))
                 "&nbsp;"
                 (when (cdr items)
                   (htm
                     (:a :style "float: right;" :href (strcat "/home?p=" (+ page 1)) "next page >")))))))))))

(defun profile-tabs-html (userid &key tab)
  (let* ((bio (getf (db userid) :bio))
         (self (eql userid *userid*))
         (show-bio-tab (or bio self)))
    (html
      (:menu :class "bar"
        (:h3 :class "label" "Profile Menu")
        (when show-bio-tab
          (if (and self (eql tab :about))
            (htm (:li :class "selected" "About"))
            (htm (:li (:a :href (strcat *base-url* "/about") "About")))))

        (if (eql tab :activity)
          (htm (:li :class "selected" "Activity"))
          (htm (:li (:a :href (strcat *base-url* "/activity") "Activity"))))
        (if (eql tab :gratitude)
          (htm (:li :class "selected" "Reputation"))
          (htm (:li (:a :href (strcat *base-url* "/reputation") "Reputation"))))  
        (if (eql tab :resource)
          (htm (:li :class "selected" "Resources"))
          (htm (:li (:a :href (strcat *base-url* "/resources") "Resources"))))
        (if (eql tab :request)
          (htm (:li :class "selected" "Requests"))
          (htm (:li (:a :href (strcat *base-url* "/requests") "Requests"))))))))

(defun profile-bio-section-html (title content &key editing editable section-name)
  (when (string= content "")
    (setf content nil))

  (when (or content editing editable)
    (html
      (:div :class "bio-section"
        (:h2 (str title)
             (when (and editable (not editing))
               (htm (:a :href (strcat *base-url* "?edit=" section-name)
                        (:img :class "icon" :src "/media/icons/pencil.png")))))
        (cond
          (editing
            (htm
              (:form :method "post" :action "/settings"
                (:input :type "hidden" :name "next" :value *base-url*)
                (:textarea :name (strcat "bio-" section-name) (str content))
                (:button :class "yes" :type "submit" :name "save" "Save")
                (:a :class "cancel" :href *base-url* "Cancel")
                ))) 
          ((not content)
            (htm
              (:p :class "empty" "I'm empty... fill me out!")))

          (t
            (htm
              (:p (str content)))))))))

(defun profile-bio-html (userid &key editing)
  ; is the user editing one of the sections?
  ; should we show edit links for the sections?
  ;  if the user is looking at their own bio
  ;   and they are not currently editing another section
  ;
  (require-user
    (let* ((strid (username-or-id userid))
           (editable (when (not editing) (eql userid *userid*)))
           (user (db userid))
           (*base-url* (strcat "/people/" strid)))
      (standard-page
        (getf (db userid) :name)
        (html
          (str (profile-tabs-html userid :tab :about))
          (if (or (eql userid *userid*) (getf user :bio))
            (htm
              (:div :class "bio"
                (str (profile-bio-section-html
                       "My self-summary"
                       (getf user :bio-summary)
                       :section-name "summary"
                       :editing (eql editing 'summary)
                       :editable editable))  
                (str (profile-bio-section-html
                       "What I'm doing with my life"
                       (getf user :bio-doing)
                       :section-name "doing"
                       :editing (eql editing 'doing)
                       :editable editable))  
                (str (profile-bio-section-html
                       "What I'm really good at"
                       (getf user :bio-skills)
                       :section-name "skills"
                       :editing (eql editing 'skills)
                       :editable editable))
                (str (profile-bio-section-html
                       "I'm also into"
                       (getf user :bio-into)
                       :section-name "into"
                       :editing (eql editing 'into)
                       :editable editable))
                (str (profile-bio-section-html
                       "You should contact me if"
                       (getf user :bio-contact)
                       :section-name "contact"
                       :editing (eql editing 'contact)
                       :editable editable))))
            (htm (:h3 "This person hasn't written anything here."))))
        :right (when editing (html (:h2 "Style guide")))
        :top (profile-top-html userid)
        :selected "people"))))

(defun profile-top-html (userid)
  (let ((user (db userid))
        (is-contact (member userid (getf *user* :following))))
    (html
     (:img :class "bigavatar" :src (strcat "/media/avatar/" userid ".jpg"))
     (:div :class "basics"
       (:h1 (str (getf user :name)))
       (:p :class "city" (str (getf *user* :city)) ", " (str (getf *user* :state)))
     (unless (eql userid *userid*)
       (htm 
         (:form :method "GET" :action (strcat *base-url* "/message")
           (:input :type "submit" :value "Send a message")) 
         (:form :method "POST" :action "/contacts"
           (:input :type "hidden" :name (if is-contact "remove" "add") :value userid)
           (:input :type "hidden" :name "next" :value *base-url*)
           (:input :class (when is-contact "cancel") :type "submit" :value (if is-contact "Remove from contacts" "Add to contacts")))))))))

(defun profile-activity-html (userid &key type)
  (let* ((user (db userid))
         (strid (username-or-id userid))
         (*base-url* (strcat "/people/" strid)))
    (require-user
      (standard-page
        (getf user :name)
        (html
          (str (profile-tabs-html userid :tab (or type :activity)))
          (when (and (eql type :request) (eql userid *userid*))
            (htm (str (simple-inventory-entry-html "request"))))
          (when (and (eql type :resource) (eql userid *userid*))
            (htm (str (simple-inventory-entry-html "resource")))) 
          (when (and (eql type :gratitude) (not (eql userid *userid*))
            (htm
              (:div :class "item"
               (:h4 "Do you have gratitude to share about " (str (getf user :name)) "?")
               (:form :method "post" :action "/gratitude/new"
                 (:input :type "hidden" :name "subject" :value userid)
                 (:input :type "hidden" :name "next" :value (strcat *base-url* "/reputation"))
                 (:table :class "post"
                  (:tr
                    (:td (:textarea :cols "1000" :rows "4" :name "text"))
                    (:td
                      (:button :class "yes" :type "submit" :class "submit" :name "create" "Post")))))))))
          (:div :class "activity"
            (str (profile-activity-items :userid userid :type type))))

        :top (profile-top-html userid)

        :right (let ((mutuals (mutual-connections userid)))
                 (when (and mutuals (not (eql userid *userid*)))
                   (html
                     (:div :class "item people"
                      (:h3 "Mutual Connections")
                      (:ul
                        (dolist (id (mutual-connections userid))
                          (htm (:li (:a :href (strcat "/people/" (username-or-id id))
                                        (str (getf (db id) :name)))))))))))

        :selected "people"))))

(defmacro ensuring-userid ((user-id base-url) &body body)
  (let ((is-number (gensym))
        (user-name (gensym)) 
        (user-data (gensym)))
    `(let ((,is-number (scan +number-scanner+ ,user-id)))
       (if ,is-number
         (let* ((,user-id (parse-integer ,user-id))
                (,user-data (db ,user-id))
                (,user-name (getf ,user-data :username)))
           (if ,user-data
             (if ,user-name
               (see-other (format nil ,base-url ,user-name))
               (progn ,@body))
             (not-found)))
         (let ((,user-id (gethash ,user-id *username-index*)))
           (if ,user-id
             (progn ,@body)
             (not-found)))))))

(defroute "/people/<id>" (id)
  (:get
    (with-user
      (ensuring-userid (id "/people/~a")
        (let ((editing (get-parameter "edit"))
              (bio (getf (db id) :bio)))
          (cond
           ;((or (not editing)
           ;     (not (eql id *userid*)))
           ; (if (getf (db id) :bio)
           ;   (profile-bio-html id)
           ;   (profile-activity-html id)))

            ((not (eql id *userid*))
             (profile-activity-html id))

            ((not editing)
             (profile-bio-html id))

            ((string= editing "doing")
             (profile-bio-html id :editing 'doing))

            ((string= editing "contact")
             (profile-bio-html id :editing 'contact))

            ((string= editing "into")
             (profile-bio-html id :editing 'into))

            ((string= editing "summary")
             (profile-bio-html id :editing 'summary))

            ((string= editing "skills")
             (profile-bio-html id :editing 'skills))
            
            (t (not-found))))))))

(defroute "/people/<id>/about" (id)
  (:get
    (require-user
      (ensuring-userid (id "/people/~a/about")
        (profile-bio-html id)))))

(defroute "/people/<id>/activity" (id)
  (:get
    (with-user
      (ensuring-userid (id "/people/~a/activity")
        (profile-activity-html id)))))

(defroute "/people/<id>/reputation" (id)
  (:get
    (require-user
      (ensuring-userid (id "/people/~a/reputation")
        (profile-activity-html id :type :gratitude)))))

(defroute "/people/<id>/resources" (id)
  (:get
    (require-user
      (ensuring-userid (id "/people/~a/resources")
        (profile-activity-html id :type :resource)))))

(defroute "/people/<id>/requests" (id)
  (:get
    (require-user
      (ensuring-userid (id "/people/~a/requests")
        (profile-activity-html id :type :request)))))


(defun nearby-people (&optional (userid *userid*))
  (let* ((user (db userid)))
    (labels ((distance (result)
               (air-distance *latitude* *longitude* (result-latitude result) (result-longitude result))))
      (sublist (remove userid
                 (sort (geo-index-query *people-geo-index* *latitude* *longitude* 25)
                       #'< :key #'distance)
                 :key #'result-id)
             0 10))))

(defun mutual-connections (one &optional (two *userid*))
  (intersection (gethash one *followers-index*)
                (getf (db two) :following)))

(defun suggested-people (&optional (userid *userid*))
  ; get nearby people
  ; get contacts of contacts
  ; get distance and number of mutuals for each
  (sort
    (let* ((user (db userid))
           (following (getf user :following)))
      (iter (for person in (remove userid
                                   (iter (for person in following)
                                         (reducing (getf (db person) :following)
                                                   by #'union))))
            (unless (member person following)
              (collect (cons (length (mutual-connections person userid)) person)))))
    #'> :key #'first))

(defroute "/people/" ()
  (:get
    (moved-permanently "/people")))

(defroute "/people" ()
  (:get
    (with-user
      (standard-page
        "People"
        (html
          ; favorites / connections?
          (:h2 "Connect with people who live nearby")
          (with-location
            (dolist (data (nearby-people))
              (let* ((id (result-id data))
                     (person (db id)))
                (str (person-card id (getf person :name))))))
          (when *user* 
            (htm
              (:h2 "People with mutual connections") 
              (dolist (data (suggested-people))
                (let* ((id (cdr data))
                       (person (db id)))
                  ; (car data) is the number of mutual connections
                  (str (person-card id (getf person :name))))))))
        :selected "people"
        :right (html
                 (str (donate-sidebar))
                 (str (invite-sidebar)))))))
