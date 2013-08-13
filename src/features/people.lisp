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

(defun inactive-people ()
  "diagnostic tool for admins only. inefficient; should not be used in code."
  (set-difference
    (loop for id in (hash-table-keys *db-results*)
          when (eq (result-type (gethash id *db-results*)) :person)
          collect id)
    *active-people-index*
    :test #'equal))

(defun create-person (&key name email password host (pending nil))
  (insert-db `(:type :person
               :name ,name
               :emails ,(list email)
               :host ,host
               :pending ,pending
               :active t
               :help t
               :pass ,(new-password password)
               :created ,(get-universal-time)
               :notify-gratitude t
               :notify-message t
               :notify-reminders t
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

    (when (getf data :active)
      (with-mutex (*active-people-mutex*)
      (push id *active-people-index*)))

    (with-locked-hash-table (*db-results*)
      (setf (gethash id *db-results*) result))

    (awhen (getf data :emails) 
      (with-locked-hash-table (*email-index*)
        (dolist (email it) 
          (setf (gethash email *email-index*) id))))

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

    (awhen (getf data :host)
      (with-locked-hash-table (*invited-index*)
        (unless (member id (gethash it *invited-index*))
          (push id (gethash it *invited-index*)))))

    (with-locked-hash-table (*activity-person-index*)
      (asetf (gethash id *activity-person-index*)
             (sort (push result it) #'> :key #'result-time)))

    (unless (< (result-time result) (- (get-universal-time) 2592000))
      (with-mutex (*recent-activity-mutex*)
        (push result *recent-activity-index*)))

    (when (and (getf data :lat)
               (getf data :long)
               (getf data :created)
               (getf data :active))

      (metaphone-index-insert names result)
      (geo-index-insert *people-geo-index* result) 

      (unless (< (result-time result) (- (get-universal-time) 15552000))
        (geo-index-insert *activity-geo-index* result)))))

(defun reindex-person-location (id)
  ;when people change locations
  (let* ((result (gethash id *db-results*))
         (data (db id))
         (lat (getf data :lat))
         (long (getf data :long)))

    (unless result
      (notice :error :note "no db result on ungeoindex-person"))

    (geo-index-remove *people-geo-index* result)
    (geo-index-remove *activity-geo-index* result)  

    (setf (result-latitude result) lat)
    (setf (result-longitude result) long)

    (when (and lat long
               (getf data :created)
               (getf data :active))

      (metaphone-index-insert (cons (getf data :name) (getf data :aliases)) result)
      (geo-index-insert *people-geo-index* result) 

      (unless (< (result-time result) (- (get-universal-time) 15552000))
        (geo-index-insert *activity-geo-index* result))

      (dolist (id (gethash id *request-index*))
        (let ((result (gethash id *db-results*)))
          (geo-index-remove *request-geo-index* result)
          (geo-index-remove *activity-geo-index* result)
          (setf (result-latitude result) lat)
          (setf (result-longitude result) long)
          (unless (< (result-time result) (- (get-universal-time) 15552000))
            (geo-index-insert *activity-geo-index* result))
          (geo-index-insert *request-geo-index* result)))

      (dolist (id (gethash id *offer-index*))
        (let ((result (gethash id *db-results*)))
          (geo-index-remove *offer-geo-index* result)
          (geo-index-remove *activity-geo-index* result)
          (setf (result-latitude result) lat)
          (setf (result-longitude result) long)
          (geo-index-insert *offer-geo-index* result) 
          (unless (< (result-time result) (- (get-universal-time) 15552000))
            (geo-index-insert *activity-geo-index* result))))

      (dolist (id (gethash id *gratitude-index*))
        (let ((result (gethash id *db-results*)))
          (geo-index-remove *activity-geo-index* result)
          (setf (result-latitude result) lat)
          (setf (result-longitude result) long)
          (unless (< (result-time result) (- (get-universal-time) 15552000))
            (geo-index-insert *activity-geo-index* result)))))))

(defun reindex-person-names (id)
  (let* ((result (gethash id *db-results*))
         (data (db id))
         (names (cons (getf data :name)
                      (getf data :aliases)))) 
    (metaphone-index-insert names result)))

(defun deactivate-person (id)
  (let ((result (gethash id *db-results*)))
    (metaphone-index-insert (list nil) result)
    (geo-index-remove *people-geo-index* result)
    (geo-index-remove *activity-geo-index* result)
    (with-mutex (*active-people-mutex*)
      (asetf *active-people-index* (remove it *active-people-index*)))
    (dolist (request-id (gethash id *request-index*))
      (delete-inventory-item request-id))
    (dolist (offer-id (gethash id *offer-index*))
      (delete-inventory-item offer-id))
    (modify-db id :active nil
                  :notify-message nil
                  :notify-reminders nil
                  :notify-gratitude nil)))

(defun reactivate-person (id)
  (let* ((result (gethash id *db-results*))
         (data (db id))
         (names (cons (getf data :name)
                      (getf data :aliases))))
    (metaphone-index-insert names result)
    (geo-index-insert *people-geo-index* result) 
    (geo-index-insert *activity-geo-index* result) 
    (with-mutex (*active-people-mutex*)
      (push id *active-people-index*))
    (modify-db id :active t
                  :notify-message t
                  :notify-reminders t
                  :notify-gratitude t)))

(defun delete-pending-account (id)
  (let ((data (db id)))

    (deactivate-person id)

    (with-locked-hash-table (*pending-person-items-index*)
      (dolist (item-id (gethash id *pending-person-items-index*))
        (remove-from-db item-id))
      (remhash id *pending-person-items-index*))

    (awhen (getf data :emails)
      (with-locked-hash-table (*email-index*)
        (dolist (email it)
          (remhash email *email-index*)))
      (with-locked-hash-table (*banned-emails-index*)
        (dolist (email it)
          (setf (gethash email *banned-emails-index*) id))))

    (awhen (getf data :username)
      (with-locked-hash-table (*username-index*)
        (remhash it *username-index*)))

    (modify-db id :pending nil
                  :banned t
                  :notify-kindista nil)))

(defun username-or-id (&optional (id *userid*))
  (or (getf (db id) :username)
      (write-to-string id)))

(defun profile-activity-items (&key (userid *userid*) (page 0) (count 20) type)
  (let ((items (gethash userid *activity-person-index*))
        (start (* page count))
        (i 0))
    (html
      (iter
        (let ((item (car items)))
          (when (or (not item)
                    (>= i (+ count start)))
            (finish))

          (when (and (or (not type)
                         (and (eql type :gratitude)
                              (eql :gift (result-type item)))
                         (eql type (result-type item)))
                     (> (incf i) start))

            (case (result-type item)
              (:gratitude
                (when (or (not type)
                          (not (eql userid (first (result-people item)))))
                  (str (gratitude-activity-item item))))
              (:person
                (str (joined-activity-item item)))
              (:gift
                (when (or (not type)
                          (eql userid (first (result-people item))))
                  (str (gift-activity-item item))))
              (:offer
                (str (inventory-activity-item "offer" item
                                          :show-what (unless (eql type :offer) t))))
              (:request
                (str (inventory-activity-item "request" item
                                            :show-what (unless (eql type :request) t)))))))

          (setf items (cdr items))

          (finally
            (str (paginate-links page (cdr items))))))))

(defun profile-tabs-html (userid &key tab)
  (let* ((person (db userid))
         (profile-p (or (getf person :bio-into)
                        (getf person :bio-contact)
                        (getf person :bio-skils)
                        (getf person :bio-doing)
                        (getf person :bio-summary)))
         (self (eql userid *userid*))
         (active (getf person :active))
         (show-bio-tab (or profile-p self)))
    (html
      (:menu :class "bar"
        (:h3 :class "label" "Profile Menu")
        (when show-bio-tab
          (if (eql tab :about)
            (htm (:li :class "selected" "About"))
            (htm (:li (:a :href (strcat *base-url* "/about") "About")))))

        (if (eql tab :activity)
          (htm (:li :class "selected" "Activity"))
          (htm (:li (:a :href (strcat *base-url* "/activity") "Activity"))))
        (if (eql tab :gratitude)
          (htm (:li :class "selected" "Reputation"))
          (htm (:li (:a :href (strcat *base-url* "/reputation") "Reputation"))))
        (when (eq active t)
          (if (eql tab :offer)
            (htm (:li :class "selected" "Offers"))
            (htm (:li (:a :href (strcat *base-url* "/offers") "Offers"))))
          (if (eql tab :request)
            (htm (:li :class "selected" "Requests"))
            (htm (:li (:a :href (strcat *base-url* "/requests") "Requests")))))

        (when (mutual-connections userid)
          (if (eql tab :connections)
            (htm (:li :class "selected" "Mutual Connections"))
            (htm (:li :class "no-rightbar" (:a :href (strcat *base-url* "/connections") "Mutual Connections")))))))))

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
                (:input :type "hidden" :name "next" :value (strcat *base-url* "/about"))
                (:textarea :name (strcat "bio-" section-name)
                           (awhen content (str (escape-for-html it))))
                (:button :class "yes" :type "submit" :name "save" "Save")
                (:a :class "cancel" :href *base-url* "Cancel")
                )))
          ((not content)
            (htm
              (:p :class "empty" "I'm empty... fill me out!")))

          (t
            (htm
              (:p (str (html-text content))))))))))

(defun profile-top-html (userid)
  (let ((user (db userid))
        (is-contact (member userid (getf *user* :following))))
    (html
     (:img :class "bigavatar" :src (get-avatar-thumbnail userid 300 300))
     (:div :class "basics"
       (:h1 (str (getf user :name))
            (cond
              ((getf user :banned)
               (htm (:span :class "help-text" " (deleted account)")))
              ((eq (getf user :active) nil)
               (htm (:span :class "help-text" " (inactive account)")))))
       (when (getf user :city)
         (htm (:p :class "city" (str (getf user :city)) ", " (str (getf user :state)))))
     (unless (eql userid *userid*)
       (when (and (db userid :active) (db *userid* :active))
         (htm
           (:form :method "post" :action "/conversations/new"
             (:input :type "hidden" :name "next" :value (script-name*))
             (:button :class "yes" :type "submit" :name "add" :value userid "Send a message"))))
       (htm
         (:form :method "GET" :action (strcat "/people/" (username-or-id userid) "/reputation")
           (:button :class "yes" :type "submit" "Express gratitude"))

         (:form :method "POST" :action "/contacts"
           (:input :type "hidden" :name (if is-contact "remove" "add") :value userid)
           (:input :type "hidden" :name "next" :value *base-url*)
           (:button :class (if is-contact "cancel" "yes") :type "submit" (str (if is-contact "Remove from contacts" "Add to contacts"))))))))))

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
           (profile-p (or (getf user :bio-into)
                          (getf user :bio-contact)
                          (getf user :bio-skils)
                          (getf user :bio-doing)
                          (getf user :bio-summary)))
           (mutuals (mutual-connections userid))
           (mutual-links (html (:ul (dolist (link (alpha-people-links mutuals))
                                      (htm (:li (str link)))))))
           (*base-url* (strcat "/people/" strid)))
      (standard-page
        (getf (db userid) :name)
        (html
          (str (profile-tabs-html userid :tab :about))
          (if (or (eql userid *userid*) profile-p)
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
        :right (if editing
                 (when mutuals
                   (mutual-connections-sidebar mutual-links)))
        :top (profile-top-html userid)
        :selected "people"))))

(defun mutual-connections-sidebar (link-list)
  (html
    (:div :class "people item right only"
     (:h3 "Mutual Connections")
     (str link-list))))

(defun profile-activity-html (userid &key type)
  (let* ((user (db userid))
         (strid (username-or-id userid))
         (mutuals (mutual-connections userid))
         (mutual-links (html (:ul (dolist (link (alpha-people-links mutuals))
                                      (htm (:li (str link)))))))
         (*base-url* (strcat "/people/" strid)))
    (standard-page
      (getf user :name)
      (html
        (when *user* (str (profile-tabs-html userid :tab (or type :activity))))
        (when (and (eql type :request) (eql userid *userid*) )
          (htm (str (simple-inventory-entry-html "a" "request"))))
        (when (and (eql type :offer) (eql userid *userid*))
          (htm (str (simple-inventory-entry-html "an" "offer"))))
        (when (and (eql type :gratitude)
                   (not (eql userid *userid*))
                   (eql (getf user :active) t))
          (htm
            (:div :class "item"
             (:h4 "Do you have gratitude to share for " (str (getf user :name)) "?")
             (:form :method "post" :action "/gratitude/new"
               (:input :type "hidden" :name "subject" :value userid)
               (:input :type "hidden" :name "next" :value (strcat *base-url* "/reputation"))
               (:table :class "post"
                (:tr
                  (:td (:textarea :cols "1000" :rows "4" :name "text"))
                  (:td
                    (:button :class "yes" :type "submit" :class "submit" :name "create" "Post"))))))))

        (:div :class "activity"
          (str (profile-activity-items :userid userid :type type :page (aif (get-parameter "p") (parse-integer it) 0)))))

      :top (profile-top-html userid)

      :right (when mutuals (mutual-connections-sidebar mutual-links))

      :selected "people")))

(defun profile-mutual-connections-html (userid)
  (let* ((user (db userid))
         (strid (username-or-id userid))
         (mutuals (mutual-connections userid))
         (*base-url* (strcat "/people/" strid)))
    (standard-page
      (getf user :name)
      (html
        (when *user* (str (profile-tabs-html userid :tab :connections)))
        (:div :class "activity"
          (:ul :class "mutuals-list" (dolist (link (alpha-people-links mutuals))
                 (htm (:li (str link)))))))

      :top (profile-top-html userid)

      :selected "people")))

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

(defun get-person (id)
  (ensuring-userid (id "/people/~a")
    (let ((editing (get-parameter "edit")))
      (cond
       ;((or (not editing)
       ;     (not (eql id *userid*)))
       ; (if (getf (db id) :bio)
       ;   (profile-bio-html id)
       ;   (profile-activity-html id)))

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

        ((or (not (eql id *userid*))
             (getf *user* :bio-summary)
             (getf *user* :bio-into)
             (getf *user* :bio-contact)
             (getf *user* :bio-skils)
             (getf *user* :bio-doing))
         (profile-activity-html id))

        ((eql id *userid*)
         (profile-bio-html id))

        (t (not-found))))))

(defun get-person-about (id)
  (require-user
    (ensuring-userid (id "/people/~a/about")
      (profile-bio-html id))))

(defun get-person-activity (id)
  (ensuring-userid (id "/people/~a/activity")
    (profile-activity-html id)))

(defun get-person-reputation (id)
  (require-user
    (ensuring-userid (id "/people/~a/reputation")
      (profile-activity-html id :type :gratitude))))

(defun get-person-offers (id)
  (require-user
    (ensuring-userid (id "/people/~a/offers")
      (profile-activity-html id :type :offer))))

(defun get-person-requests (id)
  (require-user
    (ensuring-userid (id "/people/~a/requests")
      (profile-activity-html id :type :request))))

(defun get-person-mutual-connections (id)
  (require-user
    (ensuring-userid (id "/people/~a/connections")
      (profile-mutual-connections-html id))))

(defun nearby-people ()
  (with-location
    (labels ((distance (result)
               (air-distance *latitude* *longitude* (result-latitude result) (result-longitude result))))
      (mapcar #'result-id
              (sort (remove *userid* (geo-index-query *people-geo-index* *latitude* *longitude* 50) :key #'result-id)
                    #'< :key #'distance)))))

(defun mutual-connections (one &optional (two *userid*))
  (intersection (gethash one *followers-index*)
                (getf (db two) :following)))

(defun contactp (id)
  (member id (getf *user* :following)))

(defun suggested-people (&optional (userid *userid*))
  ; get nearby people
  ; get contacts of contacts
  ; get distance and number of mutuals for each
  (flet ((mutuals (person)
           (length (mutual-connections person userid))))
    (sort
      (let* ((user (db userid))
             (following (getf user :following)))
        (iter (for person in (remove userid
                                     (iter (for person in following)
                                           (reducing (getf (db person) :following)
                                                     by #'union))))
              (unless (or (member person following)
                          (not (db person :active)))
                (collect person))))
      #'> :key #'mutuals)))

(defun sorted-contacts (&optional (userid *userid*))
  (sort
    (copy-list (db userid :following))
    #'string-lessp :key #'person-name))

(defun go-people ()
  (moved-permanently "/people"))

(defun people-tabs-html (&key (tab :contacts))
  (html
    (:menu :class "bar"
      (:h3 :class "label" "People Menu")
      
      (if (eql tab :contacts)
        (htm (:li :class "selected" "Contacts"))
        (htm (:li (:a :href "/people/contacts" "Contacts"))))

      (if (eql tab :nearby)
        (htm (:li :class "selected" "Nearby"))
        (htm (:li (:a :href "/people/nearby" "Nearby"))))

      (if (eql tab :suggested)
        (htm (:li :class "selected" "Suggested"))
        (htm (:li (:a :href "/people/suggested" "Suggested"))))

      (if (eql tab :invited)
        (htm (:li :class "selected" "Invited"))
        (htm (:li (:a :href "/people/invited" "Invited")))))))

(defun get-people ()
  (if *user*
    (see-other "/people/contacts")
    (see-other "/people/nearby")))

(defun get-people-contacts ()
  (if *user*
    (let ((contacts (sorted-contacts)))
      (standard-page
      "Contacts"
      (html
        (str (people-tabs-html))
        (unless contacts
          (htm (:h3 "No contacts")))
        (let ((letters ()))
          (dolist (id contacts)
            (let ((char (char-upcase (elt (db id :name) 0))))
              (unless (member char letters)
                (push char letters))))
          (htm
            (:a :name "index")
            (:p (fmt "~{<a href=#~(~:C~)>~:*~:C</a>~^ | ~}" (nreverse letters)))))

        (let ((letter nil))
          (dolist (id contacts)
            (let ((char (char-upcase (elt (db id :name) 0))))
              (unless (eq char letter)
                (htm (:a :name (char-downcase char))
                     (:h3 (str char) (:small " (" (htm (:a :href "#index" " back to index ")) ")")))
                (setf letter char))
              (str (person-card id (db id :name)))))))

      :selected "people"
      :right (html
               (str (donate-sidebar))
               (str (invite-sidebar)))))
    (see-other "/people/nearby")))

(defun get-people-nearby ()
  (let* ((page (if (scan +number-scanner+ (get-parameter "p"))
                (parse-integer (get-parameter "p"))
                0))
         (start (* page 20)))
    
    (standard-page
      "Nearby people"
      (html
        (when *user* (str (people-tabs-html :tab :nearby)))
        (multiple-value-bind (ids more) (sublist (nearby-people) start 20)
          (when (> page 0)
            (str (paginate-links page more)))
          (dolist (id ids)
            (str (person-card id (db id :name))))
          (str (paginate-links page more))))

      :selected "people"
      :right (html
               (str (donate-sidebar))
               (str (invite-sidebar))))))

(defun get-people-suggested ()
  (if *user*
    (standard-page
      "Suggested people"
      (html
        (str (people-tabs-html :tab :suggested))
        (dolist (id (suggested-people))
          (str (person-card id (db id :name)))))
      :selected "people"
      :right (html
               (str (donate-sidebar))
               (str (invite-sidebar))))
    (see-other "/people/nearby")))

(defun get-people-invited ()
  (if *user*
    (let ((unconfirmed (unconfirmed-invitations))
          (confirmed (gethash *userid* *invited-index*)))
      (standard-page
        "Invited"
        (html
          (str (people-tabs-html :tab :invited))

          (when unconfirmed
            (htm
              (:h2 "Unconfirmed invitations")
              (:ul
              (dolist (email unconfirmed)
                (htm (:li (str email)))))))

          (when confirmed
            (htm
              (dolist (id confirmed)
                (str (person-card id (db id :name))))))
          
          (unless (or confirmed unconfirmed)
            (htm
              (:h2 "No invitations yet.")
              (:p "Would you like to " (:a :href "/invite" "invite someone") "?"))))

        :selected "people"
        :right (html
                 (str (donate-sidebar))
                 (str (invite-sidebar)))))
      (see-other "/people/nearby")))

