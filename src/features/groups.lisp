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

(defun create-group (&key name creator)
  (insert-db `(:type :group
               :name ,name
               :creator ,creator
               :admins ,(list creator)
               :active t
               :notify-gratitude ,(list creator)
               :notify-reminders ,(list creator)
               :notify-invite-request ,(list creator)
               :created ,(get-universal-time))))

(defun index-group (id data)
  (let ((result (make-result :id id
                             :latitude (getf data :lat)
                             :longitude (getf data :long)
                             :type :group
                             :people (append (getf data :admins)
                                             (getf data :members))
                             :time (getf data :created)))
        (name (getf data :name)))

    (with-locked-hash-table (*db-results*)
      (setf (gethash id *db-results*) result))

    (setf (gethash (getf data :username) *username-index*) id)

    (with-locked-hash-table (*group-priviledges-index*)
      (dolist (person (getf data :admins))
        (push id (getf (gethash person *group-priviledges-index*) :admin)))
      (dolist (person (getf data :members))
        (push id (getf (gethash person *group-priviledges-index*) :member))))

    (with-locked-hash-table (*profile-activity-index*)
      (asetf (gethash id *profile-activity-index*)
             (sort (push result it) #'> :key #'result-time)))

    (unless (< (result-time result) (- (get-universal-time) 2592000))
      (with-mutex (*recent-activity-mutex*)
        (push result *recent-activity-index*)))

    (when (and (getf data :lat)
               (getf data :long)
               (getf data :created)
               (getf data :active))

      (metaphone-index-insert (list name) result)
      (geo-index-insert *groups-geo-index* result) 

      (unless (< (result-time result) (- (get-universal-time) 15552000))
        (geo-index-insert *activity-geo-index* result)))))

(defun reindex-group-location (id)
  ;when groups change locations
  (let* ((result (gethash id *db-results*))
         (data (db id))
         (lat (getf data :lat))
         (long (getf data :long)))

    (unless result
      (notice :error :note "no db result on reindex-group-location"))

    (geo-index-remove *groups-geo-index* result)
    (geo-index-remove *activity-geo-index* result)

    (setf (result-latitude result) lat)
    (setf (result-longitude result) long)

    (when (and lat long
               (getf data :created)
               (getf data :active))

      (metaphone-index-insert (list (getf data :name)) result)
      (geo-index-insert *groups-geo-index* result)

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

(defun reindex-group-name (id)
  (metaphone-index-insert (list (db id :name))
                          (gethash id *db-results*)))

(defun change-person-to-group (groupid admin-id)
  (modify-db groupid :type :group
                     :creator admin-id
                     :admins (list admin-id)
                     :notify-gratitude (list admin-id)
                     :notify-reminders (list admin-id)
                     :notify-invite-request (list admin-id)
                     :notify-message nil
                     :notify-kindista nil
                     :notify-expired-invites nil
                     :pass nil)
  (index-group groupid (db groupid))
  (reindex-group-location groupid)
  (reindex-group-name groupid))

(defun group-members (groupid)
  (let ((group (db groupid)))
    (append (getf group :admins) (getf group :members))))

(defun add-group-member (personid groupid)
  (with-locked-hash-table (*group-priviledges-index*)
     (pushnew groupid (getf (gethash personid *group-priviledges-index*) :member)))

  (amodify-db groupid :members (pushnew personid it)) )

(defun remove-group-member (personid groupid)
  (with-locked-hash-table (*group-priviledges-index*)
     (asetf (getf (gethash personid *group-priviledges-index*) :member)
       (remove groupid it)))
  (amodify-db groupid :members (remove personid it)) )

(defun add-group-admin (personid groupid)
  (assert (eql (db personid :type) :person))
  (with-locked-hash-table (*group-priviledges-index*)
     (asetf (getf (gethash personid *group-priviledges-index*) :admin)
            (pushnew groupid it))
     (asetf (getf (gethash personid *group-priviledges-index*) :member)
            (remove groupid it)))
  (if (member personid (db groupid :members))
    (amodify-db groupid :members (remove personid it)
                        :admins (pushnew personid it))
    (amodify-db groupid :admins (pushnew personid it))))

(defun remove-group-admin (personid groupid)
  (assert (eql (db personid :type) :person))
  (with-locked-hash-table (*group-priviledges-index*)
     (asetf (getf (gethash personid *group-priviledges-index*) :admin)
            (remove groupid it))
     (asetf (getf (gethash personid *group-priviledges-index*) :member)
            (pushnew groupid it)))
  (amodify-db groupid :admins (remove personid it)
                      :members (pushnew personid it)))

(defun group-member-links (groupid &key ul-class)
  (let* ((group (db groupid))
         (admins (getf group :admins))
         (members (append admins (getf group :members))))
    (labels ((admin-p (id)
               (not (null (member id admins))))
             (member-less-p (m1 m2)
               (cond
                 ((eql (admin-p (first m1))
                       (admin-p (first m2)))
                  (string-lessp (second m1) (second m2)))
                 ((admin-p (first m1))
                  t)
                 (t
                  nil)))
             (triple (id)
               (let ((name (db id :name)))
                 (list id name (person-link id)))))
      (html
        (:ul :class (or ul-class "")
          (dolist (list (sort (mapcar #'triple members) #'member-less-p))
            (htm
              (:li (str (third list))
               (when (admin-p (first list))
                 (htm " (admin)"))))))))))

(defun members-sidebar (groupid)
  (html
    (:div :class "people item right only"
    (:h3 "Group members")
    (str (group-member-links groupid)))))

(defun profile-group-members-html (groupid)
  (let* ((group (db groupid))
         (strid (username-or-id groupid))
         (*base-url* (strcat "/groups/" strid)))
    (standard-page
      (getf group :name)
      (html
        (when *user* (str (profile-tabs-html groupid :tab :members)))
        (:div :class "activity"
          (str (group-member-links groupid :ul-class "mutuals-list"))))
      :top (profile-top-html groupid)
      :selected "groups")))

(defun group-activity-html (groupid &key type)
  (profile-activity-html groupid :type type
                                 :right (members-sidebar groupid)))

(defun get-groups ()
  (if (and *user*
           (> (+ (length (getf *user-group-priviledges* :admin))
                 (length (getf *user-group-priviledges* :member)))
              0))
    (see-other "/groups/my-groups")
    (see-other "/groups/nearby")))

(defun groups-with-user-as-admin (&optional (userid *userid*))
"Returns an a-list of (groupid . group-name)"
  (loop for id in (getf (if (eql userid *userid*)
                          *user-group-priviledges*
                          (gethash userid *group-priviledges-index*))
                        :admin)
        collect (cons id (db id :name))))


(defun groups-with-user-as-member (&optional (userid *userid*))
"Returns an a-list of (groupid . group-name)"
  (loop for id in (getf (if (eql userid *userid*)
                          *user-group-priviledges*
                          (gethash userid *group-priviledges-index*))
                        :member)
        collect (cons id (db id :name))))

(defun group-admin-p (groupid &optional (userid *userid*))
  (member groupid (getf (if (eql userid *userid*)
                          *user-group-priviledges*
                          (gethash userid *group-priviledges-index*)) :admin)))

(defun group-activity-selection-html (groupid group-name selected tab)
  (html
    (:form :method "get" :action (strcat "/groups/" (username-or-id groupid) "/" tab)
      (:strong :class "small"
       "Display "
       (str (cond
             ((or (string= tab "activity")
                  (string= tab "offers")
                  (string= tab "requests"))
              (s+ tab " from "))
             ((string= tab "reputation")
        "gratitude about "))))
      (:select :class "group-activity" :name "display" :onchange "this.form.submit()"
        (:option :value "all" :selected (when (string= selected "all") "")
          (str (s+ group-name " and its members")))
        (:option :value "group" :selected (when (string= selected "group") "")
          (str group-name))
        (:option :value "members" :selected (when (string= selected "members") "")
          (str (s+ group-name "'s members"))))
      " "
      (:input :type "submit" :class "no-js" :value "apply"))))

(defun group-member-activity (group-members &key type count)
  (let ((count (or count (+ 20 (floor (/ 30 (length group-members))))))
        (i 0)
        (activity nil))
    (dolist (person group-members)
      (setf i 0)
      (loop for result in (gethash person *profile-activity-index*)
            while (< i count)
            when (or (not type)
                     (if (eql type :gratitude)
                       (or (and (eql :gratitude (result-type result))
                                (not (eql person (first (result-people result)))))
                           (and (eql :gift (result-type result))
                                (not (eql person (car (last (result-people result)))))))
                       (eql type (result-type result))))
            do (progn
                 (if activity
                   (asetf activity (sort (push result it) #'< :key #'activity-rank))
                   (asetf activity (list result)))
                 (incf i))))
    activity))

(defun get-my-groups ()
  (if *user*
    (let ((admin-groups (groups-with-user-as-admin))
          (member-groups (groups-with-user-as-member)))
      (standard-page
        "My Groups"
        (html
          (str (groups-tabs-html :tab :my-groups))
          (unless (or admin-groups member-groups)
            (htm (:h3 "You have not joined any groups yet.")))
          (dolist (group (sort admin-groups #'< :key #'last))
            (str (group-card (car group))))
          (dolist (group (sort member-groups #'< :key #'last))
            (str (group-card (car group)))))
        :selected "groups"
        :right (html
                 (str (donate-sidebar))
                 (str (invite-sidebar)))))

      (see-other "groups/nearby")))

(defun get-groups-nearby ()
  (nearby-profiles-html "groups" (groups-tabs-html :tab :nearby)))

(defun get-group (id)
  (ensuring-userid (id "/groups/~a")
    (group-activity-html id)))

(defun get-group-about (id)
  (require-user
    (ensuring-userid (id "/groups/~a/about")
      (profile-bio-html id))))

(defun get-group-activity (id)
  (ensuring-userid (id "/groups/~a/activity")
    (group-activity-html id)))

(defun get-group-reputation (id)
  (require-user
    (ensuring-userid (id "/groups/~a/reputation")
      (group-activity-html id :type :gratitude))))

(defun get-group-offers (id)
  (require-user
    (ensuring-userid (id "/groups/~a/offers")
      (group-activity-html id :type :offer))))

(defun get-group-requests (id)
  (require-user
    (ensuring-userid (id "/groups/~a/requests")
      (group-activity-html id :type :request))))

(defun get-group-members (id)
  (require-user
    (ensuring-userid (id "/groups/~a/members")
      (profile-group-members-html id))))

(defun groups-tabs-html (&key (tab :my-groups))
  (html
    (:menu :class "bar"
      (:h3 :class "label" "Groups Menu")

      (if (eql tab :my-groups)
        (htm (:li :class "selected" "My Groups"))
        (htm (:li (:a :href "/groups/my-groups" "My Groups"))))

      (if (eql tab :nearby)
        (htm (:li :class "selected" "Nearby"))
        (htm (:li (:a :href "/groups/nearby" "Nearby"))))

     ;(if (eql tab :suggested)
     ;  (htm (:li :class "selected" "Suggested"))
     ;  (htm (:li (:a :href "/people/suggested" "Suggested"))))

     ;(if (eql tab :invited)
     ;  (htm (:li :class "selected" "Invited"))
     ;  (htm (:li (:a :href "/people/invited" "Invited"))))
      )))
