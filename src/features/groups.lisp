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

    (with-locked-hash-table (*group-membership-index*)
      (dolist (person (result-people result))
        (push id (gethash person *group-membership-index*))))

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
                      :admins (list admin-id)))

(defun group-members (groupid)
  (let ((group (db groupid)))
    (append (getf group :admins) (getf group :members))))

(defun add-group-member (personid groupid)
  (with-locked-hash-table (*group-membership-index*)
    (push groupid (gethash personid *group-membership-index*)))
  (amodify-db groupid :members (pushnew personid it)) )

(defun remove-group-member (personid groupid)
  (with-locked-hash-table (*group-membership-index*)
    (asetf (gethash personid *group-membership-index*)
      (remove groupid it)))
  (amodify-db groupid :members (remove personid it)) )

(defun add-group-admin (personid groupid)
  (assert (eql (db personid :type) :person))
  (if (member personid (db groupid :members))
    (amodify-db groupid :members (remove personid it)
                         :admins (pushnew personid it))
    (amodify-db groupid :admins (pushnew personid it))))

(defun remove-group-admin (personid groupid)
  (assert (eql (db personid :type) :person))
  (amodify-db groupid :admins (remove personid it)
                       :members (pushnew personid it)))

(defun members-sidebar (groupid)
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
        (:div :class "people item right only"
        (:h3 "Group members")
        (:ul
          (dolist (list (sort (mapcar #'triple members) #'member-less-p))
            (htm
              (:li (str (third list))
               (when (admin-p (first list))
                 (htm " (admin)")))))))))))

(defun group-activity-html (groupid &key type)
  (profile-activity-html groupid :type type
                                 :right (members-sidebar groupid)))

(defun get-group (id)
  (ensuring-userid (id "/groups/~a")
    (group-activity-html id)))
