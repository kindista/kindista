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
               :members ,(list creator)
               :created ,(get-universal-time))))

(defun index-group (id data)
  (let ((result (make-result :id id
                             :latitude (getf data :lat)
                             :longitude (getf data :long)
                             :type :group
                             :people (getf data :members)
                             :time (getf data :created)))
        (name (getf data :name)))

    (with-locked-hash-table (*db-results*)
      (setf (gethash id *db-results*) result))

    (setf (gethash (getf data :username) *username-index*) id)

    
    (with-locked-hash-table (*group-membership-index*)
      (dolist (person (getf data :members))
        (push id (gethash person *group-membership-index*))))

    (with-locked-hash-table (*activity-group-index*)
      (asetf (gethash id *activity-group-index*)
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

(defun members-sidebar (members admins)
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
               (htm " (admin)"))))))))))

(defun group-activity-html (groupid &key type)
  (let* ((group (db groupid))
         (strid (username-or-id groupid))
         (members (getf group :members))
         (*base-url* (strcat "/groups/" strid)))
    (standard-page
      (getf group :name)
      (html
        (when *user* (str (profile-tabs-html groupid :tab (or type :activity))))
        (when (eql type :request)
          (htm (str (simple-inventory-entry-html "a" "request"))))
        (when (eql type :offer)
          (htm (str (simple-inventory-entry-html "an" "offer"))))
        (when (and (eql type :gratitude)
                   (eql (getf group :active) t))
          (htm
            (:div :class "item"
             (:h4 "Do you have gratitude to share for " (str (getf group :name)) "?")
             (:form :method "post" :action "/gratitude/new"
               (:input :type "hidden" :name "subject" :value groupid)
               (:input :type "hidden" :name "next" :value (strcat *base-url* "/reputation"))
               (:table :class "post"
                (:tr
                  (:td (:textarea :cols "1000" :rows "4" :name "text"))
                  (:td
                    (:button :class "yes" :type "submit" :class "submit" :name "create" "Post"))))))))

        (:div :class "activity"
          (str (profile-activity-items :userid groupid :type type :page (aif (get-parameter "p") (parse-integer it) 0)))))

      :top (profile-top-html groupid)

      :right (members-sidebar members (getf group :admins))

      :selected "groups")))

(defun get-group (id)
  (ensuring-userid (id "/groups/~a")
    (group-activity-html id)))
