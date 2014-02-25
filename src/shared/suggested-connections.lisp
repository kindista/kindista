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


(defun return-suggested-people (&optional (userid *userid*))
  "Tests to see if it is time to udpates the user's 'suggested connections'
  list. If so, updates the database. Either way returns that list."
  (when *user*
    (let ((current-time (get-universal-time))
          (time-updated (getf (gethash userid *person-suggested-connections-index*)
                              :time-updated)))
      (when (> (- current-time (or time-updated 0)) +day-in-seconds+)
        (update-suggestions-index userid current-time))
      (getf (gethash userid *person-suggested-connections-index*) :suggested))))


(defun update-suggestions-index (&optional (userid *userid*)
                                           (current-time (get-universal-time)))
  "For a given user's suggestion information, updates the current-time value to the current time. Recalculates the list of suggested people. Checks the list of hidden suggestions and removes them if their ranking has gone up by at least 4."

  (with-locked-hash-table (*person-suggested-connections-index*)
    (setf (gethash userid *person-suggested-connections-index*)
          `(:time-updated ,current-time
            :suggested ,(remove-hidden-suggestions
                          (calculate-suggested-people userid) userid)))))


(defun calculate-suggested-people (&optional (user-id *userid*))
  "Collects a list of people the user may know from their connections' connections, the people following them, and the people in the groups they are a member of.
Sorts them according to a rubric that takes into account if they are following the user, how many mutual connections they have with the user, and how many groups they are both a member of. Returns a list of plists with all relevant information."

  (let* ((users-connections (getf (db user-id) :following))
         (people-following-user (gethash user-id *followers-index*))
         (groups-user-is-in (get-users-groups user-id))
         (people-in-users-groups (flatten (loop for group in groups-user-is-in
                                                collect (group-members group))))
         (users-connections-connections
           (flatten (loop for person in users-connections
                          collect (gethash person *followers-index*)
                          collect (db person :following))))

         (list-to-sort ;this could be nested below
           (loop for person in (remove user-id
                                       (remove-duplicates
                                         (append people-following-user
                                                 people-in-users-groups
                                                 users-connections-connections)))
                 unless (or (member person users-connections)
                               (not (db person :active)))
                 collect person)))

    (labels ((num-of-mutuals (other-person)
               (length (mutual-connections other-person user-id)))
             (is-following-user-p (other-person)
               (when (find other-person people-following-user) t))
             (mutual-groups (other-person)
               (intersection groups-user-is-in (get-users-groups other-person)))
             (num-of-mutual-groups (other-person)
               (length (mutual-groups other-person)))
             (calculate-ranking (other-person)
               (+
                 (num-of-mutuals other-person)
                 (if (is-following-user-p other-person) 5 0)
                 (* (num-of-mutual-groups other-person) 3)))

            ;; later want to find a way to avoid cosntrucing this list
            ;; if ranking is 0
            ;;
             (other-person-data (other-person)
               (let ((reason-list (list other-person)))
                 (awhen (is-following-user-p other-person)
                   (nconc reason-list `(:following-user-p ,it)))
                 (awhen (mutual-groups other-person)
                   (nconc reason-list `(:mutual-groups ,it)))
                 (awhen (mutual-connections other-person user-id)
                   (nconc reason-list `(:num-of-mutuals ,(length it))))
                 (nconc reason-list `(:ranking ,(calculate-ranking other-person)))
                 reason-list))
             (get-ranking (data)
               (getf (cdr data) :ranking)))

      (let ((final-list
              (remove-if #'(lambda (data)
                             (< (get-ranking data) 2))
                         (sort (mapcar #'other-person-data list-to-sort)
                                #'> :key #'get-ranking))))
        (awhen final-list
          (subseq it 0 (min 100 (length it))))))))


(defun remove-hidden-suggestions (suggestion-list &optional (userid *userid*))
  "Iterates through a user's suggested connections with the following logic:
    - if there is a corresponding entry in hidden-suggested-contacts,
      - if the ranking is the same or within 4 points, remove the suggestion from the suggested connection list.
      - otherwise (if the ranking is 5 or more over), keep the suggestion and remove the corresponding entry in the hidden-suggested-contacts list."

  (remove-if #'(lambda (suggestion)
                 (aand (assoc (car suggestion) (db userid :hidden-suggested-contacts))
                       (if (< (- (getf (cdr suggestion) :ranking)
                                 (cdr it)) 5)
                         t
                         (progn
                           (remove-suggestion (car it) userid :hidden t)
                           nil))))
             suggestion-list))


(defun remove-suggestion (suggested-contact userid &key hidden)
  "By default, removes a given contact from the user's connections index (this same connection will be reinstated if not also entered into the 'hidden suggestions' value in that user's database entry). If hidden is non-nil, the function instead removes the contact from the 'hidden suggestions' list in the database."
  (if hidden
    (amodify-db userid :hidden-suggested-contacts
                (remove suggested-contact it :key #'car))
    (with-locked-hash-table (*person-suggested-connections-index*)
      (asetf (getf (gethash userid *person-suggested-connections-index*)
                   :suggested)
           (remove suggested-contact it :key #'car)))))


(defun hide-from-suggestions (suggested-contact userid)
  "Updates the :hidden-suggestion field in db with a cons cell. The (car) is the id of the hidden suggesiton, and the (cdr) is the ranking (retained because if their ranking goes up enough later, they will appear as a suggestion again)."

  (unless (assoc suggested-contact (db userid :hidden-suggested-contacts))
    (let ((ranking (or
            (getf (cdr (assoc suggested-contact
                              (getf (gethash userid
                                             *person-suggested-connections-index*)
                                    :suggested)))
                  :ranking)
            0)))

      (remove-suggestion suggested-contact userid)
      (amodify-db userid
                  :hidden-suggested-contacts
                  (push (cons suggested-contact ranking) it)))))


(defun quick-rank (other-person-id &optional (userid *userid*))
  "This is mainly used when a user removes another user from their contacts. Since there is no ranking information for them stored, this calculates it in order to store the removed user in the user's :hidden-suggested-contacts list."

  (+
    (length (mutual-connections other-person-id userid))
    (if (find other-person-id (gethash userid *followers-index*)) 5 0)
    (* (length (intersection (get-users-groups userid) (get-users-groups other-person-id))) 3)))


(defun suggestion-sidebar (&optional (userid *userid*))
  (awhen (return-suggested-people userid)

    (let ((suggestion-list it)
          (suggestion-chooser '(3 8 15)) ; here is where the number of sidebar suggestions can be changed and from where in the list they're selected 
          (sidebar-suggestions))

      (flet ((pick-unique-random (suggestion-sublist)
               ; Is there a cleaner way to do this? Only way to prevent an
               ; error when I have fewer than three suggestions.
               (when (> (length suggestion-sublist) (length sidebar-suggestions))
                 (asetf sidebar-suggestions
                      (append it (list (rand-from-list
                                         (set-difference
                                           suggestion-sublist it))))))))

        (awhen suggestion-list
          (dolist (x suggestion-chooser)
            (pick-unique-random
              (subseq it
                      0
                      (min x (length it)))))))

      (html
        (:div :class "item right only"
          (:h2 "People you may know")
         (dolist (suggestion sidebar-suggestions)
             (let* ((id (car suggestion))
                    (entity (db id))
                    (url-type (case (getf entity :type)
                                   (:person "people")
                                   (:group "groups")))
                    (reasons (cdr suggestion))
                    (link (s+ "/" url-type "/" (username-or-id id)))
                    (name (db id :name)))
                 (htm
                  (:div :class "side-sug card"
                   (:div :class "side-sug-image"
                    (:img :src (get-avatar-thumbnail id 100 100)))
                   (:div :class "side-sug-content"
                    (:form :class "float-right"
                          :method "post" :action "/people/suggested"
                          (htm
                            (:input :type "hidden"
                                    :name "remove"
                                    :value id)
                            (:input :type "hidden"
                                    :name "next"
                                    :value "/home")
                            (:button :class "simple-link gray-text"
                                     :name "submit"
                                     "x")))

                    (:h2 (:a :href link (str name)))
                    (if (getf reasons :following-user-p)
                      (htm (:p "Added you to their contacts"))
                      (aif (getf reasons :num-of-mutuals)
                        (htm (:p (:a :href (s+ link "/connections")
                                  (str (pluralize it "mutual connection")))))
                        (awhen (getf reasons :mutual-groups)
                          (htm (:p "Also a member of "
                                (str (db (rand-from-list it) :name)))))))

                  (:form :method "post" :action "/contacts" :class "add-sug"
                    (htm
                      (:input :type "hidden"
                              :name "add"
                              :value id)
                      (:input :type "hidden"
                              :name "next"
                              :value "/home")
                      (:button :class "simple-link"
                               :type "submit"
                               (:img :src "media/icons/add-contact.png" :class "icon")
                               (str " Add to contacts"))))))))))))))


(defun populate-suggestions-index ()
  (dolist (id *active-people-index*)
    (update-suggestions-index id)))


(defun reset-suggestions (userid &key hidden (reset-time t))
  "Removes all information from a user's suggestion list. If :hidden is non-nil, removes all hidden-suggestion info from that person's entry in the database."
  (if hidden
    (amodify-db userid :hidden-suggested-contacts (setf it nil))
    (with-locked-hash-table (*person-suggested-connections-index*)
      (setf (gethash userid *person-suggested-connections-index*)
            `(:suggestions nil
              :time-updated ,(if reset-time 0 (get-universal-time)))))))



