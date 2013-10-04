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

(defun profile-activity-items (&key (id *userid*) (page 0) (count 20) type)
  (let ((items (gethash id *profile-activity-index*))
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
                          (not (eql id (first (result-people item)))))
                  (str (gratitude-activity-item item))))
              (:person
                (str (joined-activity-item item)))
              (:gift
                (when (or (not type)
                          (eql id (first (result-people item))))
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

(defun profile-top-html (id)
  (let ((entity (db id))
        (is-contact (member id (getf *user* :following))))
    (html
     (:img :class "bigavatar" :src (get-avatar-thumbnail id 300 300))
     (:div :class "basics"
       (:h1 (str (getf entity :name))
            (cond
              ((getf entity :banned)
               (htm (:span :class "help-text" " (deleted account)")))
              ((eq (getf entity :active) nil)
               (htm (:span :class "help-text" " (inactive account)")))))
       (when (getf entity :city)
         (htm (:p :class "city" (str (getf entity :city)) ", " (str (getf entity :state)))))
     (unless (eql id *userid*)
       (when (and (db id :active) (db *userid* :active))
         (htm
           (:form :method "post" :action "/conversations/new"
             (:input :type "hidden" :name "next" :value (script-name*))
             (:button :class "yes" :type "submit" :name "add" :value id "Send a message"))))
       (htm
         (:form :method "GET" :action (strcat "/people/" (username-or-id id) "/reputation")
           (:button :class "yes" :type "submit" "Express gratitude"))

         (:form :method "POST" :action "/contacts"
           (:input :type "hidden" :name (if is-contact "remove" "add") :value id)
           (:input :type "hidden" :name "next" :value *base-url*)
           (:button :class (if is-contact "cancel" "yes") :type "submit" (str (if is-contact "Remove from contacts" "Add to contacts"))))))))))

(defun profile-tabs-html (id &key tab)
  (let* ((entity (db id))
         (profile-p (or (getf entity :bio-into)
                        (getf entity :bio-contact)
                        (getf entity :bio-skils)
                        (getf entity :bio-doing)
                        (getf entity :bio-summary)))
         (self (eql id *userid*))
         (admin (member *userid* (getf entity :admins)))
         (active (getf entity :active))
         (show-bio-tab (or profile-p self admin)))
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

        (when (and (eql (getf entity :type) :person)
                   (mutual-connections id))
          (if (eql tab :connections)
            (htm (:li :class "selected" "Mutual Connections"))
            (htm (:li :class "no-rightbar" (:a :href (strcat *base-url* "/connections") "Mutual Connections")))))))))

(defun profile-activity-html (id &key type right)
  (let* ((entity (db id))
         (strid (username-or-id id))
         (entity-type (getf entity :type))
         (*base-url* (case entity-type
                       (:person (strcat "/people/" strid))
                       (:group (strcat "/groups/" strid)))))
    (standard-page
      (getf entity :name)
      (html
        (when *user* (str (profile-tabs-html id :tab (or type :activity))))
        (when (and (eql type :request) (eql id *userid*) )
          (htm (str (simple-inventory-entry-html "a" "request"))))
        (when (and (eql type :offer) (eql id *userid*))
          (htm (str (simple-inventory-entry-html "an" "offer"))))
        (when (and (eql type :gratitude)
                   (not (eql id *userid*))
                   (eql (getf entity :active) t))
          (htm
            (:div :class "item"
             (:h4 "Do you have gratitude to share for " (str (getf entity :name)) "?")
             (:form :method "post" :action "/gratitude/new"
               (:input :type "hidden" :name "subject" :value id)
               (:input :type "hidden" :name "next" :value (strcat *base-url* "/reputation"))
               (:table :class "post"
                (:tr
                  (:td (:textarea :cols "1000" :rows "4" :name "text"))
                  (:td
                    (:button :class "yes" :type "submit" :class "submit" :name "create" "Post"))))))))

        (:div :class "activity"
          (str (profile-activity-items :id id :type type :page (aif (get-parameter "p") (parse-integer it) 0)))))

      :top (profile-top-html id)
      :right right
      :selected (case entity-type
                  (:person "people")
                  (:group "groups")))))
