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

(defun person-card (id &key alias button suggestion-reasons remove)
  (let* ((mutuals (mutual-connections id))
         (person (db id))
         (name (getf person :name))
         (link (s+ "/people/" (username-or-id id)))
         (city (awhen (getf person :city) it)))

    (html

      (:div :class "person card"

       (awhen remove
         (htm
           (str it)))

       (:div :class "image"
        (:a :href link (:img :src (get-avatar-thumbnail id 100 100))))

        (awhen button
          (htm
            (:div :class "card-button"
              (str (v-align-middle (str it))))))

        (:div :class "card-details"
         (:h3 (:a :href link
               (str name)))


         (when alias
           (unless (string= name alias)
             (htm (:p "nickname: " (str alias)))))

         (aif suggestion-reasons (str it)

           ; below is "default" content
           (progn
             (awhen city (htm (:p "Lives in " (str it))))
             (when mutuals
               (htm (:p (:a :href (s+ link "/connections" )
                         (str (pluralize mutuals
                                         " mutual connection")))))))))))))

(defun suggested-contact-card (id suggestion)
  (html
    (:div :class "suggested-contacts"
      (str
        (person-card
          id
          :button
            (html
              (:form :method "post" :action "/contacts"
                (:input :type "hidden" :name "add" :value id)
                (:input :type "hidden" :name "next" :value "/people/suggested")
                (:button :class "yes small" :type "submit"
                 (str "Add contact"))))

          :remove
            (html
              (:form :method "post" :action "/people/suggested" :class "decline-button"
                (:input :type "hidden" :name "remove" :value id)
                (:input :type "hidden" :name "next" :value "people/suggested")
                (:button :class "simple-link gray-text" :type "submit"
                 "x")))

          :suggestion-reasons
            (html
              (let ((trimmed-reasons (remove-from-plist (cdr suggestion)
                                                        :ranking))
                    (city (awhen (getf (db id) :city) it))
                    (link (s+ "/people/" (username-or-id id))))

                 (when (length= 6 trimmed-reasons)
                   ; trimmed-reasons is a plist.
                   ; If its length is 6, that means there are mutual groups,
                   ; the person has added the user, and there are mutual
                   ; connections. Least relevant is :num-of-mutuals,
                   ; which will be removed.
                 (setf trimmed-reasons (remove-from-plist trimmed-reasons
                                                          :num-of-mutuals)))

                 (flet ((html-relevant-info (key value)
                       (cond
                         ((eql key :following-user-p)
                          (htm (:p "Added you to their contacts")))
                         ((eql key :mutual-groups)
                          (let ((random-group (rand-from-list value)))
                            (htm (:p "Also a member of "
                               (:a :href (s+ "/groups/"
                                             (username-or-id random-group))
                                (str (db random-group :name)))))))
                         ((eql key :num-of-mutuals)
                          (htm (:p (:a :href (strcat link "/connections")
                                      (str (pluralize value
                                                      " mutual connection")))))))))

                 (when (and (length= 2 trimmed-reasons ) city)
                  (htm (:p "Lives in " (str city))))

                 (loop for (key value) on trimmed-reasons
                      by #'cddr
                      do (html-relevant-info key value))))))))))
