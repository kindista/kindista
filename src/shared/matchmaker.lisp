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

(defun new-matching-offer-notice-handler ()
  (send-matching-offer-notification-email (getf (cddddr *notice*) :offer-id)
                                          (getf (cddddr *notice*) :request-id)))

(defstruct (matchmaker (:include result)
                       (:conc-name match-))
  distance all-terms any-terms without-terms notification)

(defun post-matchmaker (id)
  (let* ((id (parse-integer id))
         (item (db id))
         (prior-matchmaker (or (getf item :match-all-terms)
                               (getf item :match-any-terms)))
         (by (getf item :by))
         (all-terms (post-parameter-words "match-all-terms"))

         (any-terms (post-parameter-words "match-any-terms"))
         (without-terms (post-parameter-words "match-no-terms"))
         (notify (when (post-parameter-string "notify-matches") t))
         (raw-distance (post-parameter-integer "distance"))
         (distance (unless (equal raw-distance 0)
                     raw-distance))
         (base-url (strcat "/requests/" id))
         (hide-offer (post-parameter-integer "hide-match"))
         (match-url (url-compose base-url "selected" "matchmaker")))

    (if hide-offer
      (progn
        (hide-matching-offer id hide-offer)
        (see-other (or (post-parameter "next") (referer) "/home")))

      (require-test ((or (eql *userid* by)
                         (group-admin-p by)
                         (matchmaker-admin-p))
                     (s+ "You can only edit your own matchmaker notifications."))

        (flet ((try-again (e)
                 (flash e :error t)
                 (get-request id
                              :notify-matches (or notify
                                                  (getf item :notify-matches))
                              :all-terms (or all-terms
                                             (getf item :match-all-terms))
                              :any-terms (or any-terms
                                             (getf item :match-any-terms))
                              :without-terms (or without-terms
                                                 (getf item :match-no-terms))
                              :distance (or distance (getf item :match-distance)))))

          (cond
            ((not (eql (getf item :type) :request))
             (flash "Matchmaker notifications are currently only available for requests" :error t)
             (see-other (referer)))

            ((not (and (getf *user* :lat) (getf *user* :long)))
              (try-again "You need to enter your location on your <a href=\"/settings\">settings page</a> before you can create or edit matchmaker notifications."))

            ((post-parameter "edit-original")
             (enter-inventory-tags :title "Edit your request"
                                   :action base-url
                                   :item-title (getf item :title)
                                   :details (getf item :details)
                                   :tags (getf item :tags)
                                   :groups-selected (getf item :privacy)
                                   :restrictedp (getf item :privacy)
                                   :next match-url
                                   :existingp t
                                   :button-text "Save request"
                                   :selected "requests"))

            ((nor any-terms all-terms)
             ;; very important that all matchmakers have at least one of these.
             ;; indexing and searching depend on (or any-terms all-terms)
             (try-again "Please enter at least 1 search term you would like to be notified about"))

            (t
             (let ((new-matchmaker-data (modify-db id
                                                   :notify-matches notify
                                                   :match-all-terms all-terms
                                                   :match-any-terms any-terms
                                                   :match-no-terms without-terms
                                                   :match-distance distance)))
               (if prior-matchmaker
                 (modify-matchmaker id new-matchmaker-data)
                 (progn
                   (with-mutex (*requests-without-matchmakers-mutex*)
                     (asetf *requests-without-matchmakers-index*
                            (remove id it :key #'result-id)))
                   (index-matchmaker id new-matchmaker-data))))

             (update-matchmaker-request-data id)
             (see-other (or (post-parameter "next")
                            (url-compose (strcat "/requests/" id)))))))))))

(defun index-matchmaker (request-id &optional data)
  (flet ((stem-terms (terms)
           (remove-duplicates (mapcan #'stem-text terms) :test #'string=)))
    (let* ((data (or data (db request-id)))
           (by (getf data :by))
           (by-data (db by))
           (all-terms (getf data :match-all-terms))
           (any-terms (getf data :match-any-terms))
           (without-terms (getf data :match-no-terms))
           (tags (getf data :tags))
           (distance (getf data :match-distance))
           (matchmaker (make-matchmaker :id request-id
                                        :latitude (getf by-data :lat)
                                        :longitude (getf by-data :long)
                                        :distance distance
                                        :all-terms (stem-terms all-terms)
                                        :any-terms (stem-terms any-terms)
                                        :without-terms (stem-terms without-terms)
                                        :tags tags
                                        :notification (getf data :notify-matches)
                                        :privacy (getf data :privacy))))

      (when (or (getf data :match-all-terms)
                (getf data :match-any-terms))
        (with-locked-hash-table (*matchmaker-requests*)
          (setf (gethash request-id *matchmaker-requests*) matchmaker))

        (if distance
          (geo-index-insert *matchmaker-requests-geo-index* matchmaker)
          (with-mutex (*global-matchmaker-requests-mutex*)
            (push matchmaker *global-matchmaker-requests-index*)))

        ;;only on load-db; (eql :matching-offers nil) when matchmaker is created
        (awhen (getf data :matching-offers)
          (with-locked-hash-table (*offers-with-matching-requests-index*)
            (dolist (offer-id it)
              (push request-id
                    (gethash offer-id *offers-with-matching-requests-index*))))
          (with-locked-hash-table (*account-inventory-matches-index*)
            (dolist (offer-id it)
              (push (cons offer-id request-id)
                    (getf (gethash by *account-inventory-matches-index*) :offers)))))))))

(defun remove-matchmaker-from-indexes (request-id &key matchmaker data)
  (let ((data (or data (db request-id)))
        (matchmaker (or matchmaker (gethash request-id *matchmaker-requests*))))

    (if (getf data :match-distance)
      (geo-index-remove *matchmaker-requests-geo-index* matchmaker)
      (with-mutex (*global-matchmaker-requests-mutex*)
        (delete matchmaker *global-matchmaker-requests-index*)))

    (remhash request-id *matchmaker-requests*)))

(defun index-matching-requests-by-account ()
"Populates the :request property value of *account-inventory-matches-index* such that key=personid/groupid value=(:offers :requests). Should be run after *offers-with-matching-requests-index* is populated."
  (flet ((index-offer (offer-id matching-request-ids)
           (let ((by (db offer-id :by)))
             (dolist (request-id matching-request-ids)
               (push (cons request-id offer-id)
                     (getf (gethash by *account-inventory-matches-index*)
                           :requests))))))

    (with-locked-hash-table (*account-inventory-matches-index*)
      (maphash #'index-offer *offers-with-matching-requests-index*))))

(defun modify-matchmaker (request-id &optional data)
  (flet ((stem-terms (terms)
           (remove-duplicates (mapcan #'stem-text terms) :test #'string=)))

    (let* ((data (or data (db request-id)))
           (by (getf data :by))
           (by-data (db by))
           (all-terms (getf data :match-all-terms))
           (any-terms (getf data :match-any-terms))
           (without-terms (getf data :match-no-terms))
           (tags (getf data :tags))
           (distance (getf data :match-distance))
           (matchmaker (gethash request-id *matchmaker-requests*)))

      (setf (match-latitude matchmaker) (getf by-data :lat))
      (setf (match-longitude matchmaker) (getf by-data :long))
      (setf (match-distance matchmaker) distance)
      (setf (match-all-terms matchmaker) (stem-terms all-terms))
      (setf (match-any-terms matchmaker) (stem-terms any-terms))
      (setf (match-without-terms matchmaker) (stem-terms without-terms))
      (setf (match-tags matchmaker) tags)
      (setf (match-notification matchmaker) (getf data :notify-matches))
      (setf (match-privacy matchmaker) (getf data :privacy)))))

(defun account-matchmakers (&optional (account-id *userid*))
  (loop for id in (gethash account-id *request-index*)
        when (or (db id :match-all-terms)
                 (db id :match-any-terms))
        collect id))

(defun matchmaker-admin-p (&optional (userid *userid*))
  (let ((user (if (eql userid *userid*)
                (db userid)
                *user*)))
    (or (getf user :admin) (getf user :matchmaker))))

(defun find-matching-requests-for-offer (offer-id)
  (let* ((offer (db offer-id))
         (offer-result (gethash offer-id *db-results*))
         (by-id (getf offer :by))
         (by (db by-id))
         (lat (or (getf offer :lat) (getf by :lat)))
         (long (or (getf offer :long) (getf by :long)))
         (stems (stem-text (s+ (getf offer :title)
                               " "
                               (getf offer :details))))
         (tags (getf offer :tags)))

    (flet ((find-strings (fn request-data offer-data)
             (or (not request-data)
                 (funcall fn #'(lambda (string)
                                 (find string offer-data :test #'equalp))
                             request-data))))

      (loop for matchmaker
            in (append *global-matchmaker-requests-index*
                       (geo-index-query *matchmaker-requests-geo-index* lat long 100))
            when (and (find-strings #'some (match-tags matchmaker) tags)
                      (find-strings #'some (match-any-terms matchmaker) stems)
                      (find-strings #'every (match-all-terms matchmaker) stems)
                      (find-strings #'notany (match-without-terms matchmaker) stems)
                      (or (not (match-distance matchmaker))
                          (<= (air-distance lat
                                           long
                                           (match-latitude matchmaker)
                                           (match-longitude matchmaker))
                               (match-distance matchmaker)))
                      (not (item-view-denied (match-privacy matchmaker) by-id))
                      (not (item-view-denied (result-privacy offer-result)
                                             (db (match-id matchmaker) :by))))
          collect (result-id matchmaker)))))

(defun find-matching-offers-for-request (request-id &optional matchmaker)
  (let* ((matchmaker (or matchmaker
                         (gethash request-id *matchmaker-requests*)))
         (distance (match-distance matchmaker))
         (nearby-offers (awhen distance
                          (geo-index-query *offer-geo-index*
                                           (match-latitude matchmaker)
                                           (match-longitude matchmaker)
                                           it)))
         (all-terms (match-all-terms matchmaker))
         (any-terms (match-any-terms matchmaker))
         (offers-with-all-terms (awhen all-terms
                                  (all-terms-stem-index-query
                                    *offer-stem-index* it)))
         (offers-with-any-terms (awhen any-terms
                                  (any-terms-stem-index-query
                                    *offer-stem-index* it)))
         (offers-with-excluded-terms (awhen (match-without-terms matchmaker)
                                       (any-terms-stem-index-query
                                         *offer-stem-index* it)))
         (offers-matching-terms (set-difference
                                  (if (and all-terms any-terms)
                                    (result-id-intersection
                                      offers-with-all-terms
                                      offers-with-any-terms)
                                    (or offers-with-all-terms
                                        offers-with-any-terms))
                                  offers-with-excluded-terms
                                  :key #'result-id)))

    (loop for offer in (if distance
                         (result-id-intersection nearby-offers
                                                 offers-matching-terms)
                         offers-matching-terms)
          when (and (intersection (match-tags matchmaker) (result-tags offer)
                                  :test #'equalp)
                    (not (item-view-denied (result-privacy offer) *userid*))
                    (not (item-view-denied (match-privacy matchmaker)
                                           (db (result-id offer) :by))))
          collect (result-id offer))))

(defun hide-matching-offer (request-id offer-id)
  (let* ((request (db request-id))
         (offer (db offer-id))
         (requested-by (getf request :by))
         (offered-by (getf offer :by)))
    (if (or (find *userid* (list offered-by requested-by))
            (getf *user* :matchmaker))
      (progn
        (unindex-inventory-match request-id requested-by offer-id offered-by)
        (amodify-db request-id :hidden-matching-offers (pushnew offer-id it)
                               :matching-offers (remove offer-id it)))
      (permission-denied))))

(defun unindex-inventory-match (request-id requested-by offer-id offered-by)
  (with-locked-hash-table (*offers-with-matching-requests-index*)
    (asetf (gethash offer-id *offers-with-matching-requests-index*)
           (remove request-id it)))

  (with-locked-hash-table (*account-inventory-matches-index*)
    (asetf (getf (gethash offered-by *account-inventory-matches-index*)
                 :requests)
           (remove (cons request-id offer-id) it :test #'equal))
    (asetf (getf (gethash requested-by
                          *account-inventory-matches-index*)
                 :offers)
           (remove (cons offer-id request-id) it :test #'equal))))

(defun unmatch-request-matches (request-id requested-by offers-to-unmatch)
  (dolist (offer-id offers-to-unmatch)
    (let* ((offer (db offer-id))
           (offered-by (getf offer :by)))
      (unindex-inventory-match request-id requested-by offer-id offered-by))))

(defun update-matchmaker-request-data (request-id &key data matchmaker)
  (let* ((request (or data (db request-id)))
         (prior-matching-offers (getf request :matching-offers))
         (rejected-offers (getf request :hidden-matching-offers))
         (requested-by (getf request :by))
         (matchmaker (or matchmaker
                         (gethash request-id *matchmaker-requests*)))
         (all-old-matches (append prior-matching-offers rejected-offers))
         (new-matching-offers (find-matching-offers-for-request request-id
                                                                matchmaker))
         (new-useful-offers (set-difference new-matching-offers
                                            rejected-offers)))

    (unmatch-request-matches request-id
                             requested-by
                             (set-difference all-old-matches
                                             new-matching-offers))

    (dolist (offer-id (set-difference new-matching-offers
                                      all-old-matches))

      (let* ((offer (db offer-id))
             (offered-by (getf offer :by)))
        (with-locked-hash-table (*offers-with-matching-requests-index*)
          (push request-id
                (gethash offer-id *offers-with-matching-requests-index*)))

        (with-locked-hash-table (*account-inventory-matches-index*)
          (asetf (getf (gethash requested-by
                                *account-inventory-matches-index*)
                       :offers)
                 (pushnew (cons offer-id request-id) it :test #'equal))
          (asetf (getf (gethash offered-by
                                *account-inventory-matches-index*)
                       :requests)
                 (pushnew (cons request-id offer-id) it :test #'equal)))))

    (amodify-db request-id
                :matching-offers new-useful-offers)))

(defun unmatch-offer-matches (offer-id offered-by requests-to-unmatch)
  (dolist (request-id requests-to-unmatch)
    (let* ((request (db request-id))
           (requested-by (getf request :by)))
      (when (or (find offer-id (getf request :hidden-matching-offers))
                (find offer-id (getf request :matching-offers)))
        (amodify-db request-id :hidden-matching-offers (remove offer-id it)
                               :matching-offers (remove offer-id it))
        (unindex-inventory-match request-id requested-by offer-id offered-by)))))

(defun update-matchmaker-offer-data (offer-id)
  (let ((offered-by (db offer-id :by))
        (prior-matching-requests (copy-list (gethash offer-id *offers-with-matching-requests-index*)))
        (new-matching-requests (find-matching-requests-for-offer offer-id))
        (hidden-from-requests)
        (now (get-universal-time)))

    ;; remove offer from requests that no longer match
    (unmatch-offer-matches offer-id
                           offered-by
                           (set-difference prior-matching-requests
                                           new-matching-requests))

    ;; add offer to requests that now match
    (dolist (request-id (set-difference new-matching-requests
                                        prior-matching-requests))
      (let* ((request (db request-id))
             (requested-by (getf request :by))
             (matching-offers (copy-list (getf request :matching-offers))))
        (if (find offer-id (getf request :hidden-matching-offers))
          (push request-id hidden-from-requests)
          (unless (find offer-id matching-offers)
            (modify-db request-id :matching-offers (cons offer-id
                                                         matching-offers))

            (when (getf request :notify-matches)
              (notice :new-matching-offer :time now
                                          :offer-id offer-id
                                          :request-id request-id))

            (with-locked-hash-table (*account-inventory-matches-index*)
              (asetf (getf (gethash requested-by
                                    *account-inventory-matches-index*)
                           :offers)
                     (pushnew (cons offer-id request-id) it :test #'equal))
              (asetf (getf (gethash offered-by
                                    *account-inventory-matches-index*)
                           :requests)
                     (pushnew (cons request-id offer-id) it :test #'equal)))))))

    (with-locked-hash-table (*offers-with-matching-requests-index*)
      (setf (gethash offer-id *offers-with-matching-requests-index*)
            (set-difference new-matching-requests hidden-from-requests)))))

(defun item-matches-html (id &key data current-matches all-terms any-terms without-terms distance notify-matches)
  (let* ((data (or data (db id)))
         (item-type (getf data :type))
         (requestp (eql item-type :request))
         (notify-matches (or (getf data :notify-matches)
                             notify-matches))
         (self (eql (getf data :by) *userid*))
         (all-terms (or all-terms (getf data :match-all-terms)))
         (any-terms (or any-terms (getf data :match-any-terms)))
         (active-matchmaker (or all-terms any-terms))
         (without-terms (or without-terms (getf data :match-no-terms)))
         (pre-existing-distance (when (or (getf data :match-all-terms)
                                          (getf data :match-any-terms))
                                  (or (getf data :match-distance) 0)))
         (distance (or distance pre-existing-distance))
         (base-url (strcat "/" (if requestp "requests/" "offers/") id))
         (matchmaker-url (url-compose base-url "selected" "matchmaker"))
         (current-matches (or current-matches
                              (if requestp
                                (getf data :matching-offers)
                                (gethash id *offers-with-matching-requests-index*))))
         (tab (or (get-parameter "selected")
                  (if active-matchmaker "matches" "matchmaker"))))
    (html
      (:div :class "item-matches card"
        (when (and requestp active-matchmaker)
          (htm
            (:menu :type "toolbar" :class "bar"
              (if (equalp tab "matchmaker")
                (htm (:li :class "selected" "Matchmaker"))
                (htm
                  (:li (:a :href matchmaker-url "Matchmaker"))))
              (if (equalp tab "matches")
                (htm (:li :class "selected" "Matching Offers"))
                (htm
                  (:li (:a :href (url-compose base-url "selected" "matches")
                          "Matching Offers")))))))

        (cond
         ((or (string= tab "matches") (not requestp))
          (if current-matches
            (progn
              (unless requestp
                (htm
                  (:menu :type "toolbar" :class "bar"
                    (:li :class "selected" "Matching Requests"))))
              (dolist (item current-matches)
                (let ((result (gethash item *db-results*)))
                  (unless (item-view-denied (result-privacy result))
                    (htm
                      (:div :class "item"
                        (if requestp
                          (str (featured-offer-match-html (result-id result) id))
                          (str (featured-request-match-html (result-id result))))))))))
            (when requestp
              (htm
                (:strong
                  "There are currently no matching offers for the "
                  (:a :href matchmaker-url "search terms specified")
                  ". "
                  (str
                    (if (getf data :notify-matches)
                      "You will be notified by email whenever someone posts a new offer that matches your search terms."
                      "You have chosen not to be notified when someone posts a new offer that matches your search terms."))
                  " You can change your "
                  (:a :href matchmaker-url "notification preferences")
                  " for this request at any time.")))))

         ((string= tab "matchmaker")
          (htm
            (:p
              "Please fill out this form to be notified "
              "when someone posts an offer that matches the above request. "
              (:br)
              (:em "It is important that you separate multiple requests "
               "into separate posts so each request can be matched with "
               "with specific offers. "
               "If the item you have posted above includes multiple requests "
               (:strong "please ")
               "edit it and post each request separately."))
            (:form :method "post"
                   :action (strcat "/requests/" id "/matchmaker")
                   :class "matchmaker"

              (:h3 "Show me offers related to the above request that...")
              (:label
                "...contain " (:strong "ANY") " of these words:"
                (:br)
                (:input :type "text"
                        :name "match-any-terms"
                        :value (awhen any-terms
                                 (separate-with-spaces it))))
              (:br)
              (:label
                "...contain  " (:strong "ALL") " of these words:"
                (:br)
                (:input :type "text"
                        :name "match-all-terms"
                        :value (awhen all-terms
                                 (separate-with-spaces it))))
              (:br)
              (:label
                "...contain  " (:strong "NONE") " of these words:"
                (:br)
                (:input :type "text"
                        :name "match-no-terms"
                        :value (awhen without-terms
                                 (separate-with-spaces it))))
              (:br)
              (:label "...have any of these tags:"
                (:br)
                (:div :class ""
                 (str (display-tags "request" (getf data :tags)))

                 (:p (:strong "Note: ")
                  "To change which tags are matched, please "
                  (:button :type "submit"
                           :class "green inline-link"
                           :name "edit-original"
                    "edit your request")
                  " and change your keywords")))
               (:label :class "distance-selection"
                 "...are located within "
                 (str (distance-selection-dropdown (or distance
                                                       25))))
               (:br)
               (when self
                 (htm
                   (:input :type "checkbox"
                           :name "notify-matches"
                           :checked (when notify-matches "checked")
                   "Notify me by email when someone posts a matching offer")))
               (:br)
               (:div :class "float-right"
                 (:button :class "cancel" :type "submit" :name "cancel" "Cancel")
                 (:button :class "yes" :type "submit" :name "submit-matchmaker" "Save"))
              ))))))))

(defun matching-inventory-items-by-user (&optional (userid *userid*))
"Returns a list of ((:offers . ((:account-id :offer-id :request-id)...)) (:requests . ((:account-id :offer-id :request-id)...)).  :account-id is either the user's account or a group that the user is an admin of. Only returns :offers/:requests cons cell when there are :offers/:requests"
  (let ((accounts (cons userid (groups-with-user-as-admin userid)))
        (matching-offers (cons :offers (list nil)))
        (matching-requests (cons :requests (list nil))))
    (flet ((items-to-plist (account-id)
             (let ((items (gethash account-id
                                   *account-inventory-matches-index*)))
               ;; *account-inventory-matches-index* has format:
               ;; (match-item-id . my-item-id)
               (dolist (request (getf items :requests))
                 (push (list :account-id account-id
                             :request (car request)
                             :offer (cdr request))
                       (cadr matching-requests)))
                (dolist (offer (getf items :offers))
                 (push (list :account-id account-id
                             :offer (car offer)
                             :request (cdr offer))
                       (cadr matching-offers))))))
      (mapcar #'items-to-plist accounts))

    (remove nil
            (list (when (caadr matching-offers) matching-offers)
                  (when (caadr matching-requests) matching-requests)))))

(defun highlight-relevant-inventory-text (offer-id request-id)
  (let* ((matchmaker (gethash request-id *matchmaker-requests*))
         (offer (db offer-id))
         (offer-url (strcat "/offers/" offer-id))
         (stems (append (match-any-terms matchmaker)
                        (match-all-terms matchmaker))))
   (html
     (awhen (getf offer :title)
       (htm (:p :class "inventory-match-title"
              (:a :href offer-url (str (highlight-stems-in-text stems it))))))

     (awhen (getf offer :details)
       (htm (:p :class "inventory-match-text"
              (str (highlight-stems-in-text stems it))))))))

(defun featured-request-match-html
  (request-id
   &key offer-id
        request-data
        featured
   &aux (request (or request-data (db request-id)))
        (url (strcat "/requests/" request-id))
        (matchmaker (gethash request-id *matchmaker-requests*))
        (adminp (getf *user* :admin))
        (terms (union (getf request :match-all-terms)
                      (getf request :match-any-terms)))
        (by (getf request :by))
        (mine (or (eql *userid* by)
                  (find by (getf *user-group-privileges* :admin))))
        (reply-p (nor mine
                      ;; only necessary if we use this function to let admins
                      ;; see other user's matches
                       (and adminp
                            (item-view-denied
                              (match-privacy matchmaker)))))
        (tags (match-tags matchmaker)))

  (html
    (:div
      (:div :class (s+ (when (and featured (not mine)) "featured ")
                       "match-header")
       (if mine
         (htm
           (:p "...matches a "
            (:a :href url "request")
            " by "
            (str (person-link by))))
         (when (or offer-id featured)
           (htm
             (when featured
               (htm (:h3 "Will you respond to this request?...")))

             (when offer-id
               (htm
                 (:form :method "post"
                  :action (strcat "/requests/"
                                  request-id
                                  "/matchmaker")
                  :class "float-right"
                  (:button :type "submit"
                   :name "hide-match"
                   :class "red simple-link"
                   :value offer-id
                   "hide this request"))))))))

      (:div :class "match-details"
        (awhen (getf request :title)
          (htm (:p :class "inventory-match-title"
                 (:strong (:a :href url (str (html-text it))))))
        ;(when (and (getf request :title) (getf request :details))
        ;  (htm (:br) (:br)))
         (awhen (getf request :details)
           (htm (:p :class "inventory-match-text"
                  (str (html-text it))))))

        (:p :class "match-reason"
         (:span :class "tags"
          (:strong "tags:  ")
          (str (display-tags "request" tags))))

        (:p :class "match-reason"
         (:span :class "tags"
          (:strong "matchmaker:  ")
          (dolist (term terms)
            (htm (str term))
            (unless (eql term (car (last terms)))
              (htm " &middot ")))))

        (unless mine
          (htm (:p :class "other-party-details"
                 (:em "posted by "
                  (str (person-link by))
                  " "
                  (str
                    (humanize-universal-time (or (getf request :edited)
                                                 (getf request :created))))
                  " (within "
                  (str (distance-string
                         (with-location
                           (air-distance (match-latitude matchmaker)
                                         (match-longitude matchmaker)
                                         *latitude*
                                         *longitude*))))
                  ") "))))

       (:div :class "actions"
        (:form :method "post" :action url
         (:input :type "hidden" :name "next" :value (request-uri*))
         (when reply-p
           (htm (:input :type "hidden" :name "match" :value offer-id))
           (htm (:input :type "submit" :name "reply" :value "Reply")))
         (when mine
           (htm
             (:input :type "submit" :name "delete" :value "Delete")
             " &middot; ")))

        (when mine
          (htm
            (:a :href (url-compose url "selected" "matchmaker")
             " Edit matchmaker"))))))))

(defun featured-offer-match-html
  (offer-id
   request-id
   &key featured
   &aux (result (gethash offer-id *db-results*))
        (offer (db offer-id))
        (adminp (getf *user* :admin))
        (by (getf offer :by))
        (mine (or (eql *userid* by)
                  (find by (getf *user-group-privileges* :admin))))
        (reply (nor (and adminp
                         (item-view-denied (result-privacy result)))
                    mine))
        (url (strcat "/offers/" offer-id)))
  (html
    (:div
      (:div :class (s+ (when (and featured (not mine)) "featured ")
                       "match-header")
       (if mine
         (htm
           (:p "...matches an "
            (:a :href url "offer")
            " by "
            (str (person-link by))))
         (htm
           (when featured (htm (:h3 "Featured offer")))

           (:form :method "post"
            :action (strcat "/requests/" request-id "/matchmaker")
            :class "float-right"
            (:button :type "submit"
             :name "hide-match"
             :class "red simple-link"
             :value offer-id
             "hide this offer")))))

      (:div :class "match-details"
        (str (highlight-relevant-inventory-text offer-id request-id))

        (:p :class "match-reason"
          (:span :class "tags"
            (:strong "tags:  ")
            (str (display-tags "offer" (result-tags result)))))

       (unless mine
         (htm
           (:p :class "other-party-details"
             (:em "posted by "
              (str (person-link by)) " "
              (str (humanize-universal-time (result-time result)))
              " (within "
              (str (distance-string
                     (with-location
                       (air-distance (result-latitude result)
                                     (result-longitude result)
                                     *latitude*
                                     *longitude*))))
              ") "))))

       (:div :class "actions"
        (str (activity-icons :hearts (length (loves offer-id))
                             :url url))
        (:form :method "post" :action url
         (:input :type "hidden" :name "next" :value (request-uri*))
         (when reply
           (htm (:input :type "hidden" :name "match" :value request-id))
           (htm (:input :type "submit" :name "reply" :value "Reply")))
         (when (or adminp mine)
           (when reply (htm " &middot; "))
           (htm
             (:input :type "submit" :name "edit" :value "Edit")
             " &middot; "
             (if adminp
               (htm (:input :type "submit" :name "inappropriate-item" :value "Inappropriate"))
               (htm (:input :type "submit" :name "delete" :value "Delete")))))))))))

(defun matching-item-count-html (item-id item-type count &key admin)
  (html
    (:div :class "reciprocity matches"
     (:a :href (url-compose (strcat "/" item-type "s/" item-id)
                            "selected" "matches")
      (:img :alt "sharing:" :src "/media/icons/share.png")
      (:strong
        (str (if admin
               (s+ "There " (if (> count 1) "are " "is "))
               "You have "))
        (str count)
        " matching "
        (str (if (string= item-type "offer")
               "request"
               "offer"))

        (when (> count 1) (htm "s"))
        " for this "
        (str item-type))))))
