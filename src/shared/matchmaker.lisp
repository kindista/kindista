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

(defstruct (matchmaker (:include result)
                       (:conc-name match-))
  distance all-terms any-terms without-terms)

(defun post-matchmaker (id)
  (let* ((id (parse-integer id))
         (item (db id))
         (by (getf item :by))
         (all-terms (post-parameter-words "match-all-terms"))

         (any-terms (post-parameter-words "match-any-terms"))
         (without-terms (post-parameter-words "match-no-terms"))
         (raw-distance (post-parameter-integer "distance"))
         (distance (unless (equal raw-distance 0)
                     raw-distance))
         (base-url (strcat "/requests/" id))
         (match-url (url-compose base-url "notify-matches" "on")))

    (require-test((or (eql *userid* by)
                       (group-admin-p by)
                       (getf *user* :admin))
                  (s+ "You can only edit your own matchmaker notifications."))
      (flet ((try-again (e)
               (flash e :error t)
               (get-request id
                            :notify-matches t
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
            (try-again "You need to enter your location on your <a href=\"/settings\">settings page</a> before you can create matchmaker notifications."))

          ((post-parameter "edit-original")
           (enter-inventory-tags :title "Edit your request"
                                 :action base-url
                                 :text (getf item :text)
                                 :tags (getf item :tags)
                                 :groups-selected (getf item :privacy)
                                 :restrictedp (getf item :privacy)
                                 :next match-url
                                 :existingp t
                                 :button-text "Save request"
                                 :selected "requests"))

          ((nor any-terms all-terms)
           (try-again "Please enter at least 1 search term you would like to be notified about"))

          (t

           (let ((new-matchmaker-data (modify-db id
                                                 :notify-matches t
                                                 :match-all-terms all-terms
                                                 :match-any-terms any-terms
                                                 :match-no-terms without-terms
                                                 :match-distance distance)))
             (if (getf item :notify-matches)
               new-matchmaker-data
               (index-matchmaker id new-matchmaker-data)))

           (update-matchmaker-request-data id)
           (see-other (or (post-parameter "next")
                          (url-compose (strcat "/requests/" id))))))))))

(defun index-matchmaker (request-id &optional data)
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
                                      :all-terms all-terms
                                      :any-terms any-terms
                                      :without-terms without-terms
                                      :tags tags
                                      :privacy (getf data :privacy))))

    (with-locked-hash-table (*matchmaker-requests*)
      (setf (gethash request-id *matchmaker-requests*) matchmaker))

    (if distance
      (geo-index-insert *matchmaker-requests-geo-index* matchmaker)
      (with-mutex (*global-matchmaker-requests-mutex*)
        (push matchmaker *global-matchmaker-requests-index*)))

    ;;only on load-db; (eql :matching-offers nil) when matchmaker is created
    (with-locked-hash-table (*offers-with-matching-requests-index*)
      (dolist (offer-id (getf data :matching-offers))
        (push request-id
              (gethash offer-id *offers-with-matching-requests-index*))))))

(defun find-matching-requests-for-offer (offer-id)
  (let* ((offer (db offer-id))
         (offer-result (gethash offer-id *db-results*))
         (by-id (getf offer :by))
         (by (db by-id))
         (lat (or (getf offer :lat) (getf by :lat)))
         (long (or (getf offer :long) (getf by :long)))
         (stems (stem-text (getf offer :text)))
         (tags (getf offer :tags)))

    (flet ((find-strings (fn request-data offer-data)
             (or (not request-data)
                 (funcall fn #'(lambda (string)
                                 (find string offer-data) :test #'equalp)
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
                                  :key #'match-id)))

    (loop for offer in (if distance
                         (result-id-intersection nearby-offers
                                                 offers-matching-terms)
                         offers-matching-terms)
          when (and (intersection (match-tags matchmaker) (result-tags offer)
                                  :test #'equalp)
                    (not (item-view-denied (result-privacy offer) *userid*))
                    (not (item-view-denied (match-privacy matchmaker)
                                           (db (result-id offer) :by))))
          collect offer)))

(defun update-matchmaker-request-data (request-id &key data matchmaker)
  (let* ((request (or data (db request-id)))
         (matchmaker (or matchmaker
                         (gethash request-id *matchmaker-requests*)))
         (current-matching-offers (db request :matching-offers))
         (rejected-offers (db request :hidden-matching-offers))
         (all-old-matches (append current-matching-offers rejected-offers))
         (new-matching-offers (find-matching-offers-for-request request-id
                                                                matchmaker))
         (new-useful-offers (set-difference new-matching-offers
                                            rejected-offers)))

    (with-locked-hash-table (*offers-with-matching-requests-index*)
      (dolist (offer-id (set-difference all-old-matches
                                        new-matching-offers))
        (asetf (gethash offer-id *offers-with-matching-requests-index*)
               (remove request-id it)))
      (dolist (offer-id (set-difference new-matching-offers
                                        all-old-matches))
        (push request-id
              (gethash offer-id *offers-with-matching-requests-index*))))

    (amodify-db request-id
                :matching-offers new-useful-offers
                :hidden-matching-offers (intersection rejected-offers
                                                      new-matching-offers))))

