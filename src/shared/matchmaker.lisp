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
         (pre-existing-matchmaker (getf item :notify-matches))
         (all-terms (post-parameter-words "match-all-terms"))

         (any-terms (post-parameter-words "match-any-terms"))
         (without-terms (post-parameter-words "match-no-terms"))
         (tags (post-parameter-string-list "match-tags"))
         (distance (post-parameter-integer "distance"))
        ;(url (url-compose (strcat "/requests/" id) "notify-matches" "on"))
         )
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
                            :distance (or distance (getf item :distance)))))
        (cond
          ((not (eql (getf item :type) :request))
           (flash "Matchmaker notifications are currently only available for requests" :error t)
           (see-other (referer)))

          ((not (and (getf *user* :lat) (getf *user* :long)))
            (try-again "You need to enter your location on your <a href=\"/settings\">settings page</a> before you can create matchmaker notifications."))

          ((nor any-terms all-terms)
           (try-again "Please enter at least 1 search term you would like to be notified about"))

          ((not tags)
           (try-again "Please check at least 1 tag"))

          (t

           (if pre-existing-matchmaker
             (modify-matchmaker id :data item
                                   :all-terms all-terms
                                   :any-terms any-terms
                                   :without-terms without-terms
                                   :tags tags
                                   :distance distance)
             (progn
               (index-matchmaker id
                                 (modify-db id
                                            :notify-matches t
                                            :match-all-terms all-terms
                                            :match-any-terms any-terms
                                            :match-no-terms without-terms
                                            :match-tags tags
                                            :match-distance distance))))

           (update-matching-inventory-data id)
           (see-other (or (post-parameter "next")
                          (url-compose (strcat "/requests/" id))))))))))

(defun index-matchmaker (request-id &optional data)
  (let* ((data (or data (db request-id)))
         (by (getf data :by))
         (by-data (db by))
         (all-terms (getf data :match-all-terms))
         (any-terms (getf data :match-any-terms))
         (without-terms (getf data :match-no-terms))
         (tags (getf data :match-tags))
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

    (with-locked-hash-table (*offers-with-matching-requests-index*)
      (dolist (offer-id (getf data :matching-offers))
        (push request-id
              (gethash offer-id *offers-with-matching-requests-index*))))))

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
          when (intersection (match-tags matchmaker)
                             (result-tags offer)
                             :test #'equalp)
          collect (result-id offer))))

(defun update-matching-inventory-data (request-id &key data matchmaker)
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

(defun modify-matchmaker (request-id &key data all-terms any-terms without-terms tags distance)
  (let* ((matchmaker (gethash request-id *matchmaker-requests*))
         (locationp (and (match-latitude matchmaker)
                         (match-longitude matchmaker)))
         (data (or data (db request-id)))
         )
  (when text
      (let* ((oldstems (stem-text (getf data :text)))
             (newstems (stem-text text))
             (common (intersection oldstems newstems :test #'string=)))

        (flet ((commonp (stem)
                 (member stem common :test #'string=)))

          (setf oldstems (delete-if #'commonp oldstems))
          (setf newstems (delete-if #'commonp newstems))

            (with-locked-hash-table (*matching-requests-stem-index*)
              (dolist (stem oldstems)
                (asetf (gethash stem *matching-requests-stem-index*)
                       (remove result it))))
              (dolist (stem newstems)
                (push result (gethash stem *matching-requests-stem-index*)))

          )))
    )
  )
