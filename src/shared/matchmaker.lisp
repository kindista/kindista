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

(defun post-matchmaker (id)
  (let* ((id (parse-integer id))
         (item (db id))
         (by (getf item :by))
         (all-terms (post-parameter-words "match-all-terms"))

         (any-terms (post-parameter-words "match-any-terms"))
         (tags (post-parameter-string-list "match-tags"))
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
                            )
               ))
        (cond
          ((not (eql (getf item :type) :request))
           (flash "Matchmaker notifications are currently only available for requests" :error t)
           (see-other (referer)))

          ((nor any-terms all-terms)
           (try-again "Please enter at least 1 search term"))

          ((not tags)
           (try-again "Please check at least 1 tag"))
          
          ))
     )) 
  )
