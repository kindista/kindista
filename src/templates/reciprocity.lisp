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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DISPLAY-GRATITUDE-RECIPROCITIES
;;
;; receives a "result" struct (of :type :gratitude)
;; and displays up to five small gray boxes, each with a random request
;; from one of the recipients who have requests.

(defun display-gratitude-reciprocities (gratitude-result)
  (let* ((details (retrieve-gratitude-reciprocity-details gratitude-result))
         (selected-subject-ids
           (mapcar #'(lambda (plist) (getf plist :userid)) details)))

    (when selected-subject-ids
      (html
        (:div :class "reciprocity"
          (:h3
            "Can you share with "
            (str (format nil *english-list-or*
                         (mapcar #'(lambda (x) (db x :name))
                           selected-subject-ids)))
            "?")
          (dolist (one-item details)
            (str (display-gratitude-reciprocity one-item))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RETRIEVE-GRATITUDE-RECIPROCITY-DETAILS
;;
;; - Whittles down the list of people to show request boxes
;;   for. Users with no requests are not shown; >5 boxes are not
;;   displayed.
;; - returns a list of plists in format
;;   (:id <userid> :req <random request> :more <their number of
;;   requests minus 1>)

(defun retrieve-gratitude-reciprocity-details (gratitude-result)
  (let ((subject-list (cdr (result-people gratitude-result))))
  (loop for i in subject-list
        with counter = 0
        until (= counter 5)
        for request-list = (gethash i *request-index*)
        for num-requests = (length request-list)
        unless (= i *userid*)
        when request-list
        do (incf counter)
        and
        collect (list :userid i
                      :requestid (nth (random num-requests) request-list)
                      :more (when (> num-requests 1)
                              num-requests)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DISPLAY-GRATITUDE-RECIPROCITY
;; displays a box with specified request from specified user,
;; a link to reply, and a link to more of their requests (if there are).

(defun display-gratitude-reciprocity (reciprocity-details)
  (let ((user-id (getf reciprocity-details :userid))
        (request-id (getf reciprocity-details :requestid))
        (more-requests (getf reciprocity-details :more)))
    (html
      (:div :class "recip-container"
        (:div :class "recip-header"
          (str (person-link user-id))
          " is requesting:")
        (:div :class "recip-details"
          (str (ellipsis (db request-id :text)
                         120
                         :see-more (strcat "/requests/" request-id)))
          (:div :class "recip-reply"
            (:a :href (strcat "/requests/" request-id "/reply") "Reply")
            (when more-requests
              (htm
                " &middot; "
                (:a :href (strcat "/people/" (username-or-id user-id) "/requests")
                 "See "
                 (str (if (= more-requests 1)
                        "one more request"
                        (strcat more-requests " more requests"))))))))))))
