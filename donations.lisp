; Copyright 2013 CommonGoods Network, Inc.
;
; This file is part of Kindista.
;
; Kindista is free software: you can redistribute it and/or modify
; it under the terms of the GNU Affero General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; Kindista is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU Affero General Public License for more details.
;
; You should have received a copy of the GNU Affero General Public License
; along with Kindista.  If not, see <http://www.gnu.org/licenses/>.

(in-package :kindista)

(defvar *donation-plans* (make-hash-table :synchronized t))

(defun load-donation-plans ()
  (do* ((offset 0)
        (plans (stripe:sstruct-get
                 (stripe:list-plans :count 100)
                 :data)
               (stripe:sstruct-get
                 (stripe:list-plans :count 100 :offset (incf offset 100))
                 :data)))
       ((not plans))
       (dolist (plan plans)
         (setf (gethash (stripe:sstruct-get plan :amount) *donation-plans*) 
               (stripe:sstruct-get plan :id)))))

(defun make-donation-plan (amount)
  (aif (gethash amount *donation-plans*)
    it
    (let ((plan-id (format nil "donate-monthly-~d" amount)))
      (handler-case
        (let ((result (stripe:create-plan
                        :id plan-id
                        :amount amount
                        :currency "usd"
                        :interval "month"
                        :name (format nil "Monthly Donation: $~$" (/ amount 100)))))
          (acond
            ((stripe:sstruct-get result :id)
             (setf (gethash amount *donation-plans*) it))
            
            (t nil)))
        (stripe::stripe-error (err)
                              (cond
                                ((search "exists" (stripe:sstruct-get
                                                    (stripe::stripe-error-reply err)
                                                    :error :message))
                                 (setf (gethash amount *donation-plans*) plan-id))
                                (t (signal err))))))))

(defun update-donation-subscription (amount &key (user *user*))
  (awhen (getf user :custid)
    (stripe:update-subscription
      it
      :plan (make-donation-plan amount)
      :prorate :false)))

(defun cancel-donation-subscription (&optional (user *user*))
  (awhen (getf user :custid)
    (stripe:delete-subscription it)))
