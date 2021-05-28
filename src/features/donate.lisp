;;; Copyright 2012-2021 CommonGoods Network, Inc.
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

(defvar *donation-plans* (make-hash-table :synchronized t))

(defparameter *amazon-smile-link* "http://smile.amazon.com/ch/26-3664513")

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

(defun percent ()
  95/100)

(defun get-fundbar ()
  (setf (content-type*) "image/png")
  (let ((out (send-headers)))
    (vecto:with-canvas (:width 320 :height 34)
      (vecto:set-font (vecto:get-font "/usr/share/fonts/TTF/Ubuntu-B.ttf")
                      12)
      (vecto:set-rgb-fill 255/255 255/255 255/255)
      (vecto:draw-string 0 20 "$99,999/$999,999 monthly goal")
      (vecto:draw-string 245 20 "Donate Now")
      (vecto:set-rgb-fill 55/255 84/255 151/255)
      (vecto:set-rgb-fill 90/255 90/255 90/255)
      (vecto:set-rgb-stroke 130/255 130/255 130/255)
      (vecto:set-line-width 2)
      (vecto:rounded-rectangle 3 3 316 12 6 6)
      (vecto:fill-and-stroke)
      (vecto:rounded-rectangle 2 2 (* 320 (max 4/100 (percent))) 14 7 7)
      (vecto:set-rgb-fill 55/255 84/255 151/255)
      (vecto:set-rgb-stroke 96/255 119/255 171/255)
      (vecto:fill-and-stroke)
      (vecto:save-png-stream out))))

(defun donate-monthly-1 ()
  (html
    (:form :id "donate" :method "post" :action "/donate"
      (:a :href "/" (:img :src "/media/logo.png"))
      (:input :type "hidden" :name "type" :value "monthly")
      (:h2 "Sign up to make a monthly contribution")
      (:button :type "submit" :name "amount" :value "5" "$5")
      (:button :type "submit" :name "amount" :value "10" "$10")
      (:button :type "submit" :name "amount" :value "20" "$20")
      (:button :type "submit" :name "amount" :value "35" "$35")
      (:button :type "submit" :name "amount" :value "50" "$50")
      (:button :type "submit" :name "amount" :value "100" "$100")
      (:button :type "submit" :name "amount" :value "250" "$250")
      (:button :type "submit" :name "amount" :value "other" "Other")

      (:h3 (:a :href "/donate/once" "Or, make a one-time donation"))

      (:p "We do not store your credit card information, and we have a really good " (:a :href "/privacy" "privacy policy") ".")
     ; (:p "For information on other ways to donate, " (:a :href "/donate/more" "click here") ".")
      (:p "We also appreciate checks mailed to:"
           (:br)
           "Kindista"
           (:br)
           "C/O CommonGoods Network"
           (:br)
           "7929 SE 106th Avenue"
           (:br)
           "Portland, OR 97266")
      (:p "Please make sure to include your email address so we can send you a thank you!"))))

(defun donate-once-1 ()
  (html
    (:form :id "donate" :method "post" :action "/donate"
      (:a :href "/" (:img :src "/media/logo.png"))
      (:input :type "hidden" :name "type" :value "once")
      (:h2 "Make a one-time donation supporting Kindista")
      (:button :type "submit" :name "amount" :value "10" "$10")
      (:button :type "submit" :name "amount" :value "20" "$20")
      (:button :type "submit" :name "amount" :value "25" "$25")
      (:button :type "submit" :name "amount" :value "35" "$35")
      (:button :type "submit" :name "amount" :value "50" "$50")
      (:button :type "submit" :name "amount" :value "100" "$100")
      (:button :type "submit" :name "amount" :value "250" "$250")
      (:button :type "submit" :name "amount" :value "other" "Other")

      (when *user* (htm (:h3 (:a :href "/donate" "Or, make a monthly donation"))))

      (:p "We do not store your credit card information, and we have a really good " (:a :href "/privacy" "privacy policy") ".")
     ; (:img :src "/media/pbs.png" :width "119" :height "26") no such file in filesystem
     ; (:p "For information on other ways to donate, " (:a :href "/donate/more" "click here") ".")
      (:p "We also appreciate checks mailed to:"
           (:br)
           "Kindista"
           (:br)
           "C/O CommonGoods Network"
           (:br)
           "7929 SE 106th Avenue"
           (:br)
           "Portland, OR 97266")
      (:p "Please make sure to include your email address so we can send you a thank you!")   )))

(defun donate-dialog-2 (&optional show-error)
  (with-donate-info
    (html
      (:form :id "donate" :method "post" :action "/donate"
        (:a :href "/" (:img :src "/media/logo.png"))
        (:h2 "Step 2/4")
        (:h3 "Billing address")
        (:ul
          (:li :class "full"
            (:label :for "name"
                    :class (when (and show-error
                                      (empty-string-p (donate-info-name*)))
                                   "error")
                    "*Name on card")
            (:input :name "name" :type "text" :value (donate-info-name*)))
          (:li :class "full"
            (:label :for "address"
                    :class (when (and show-error
                                      (empty-string-p (donate-info-address*)))
                             "error")
                    "*Address")
            (:input :name "address" :type "text" :value (donate-info-address*)))
          (:li :class "half"
            (:label :for "city" "*City" 
                    :class (when (and show-error
                                      (empty-string-p (donate-info-city*)))
                             "error"))
            (:input :name "city" :type "text" :value (donate-info-city*)))
          (:li :class "quarter"
            (:label :for "state"
                    :class (when (and show-error
                                      (empty-string-p (donate-info-state*)))
                             "error")
                    "*State")
            (:select :name "state" (str (state-options (donate-info-state*)))))
          (:li :class "quarter"
            (:label :for "zip"
                    :class (when (and show-error
                                      (empty-string-p (donate-info-zip*)))
                             "error")
                    "*Zip")
            (:input :name "zip" :type "text" :value (donate-info-zip*)))
          (:li :class "half"
            (:label :for "email"
                    :class (when (and show-error
                                      (empty-string-p (donate-info-email*)))
                             "error")
                    "*Email")
            (:input :name "email" :type "text" :value (donate-info-email*)))
          (:li :class "half"
            (:label :for "phone"
                    :class (when (and show-error
                                      (empty-string-p (donate-info-phone*)))
                             "error")
                    "*Phone")
            (:input :name "phone" :type "text" :value (donate-info-phone*))))
        (:button :class "nav" :type "submit" "Next >")

        (:p "We do not store your credit card information, and we have a really good " (:a :href "/privacy" "privacy policy") ".")
     ; (:p "For information on other ways to donate, " (:a :href "/donate/more" "click here") ".")
      ))))

(defun stripe-tokenize (&key name address city state zip)
  (html
    (:script :type "text/javascript" :src "https://js.stripe.com/v1/")
    (:script :type "text/javascript"
      (str
        (ps
          (defvar *processing* nil)
          (defun tokenize (form)
            (unless *processing*
              (setf *processing* t)
              (dolist (element ((@ document get-elements-by-tag-name) "label"))
                ((@ element set-attribute) "class" ""))
              ((@ *stripe set-publishable-key) (ps:lisp *stripe-publish-key*))
              ((@ *stripe create-token)
               (parenscript:create
                 :number (@ ((@ document get-element-by-id) "ccn") value)
                 :cvc (@ ((@ document get-element-by-id) "cvc") value)
                 :exp_month (@ ((@ document get-element-by-id) "ccm") value)
                 :exp_year (@ ((@ document get-element-by-id) "ccy") value)
                 :name (ps:lisp (or name
                                    (donate-info-name*)))
                 :address_city (ps:lisp (or city
                                           (donate-info-city*)))
                 :address_line1 (ps:lisp (or address
                                             (donate-info-address*)))
                 :address_state (ps:lisp (or state
                                             (donate-info-state*)))
                 :address_zip (ps:lisp (or zip
                                           (donate-info-zip*))))
               (lambda (status response)
                 ((@ console log) response)
                 (cond
                   ((@ response :error)
                    (setf *processing* nil)
                    (cond
                      ((or (eq (@ response :error :code) "invalid_number")
                           (eq (@ response :error :code) "incorrect_number"))
                       ((@ ((@ document get-element-by-id) "lccn") set-attribute) "class" "error"))
                      ((eq (@ response :error :code) "invalid_cvc")
                       ((@ ((@ document get-element-by-id) "lcvc") set-attribute) "class" "error"))
                      ((eq (@ response :error :code) "card_declined")
                       (alert "Your card was declined by our payment processor.")
                       ((@ ((@ document get-element-by-id) "lccn") set-attribute) "class" "error"))
                      ((eq (@ response :error :code) "invalid_expiry_month")
                       ((@ ((@ document get-element-by-id) "lccm") set-attribute) "class" "error") 
                       ((@ ((@ document get-element-by-id) "lccy") set-attribute) "class" "error"))
                      ((eq (@ response :error :code) "invalid_expiry_year")
                       ((@ ((@ document get-element-by-id) "lccm") set-attribute) "class" "error")
                       ((@ ((@ document get-element-by-id) "lccy") set-attribute) "class" "error"))
                      (t (alert "unknown error!"))))
                   (t (setf (@ ((@ document get-element-by-id) "cctoken") value)
                            (@ response :id))
                      ((@ form submit)))))))
            f))))))

(defun credit-card-details-form (&key show-error card)
  (html
    (:li :class "full"
      (:label :for "ccn"
              :class (when (and show-error
                                (not card)
                                (empty-string-p (donate-info-token*)))
                       "error")
              :id "lccn"
              "*Card number")
      (:input :id "ccn" :type "text"))
    (:li :class "quarter"
      (:label :for "cvc"
              :class (when (and show-error
                                (not card)
                                (empty-string-p (donate-info-token*)))
                       "error")
              :id "lcvc"
              "*CVC " (:a :href "http://en.wikipedia.org/wiki/Card_security_code" :target "_blank" "(?)"))
      (:input :id "cvc" :type "text"))
    (:li :class "half"
      (:label :for "ccm"
              :class (when (and show-error
                                (not card)
                                (empty-string-p (donate-info-token*)))
                       "error")
              :id "lccm"
              "*Exp month")
      (:select :id "ccm"
        (:option :value "01" "01")
        (:option :value "02" "02")
        (:option :value "03" "03")
        (:option :value "04" "04")
        (:option :value "05" "05")
        (:option :value "06" "06")
        (:option :value "07" "07")
        (:option :value "08" "08")
        (:option :value "09" "09")
        (:option :value "10" "10")
        (:option :value "11" "11")
        (:option :value "12" "12")))
    (:li :class "quarter"
      (:label :for "ccy"
              :class (when (and show-error
                                (not card)
                                (empty-string-p (donate-info-token*)))
                       "error")
              :id "lccy"
              "*Exp year")
      (:select :id "ccy"
        (let ((current-year (current-year)))
          (loop for i from current-year to (+ current-year 10)
                do (htm (:option :value i (str i))))))) 

  (:p "We do not store your credit card information, and we have a really good " (:a :href "/privacy" "privacy policy") ".")
; (:p "For information on other ways to donate, " (:a :href "/donate/more" "click here") ".")
))

(defun donate-dialog-3 (&key show-error show-amount)
  (with-donate-info
    (html
      (str (stripe-tokenize))
      (:form :id "donate":method "post" :action "/donate" :onsubmit "return tokenize(this);"
        (:a :href "/" (:img :src "/media/logo.png"))
        (:h2 "Step 3/4")
        (:h3 "Credit card info")
        (:input :id "cctoken" :name "token" :type "hidden")
        (:ul
          (when (or (not (donate-info-amount*)) show-amount)
            (htm
              (:li :class "full"
                (:label :for "amount"
                        :class (when (and show-error
                                          (not (donate-info-amount*)))
                                 "error")
                        "*Donation amount")
                (:input :name "amount" :type "text" :value (donate-info-amount*))))) 
          (str (credit-card-details-form :show-error show-error))
          )
        (:button :id "ccnext" :class "nav" :type "submit" "Next >")
     ; (:p "For information on other ways to donate, " (:a :href "/donate/more" "click here") ".")
      ))))

(defun donate-dialog-4 ()
  (html
    (:form :id "donate" :method "post" :action "/donate"
      (:a :href "/" (:img :src "/media/logo.png"))
      (:h2 "Step 4/4")
      (:h3 "Confirm donation")

      (:p (:strong "Donation amount:") " $" (str (donate-info-amount*)))
      (:p (:strong "Donation type:") " " (str (donate-info-type*)))

      (:button :name "confirm" :class "nav" :type "submit" "Donate >")

      (:p "We do not store your credit card information, and we have a really good " (:a :href "/privacy" "privacy policy") ".")
     ; (:p "For information on other ways to donate, " (:a :href "/donate/more" "click here") ".")
      )))

(defun donate-page (dialog)
  (base-page
    "Donate"
    (html
      (str dialog))
    :class "donate"))


(defun get-donate ()
  (if *user*
    (base-page
      "Donate"
      (donate-page (donate-monthly-1)))
    (see-other "/donate/once")))


(defun post-donate ()
  (let ((amount (post-parameter "amount"))
        (type (post-parameter "type"))
        (name (post-parameter "name"))
        (address (post-parameter "address"))
        (city (post-parameter "city"))
        (state (post-parameter "state"))
        (zip (post-parameter "zip"))
        (email (post-parameter "email"))
        (phone (post-parameter "phone"))
        (token (post-parameter "token")) 
        (confirm (post-parameter "confirm")))

    (with-donate-info
        (cond
          ((and type
                (or (string= type "monthly")
                    (string= type "once")))

           (setf (donate-info-amount*)
                 (if (and amount (scan +number-scanner+ amount))
                   (parse-integer amount)
                   nil))

           (setf (donate-info-type*) type)
           (see-other "/donate/2"))

          ((and (or (string= (donate-info-type*) "monthly")
                    (string= (donate-info-type*) "once"))
                name
                address
                city
                state
                zip
                email
                phone)

           (setf (donate-info-name*) name)
           (setf (donate-info-address*) address)
           (setf (donate-info-city*) city)
           (setf (donate-info-state*) state)
           (setf (donate-info-zip*) zip)
           (setf (donate-info-email*) email)
           (setf (donate-info-phone*) phone)

           (if (and (not (string= name ""))
                    (not (string= address ""))
                    (not (string= city ""))
                    (not (string= state ""))
                    (scan +email-scanner+ email)
                    (scan +phone-scanner+ phone))
             (see-other "/donate/3")
             (donate-page (donate-dialog-2 t))))

          ((not (empty-string-p token))
            (unless (donate-info-amount*)
              (if (and amount (scan +number-scanner+ amount))
                (setf (donate-info-amount*) (parse-integer amount)) 
                (donate-page (donate-dialog-3 :show-error t :show-amount t))))
            (setf (donate-info-token*) token)
            (see-other "/donate/4"))

          ((and confirm
                (donate-info-type*)
                (donate-info-name*)
                (donate-info-address*)
                (donate-info-city*)
                (donate-info-state*)
                (donate-info-zip*)
                (donate-info-email*)
                (donate-info-phone*) 
                (donate-info-amount*) 
                (not (empty-string-p (donate-info-token*))))

           (handler-case
             (cond
               ((string= (donate-info-type*) "once")

                (stripe:create-charge :card (donate-info-token*)
                                      :amount (* 100 (donate-info-amount*))
                                      :currency "USD"
                                      :receipt-email (donate-info-email*)
                                      :description (strcat* "Donation from " (donate-info-email*)))
                (when *user* (modify-db *userid* :donated t))
                (flash "Thank you so much for your donation! You will receive email from us shortly.")
                (see-other "/"))

               ((string= (donate-info-type*) "monthly")
                (aif (getf *user* :custid)
                  (progn
                    (stripe:update-subscription it
                      :plan (make-donation-plan (* 100 (donate-info-amount*)))
                      :prorate nil
                      :card (donate-info-token*))
                    (when *user*
                      (modify-db *userid*
                                 :plan (donate-info-amount*)))
                    (flash "Thank you so much for your donation! You will receive email from us shortly.")
                    (see-other "/"))
                  (let ((customer (stripe:create-customer
                                    :card (donate-info-token*)
                                    :email (donate-info-email*)
                                    :plan (make-donation-plan (* 100 (donate-info-amount*))))))

                    (acond
                      ((stripe:sstruct-get customer :id)
                       (when *user*
                         (modify-db *userid*
                                    :donated t
                                    :custid it
                                    :phone phone
                                    :plan (donate-info-amount*)))
                       (flash "Thank you so much for your donation! You will receive email from us shortly.")
                       (see-other "/"))

                      (t (flash "An error occurred while processing your payment. Humans have been notified, and we'll be in touch shortly." :error t)))))))
             (stripe::stripe-error (err)
               (let ((code (stripe:sstruct-get (stripe::stripe-error-reply err) :error :code)))
                 (cond
                   ((string= code "card_declined")
                    "Your card was declined")

                   ((string= code "processing_error")
                    "Our payment processor encountered an error while processing your card.")

                   ((string= code "invalid_cvc")
                    "The CVC provided was incorrect.")

                   (t "An error occurred while processing your card."))))))

          (t (see-other "/donate"))))))

(defun get-donate-2 ()
  (with-donate-info
    (if (and (donate-info-type*))
      (base-page
        "Donate"
        (donate-page (donate-dialog-2)))
      (see-other "/donate"))))

(defun get-donate-3 ()
  (with-donate-info
    (if (and (donate-info-type*)
             (donate-info-name*)
             (donate-info-address*)
             (donate-info-city*)
             (donate-info-state*)
             (donate-info-zip*)
             (donate-info-email*)
             (donate-info-phone*))
      (base-page
        "Donate"
        (donate-page (donate-dialog-3)))
      (see-other "/donate"))))

(defun get-donate-4 ()
  (with-donate-info
    (if (and (donate-info-type*)
             (donate-info-name*)
             (donate-info-address*)
             (donate-info-city*)
             (donate-info-state*)
             (donate-info-zip*)
             (donate-info-email*)
             (donate-info-phone*) 
             (donate-info-amount*) 
             (not (empty-string-p (donate-info-token*))))
      (base-page
        "Donate"
        (donate-page (donate-dialog-4)))
      (see-other "/donate"))))

(defun get-donate-once ()
  (base-page
    "Donate"
    (donate-page (donate-once-1))))
