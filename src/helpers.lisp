;;; Copyright 2012-2022 CommonGoods Network, Inc.
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

(defparameter +number-scanner+ (create-scanner "^\\d+$"))
(defparameter +full-name-scanner+ (create-scanner "^([a-zA-Z]+\\.? )[a-zA-Z]+"))
(defparameter +bot-scanner+ (create-scanner "(spider)|(bot)|(rackspace monitoring)|(monitoring.api.rackspacecloud)" :case-insensitive-mode t))

(defparameter +text-scanner+ (create-scanner "[a-zA-Z]+"))

;old-email-scanner "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,6}$"))
;;http://www.regular-expressions.info/email.html
(defparameter +email-scanner+ (create-scanner "^(?=[a-zA-Z0-9][a-zA-Z0-9@._%+-]{5,253}$)[a-zA-Z0-9._%+-]{1,64}@(?:(?=[a-zA-Z0-9-]{1,63}\\.)[a-zA-Z0-9]+(?:-[a-zA-Z0-9]+)*\\.){1,8}[a-zA-Z]{2,63}$"))
(defparameter *english-list*
  "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}")
(defparameter *english-list-or*
  "~{~#[~;~a~;~a or ~a~:;~@{~a~#[~;, or ~:;, ~]~}~]~}")
(defparameter +float-scanner+ (create-scanner "^-?(\\d+)\\.(\\d+)$"))
(defparameter +zip-scanner+ (create-scanner "^(\\d{5})((-)(\\d{4}))?$"))
(defparameter +phone-scanner+ (create-scanner
                                "(?:(?:\\+?1[\\(\\s]*(?:[.-]\\s*)?)?(?:(\\s*([2-9]1[02-9]|[2-9][02-8]1|[2-9][02-8][02-9])[\\)\\s]*)|([2-9]1[02-9]|[2-9][02-8]1|[2-9][02-8][02-9]))\\s*(?:[.-]\\s*)?)([2-9]1[02-9]|[2-9][02-9]1|[2-9][02-9]{2})\\s*(?:[.-]\\s*)?([0-9]{4})"))

(defparameter +date-scanner+ (create-scanner "^(0[1-9]|1[0-2]|[1-9])([/-])(0[1-9]|3[01]|[12][0-9]|[0-9])([/-])(19[0-9][0-9]|[2-9][0-9][0-9][0-9])$"))

(defparameter +time-scanner+ (create-scanner "^(0[1-9]|1[0-2]|[1-9])(:[0-5][0-9])? ?(a|A)|(p|P)\\.?(m|M)\\.? ?$"))

(defun strcat (&rest items)
  (format nil "~{~A~}" items))

(defun strcat* (&rest items)
  (format nil "~{~A~}" (remove nil items)))

(defmacro s+ (&rest strings)
  `(concatenate 'string ,@strings))

(defmacro html (&body body)
  (let ((sym (gensym)))
    `(with-html-output-to-string (,sym)
       ,@body)))

(defmacro asetf (place value)
  `(anaphora::symbolic setf ,place ,value))

(defun find-string (string list)
  (find string list :test #'string=))

(defmacro string-case (string &rest items)
  `(dolist (item ',items)
     (when (string= ,string (car item))
       (return (cadr item)))))

(defun safe-parse-integer (int?)
  (cond
    ((typep int? 'integer) int?)
    ((and (typep int? 'string)
          (scan +number-scanner+ int?))
     (parse-integer int?))))

(defun average (list)
  (/ (reduce #'+ list) (length list)))

(defun progress-bar (percent)
  (html
    (:div :class "progress-bar"
      (:span :style (strcat "width: " percent "%")))))

(defun page-title-bar (title)
  (html
    (:div :class "title-bar"
      (:h2 (str title)))))

(defun generate-js ()
  (paren-files:compile-script-file-to-js-file
    (s+ *kindista-root-path* "js/main.paren")
    :destination-file (s+ *kindista-root-path* "html/kindista.js")))

(defmacro bind-db-parameters ((item-type id parameters &optional binding-prefix result) &body body)
"Binds item-type to (db id) and supplied prefixed-parameters to (getf item-type parameter). When supplied result is bound to (gethash id *db-results*)."
  (let (bindings)
    (dolist (parameter parameters)
      (push (list (k-symbol (aif binding-prefix (strcat it "-" parameter)
                              parameter))
                  (list 'getf item-type (make-keyword parameter)))
            bindings))
    (when result
      (push `(,result (gethash ,id *db-results*)) bindings))
    (push `(,item-type (db ,id)) bindings)
    `(let* ,bindings ,@body)))

(defun safe-sort (sequence predicate &key key)
  (sort (copy-list sequence) predicate :key key))

(defun validate-name (string)
  (scan +full-name-scanner+ string))

(defun validate-email (string)
  (scan +email-scanner+ string))

(defun find-nil-ids ()
  (loop for id from 0 to *db-top*
        when (null (db id))
        collect id))

(defun fsync (stream)
  (finish-output stream)
  (sb-posix:fsync (sb-posix:file-descriptor stream)))

(defmacro with-file-lock ((path &key interval) &body body)
  "Get an exclusive lock on a file. If lock cannot be obtained, keep
   trying after waiting a while"
  (let ((lock-path (gensym))
        (lock-file (gensym)))
    `(let ((,lock-path (format nil "~a.lock" (namestring ,path))))
       (unwind-protect
         (progn
           (loop
             :for ,lock-file = (open ,lock-path :direction :output
                                     :if-exists nil
                                     :if-does-not-exist :create)
             :until ,lock-file
             :do (sleep ,(or interval 0.1))
             :finally (close ,lock-file))
           ,@body)
         (ignore-errors
           (delete-file ,lock-path))))))

(defun sublist (list &optional start count)
  (when start
    (setf list (nthcdr start list)))
  (let ((length (length list)))
    (cond
      ((or (not count)
           (>= count length))
       (values list nil))
      (t
       (values (subseq list 0 count) t)))))

(defun intersection-fourth (list1 list2)
  (intersection list1 list2 :key #'fourth))

(defun string-intersection (list1 list2)
  (intersection list1 list2 :test #'string=))

(defun string-to-keyword (string)
  (make-keyword (string-upcase (substitute #\- #\_ string))))

(defun remove-nil-plist-pairs (plist)
  (let (new-list)
    (doplist (key value plist)
      (when value (nconcf new-list (list key value))))
    new-list))

(defun k-symbol (string)
  (intern (string-upcase string) :kindista))

(defun symbol= (symbol-a symbol-b)
  (equalp (symbol-name symbol-a)
          (symbol-name symbol-b)))

(defun hyphenate (string)
  (ppcre:regex-replace-all " "
                           (remove-if #'(lambda (char)
                                          (find char
                                                '("," "." "!" "?" "(" ")" "'")
                                                :test #'string=))
                                      (string-downcase string))
                           "-"))

(defun words-from-string (string-to-split)
  (when string-to-split
    (iter (for word in (split " " (ppcre:regex-replace-all "[\\r\\n,<|>]"
                                                           (string-downcase string-to-split)
                                                           " ")))
          (when (ppcre:scan +text-scanner+ word)
            (collect word)))))

(defun word-count (string)
  (length (words-from-string string)))

(defun emails-from-string (string)
  (iter (for email in (split " " (ppcre:regex-replace-all "[\\r\\n,<|>]" (string-downcase string) " ")))
        (when (ppcre:scan +email-scanner+ email)
          (collect email))))

(defun decode-json-octets (octets &key (external-format :utf-8))
  (json:decode-json-from-string
    (octets-to-string octets :external-format external-format)))

(defun mailinate-user-emails (&key (accounts-to-omit (list 1)) groups-to-omit)
  "For use in development environment only. Gives all users a mailinator email address for testing functionality and to prevent emails from being sent to users by mistake."
  (unless *productionp*
    (let ((group-members (flatten (mapcar #'(lambda (id) (gethash id *group-members-index*))
                                          groups-to-omit))))
      (dolist (id *active-people-index*)
        (unless (or (find id accounts-to-omit)
                    (find id group-members))
          (let ((user (db id)))
            (amodify-db id
                        :emails (cons (s+ "k-"
                                          (ppcre:regex-replace-all
                                            " "
                                            (string-downcase (getf user :name))
                                            "-" )
                                          "@mailinator.com")
                                      it))))))))

(defun separate-with-commas (list &key omit-spaces)
  (format nil (if omit-spaces "~{~A,~}" "~{~A, ~}") list))

(defun remove-whitespace-around-string (string)
  (string-trim
    '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout)
    string))

(defun separate-with-spaces (list)
  (format nil "~{~A ~}" list))

(defun item-view-denied (result-privacy &optional (userid *userid*))
  (and result-privacy
       (nor (find userid result-privacy)
            ;; userid can be a group in some cases
            (find userid
                 (apply #'append
                        (mapcar #'group-members result-privacy))))))

(defun remove-private-items (items)
  (remove-if #'item-view-denied items :key #'result-privacy))

(defun love-component (loves)
   (* (+ (log loves) 0.1) 60000)
   )

(defun activity-rank
  (result
   &key (userid *userid*)
        (user *user*)
        (contact-multiplier 1)
        (distance-multiplier 1)
        (now (get-universal-time))
        (sitewide)
   &aux (age (max (- now
                     (or (result-time result) 0))
                  1))
        (refresh-offset (or (when (and (result-created result)
                                       (not (eq (result-created result)
                                                (result-time result))))
                              (- 0
                                 (* 9 (+ 1 (log (+ 1
                                                   (/ (- now
                                                         (result-created result))
                                                      10000)))))))
                            0))
        (contacts (getf user :following))
        (lat (or (getf user :lat) *latitude*))
        (long (or (getf user :long) *longitude*))
        (contact-p (intersection contacts (result-people result)))
        (self-offset (if (eql (car (result-people result)) userid)
                       ;; don't use "=" because userid can be nil
                       -50
                       0))
        (time-component (/ 1000 (+ 1 (log (+ 1 (/ age 300000))))))
        (distance (unless sitewide
                    (if (and (result-latitude result)
                             (result-longitude result))
                           (max 0.3
                                (air-distance lat
                                              long
                                              (result-latitude result)
                                              (result-longitude result)))
                         5000)))
        (distance-component (unless sitewide
                              (/ (* 200 distance-multiplier)
                                 (log (+ 6 distance)))))
        (contact-component (if contact-p
                             (* 18 contact-multiplier)
                             0))
        (love-component (aif (loves (result-id result))
                          (* (log (* 1.4 (length it)))
                             9)
                          0)))
  "Higher scores rank higher."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (values (round (apply #'+ (remove nil
                                    (list self-offset
                                          refresh-offset
                                          time-component
                                          distance-component
                                          contact-component
                                          love-component
                                          ))))

          (list :distance-component distance-component
                :self-offset self-offset
                :refresh-offset refresh-offset
                :time-component time-component
                :contact-component contact-component
                :love-component love-component
                :age (humanize-universal-time (result-time result))
                )))

(defun event-rank
  (result
   &aux (contacts (getf *user* :following))
        (currentness (abs (- (or (result-time result) 0)
                             (get-universal-time))))
        (distance (air-distance *latitude*
                                *longitude*
                                (result-latitude result)
                                (result-longitude result))))
  (round (- currentness
            (/ 120000
               (log (+ (if (intersection contacts (result-people result))
                           1
                           distance)
                       4)))
            (* (length (loves (result-id result))) 60000))))

(defun inventory-item-rank
  (result
   &aux (age (- (get-universal-time) (or (result-time result) 0)))
        (loves (max 1 (length (loves (result-id result))))))
   (* (/ 50
         (log (+ (/ age 86400)
                 6)))
      (expt loves 0.15)))

(defun inventory-rank
  (alist)
"Takes an a-list of ((request . (whether the request had matching terms in the :title, :details, and/or :tags))...)  and returns a ranked list of results"

  (flet ((rank (item)
           (+ (inventory-item-rank (car item))
              (if (find :title (cdr item)) 25 0)
              (if (find :tags (cdr item)) 8 0))))

    (mapcar #'car (sort alist #'> :key #'rank))))

(defun refresh-item-time-in-indexes
  (id
   &key (time (get-universal-time))
        ;; get-inventory-refresh is called by server not client
        server-side-trigger-p)

  (let* ((result (gethash id *db-results*))
         (type (result-type result))
         (item (db id))
         (by (case type
               ((or :offer :request)
                (getf item :by))
               (:gratitude (getf item :author))))
         (group-adminp (member *userid* (db by :admins))))

    (when (and (or (eql *userid* by) group-adminp server-side-trigger-p)
               (not (db by :test-user))
               (or (eq type :gratitude)
                   (eq type :offer)
                   (eq type :request)))

      (setf (result-time result) time)

      (when (or (eql type :offer)
                (eql type :request))
        (with-mutex (*inventory-refresh-timer-mutex*)
          (setf *inventory-refresh-timer-index*
                (safe-sort (push result *inventory-refresh-timer-index*)
                           #'<
                           :key #'result-time))))

      (with-locked-hash-table (*profile-activity-index*)
        (asetf (gethash by *profile-activity-index*)
               (safe-sort it #'> :key #'result-time)))

      (unless (< (result-time result) (- (get-universal-time) 15552000))
        (unless (< (result-time result) (- (get-universal-time) 2592000))
          (with-mutex (*recent-activity-mutex*)
            (setf *recent-activity-index*
                  (safe-sort (pushnew result *recent-activity-index*)
                             #'> :key #'result-time))))
        (geo-index-insert *activity-geo-index* result)))))

(defun url-parts (url)
  (iter (for part in (split " " (ppcre:regex-replace-all "/" url " ")))
        (collect part)))

(defun resource-url
  (resource-id
   &optional (resource (db resource-id))
             (omit-leading-forward-slash nil)
   &aux
             (id (case (getf resource :type)
                   ((or :person :group)
                    (username-or-id resource-id))
                   (t resource-id))))

  (strcat* (unless omit-leading-forward-slash "/")
           (case (getf resource :type)
           (:offer "offers")
           (:request "requests")
           (:event "events")
           (:conversation "conversations")
           (:transaction "transactions")
           (:gratitude "gratitude")
           (:image "image")
           (:person "people")
           (:group "groups")
           (:gift "gifts"))
           "/"
           id))

(defun url-compose (base &rest params)
  (do ((param-strings ()))
      ((not params)
       (cond
         ((not param-strings )
          base)
         ((and (find #\? base :test #'equal)
               (find #\= base :test #'equal))
          (format nil "~a&~{~a~^&~}" base
                                         param-strings))
         (t
          (format nil "~a~a~{~a~^&~}" base
                                      (if param-strings "?" "")
                                      param-strings))))
      (when (cadr params)
        (push (if (consp (cadr params))
                (format nil "~a=~{~a~^+~}" (car params) (cadr params))
                (format nil "~a=~a" (car params) (cadr params)))
              param-strings))
      (setf params (cddr params))))

(defun ellipsis (text &key (length 160) see-more plain-text email)
  (let ((newtext (subseq text 0 (min (length text) length))))
    (if (> (length text) length)
      (if plain-text
        (s+ newtext "...")
        (html
          (str (if email
                 newtext
                 (html-text newtext)))
          "..."
          (when see-more
            (htm (:a :href see-more
                     :style (when email
                              "color: #5c8a2f; font-weight: bold; text-decoration: none;"
                              )
                  " see more")))))
      (if plain-text
        newtext
        (html-text newtext)))))

(defun beginning-html-paragraphs
  (html-text
    &key (count 2)
    &aux (shorten-p (scan (strcat "(<p>.*?</p>){" (+ count 1) "}")
                          html-text)))

  (values (if shorten-p
            (scan-to-strings
              (create-scanner (strcat "(<p>.*?</p>){0," count "}"))
              html-text)
            html-text)
          (when shorten-p t)))

(defun html-text (string)
  (if string
    (regex-replace-all "\\n" (escape-for-html string) "<br>")
    ""))

(defun distance-string (miles)
  (let ((distance (/ (round miles 0.5) 2)))
    (cond
      ((<= distance 1/2)
       "1/2 mile")
      ((eql distance 1)
       "1 mile")
      ((typep distance 'ratio)
       (format nil "~1$ miles" (coerce distance 'float)))
      (t
       (format nil "~d miles" distance)))))

(defvar *state-options*
  (html
    (dolist (state '("AL" "AK" "AZ" "AR" "CA" "CO" "CT" "DE" "DC" "FL" "GA" "HI" "ID" "IL" "IN" "IA" "KS" "KY" "LA" "ME" "MD" "MA" "MI" "MN" "MS" "MO" "MT" "NE" "NV" "NH" "NJ" "NM" "NY" "NC" "ND" "OH" "OK" "OR" "PA" "RI" "SC" "SD" "TN" "TX" "UT" "VT" "VA" "WA" "WV" "WI" "WY"))
      (htm
        (:option :value state (str state))))))

(defun state-options (&optional selected)
  (html
    (dolist (state '("AL" "AK" "AZ" "AR" "CA" "CO" "CT" "DE" "DC" "FL" "GA" "HI" "ID" "IL" "IN" "IA" "KS" "KY" "LA" "ME" "MD" "MA" "MI" "MN" "MS" "MO" "MT" "NE" "NV" "NH" "NJ" "NM" "NY" "NC" "ND" "OH" "OK" "OR" "PA" "RI" "SC" "SD" "TN" "TX" "UT" "VT" "VA" "WA" "WV" "WI" "WY"))
      (htm
        (:option :value state :selected (if (equalp selected state) "selected" nil) (str state))))))

(defun cons-assoc (cell a-list)
  (assoc cell a-list :test #'equalp))

(defun cons-to-string (cell)
  (strcat (car cell) (aif (cdr cell) (strcat "." it) "")))

(defun list-list (data)
  (list (list data)))

(defun assoc-assoc (id a-list)
  (assoc (assoc id (mapcar #'car a-list)) a-list))

(defun nor (&rest items)
"Returns true if none of the items are true."
 (notany #'identity items))

(defun or-string= (string test-strings)
"Returns 'string' if it is a member of test-strings"
  (find string test-strings :test #'string=))

(defun parse-cons (string)
"Returns a cons cell from a string. Integers are parsed, other elements returned as strings. ex. '6' -> (6), '6.5' -> (6 . 5), '2.string' -> (2 . 'string')"
  (loop for i = 0 then (1+ j)
        as j = (position #\. string :start i)
        with current = nil
        do (setf current (subseq string i j))
        if (scan +number-scanner+ current)
        collect (parse-integer current) into ids
        else collect current into ids
        while (and j (< (length ids) 3))
        finally (return (awhen (car ids) (cons it (cadr ids))))))

(defun empty-string-p (string)
  (or (not string) (string= string "")))

(defun mutual-connections (other-person-id &optional (user-id *userid*))
  "A user is considered a \"mutual connection\" between the current user
  (user) and another user (other-person) if:
     - current user follows mutual connection
     - mutual connection follows other-person
   A user is considered a mutual connection between the current user and a group if they:
     - current user follows mutual connection
     - mutual connection is in the group"
  (let ((other-person-data (db other-person-id)))
   (intersection (if (eql (getf other-person-data :type) :person)
                  (gethash other-person-id *followers-index*)
                  (remove *userid*
                          (union (getf other-person-data :admins)
                                 (getf other-person-data :members))))
                (getf (db user-id) :following))))

(defmacro ensuring-userid ((user-id base-url) &body body)
  (let ((is-number (gensym))
        (user-name (gensym))
        (user-data (gensym))
        (merged-into (gensym)))
    `(let ((,is-number (scan +number-scanner+ ,user-id)))
       (if ,is-number
         (let* ((,user-id (parse-integer ,user-id))
                (,user-data (db ,user-id))
                (,user-name (getf ,user-data :username))
                (,merged-into (getf ,user-data :merged-into)))
           (cond
            ((not ,user-data)
             (not-found))

            (,user-name
             (see-other (apply #'url-compose
                               (format nil ,base-url ,user-name)
                               (flatten (get-parameters*)))))
            (,merged-into (apply #'url-compose
                               (format nil ,base-url ,merged-into)
                               (flatten (get-parameters*))))
            (t (progn ,@body))))
         (let ((,user-id (gethash ,user-id *username-index*)))
           (if ,user-id
             (progn ,@body)
             (not-found)))))))

(defun confirmed-location (&optional (id *userid*))
  (let* ((data (if (eq id *userid*) *user* (db id)))
         (location-details (getf data :location))
         (lat (getf data :lat))
         (long (getf data :long)))
    (when (and location-details lat long)
      (values location-details lat long))))

(defun username-or-id (&optional (id *userid*))
  (or (getf (db id) :username)
      (write-to-string id)))

(defun alpha-people-links (userid-list)
  (mapcar
    #'cdr
    (sort
      (iter (for id in (copy-list userid-list))
            (let* ((name (db id :name))
                   (link (html (:a :href (strcat "/people/" (username-or-id id))
                                         (str name)))))
              (collect (cons name link))))
     #'string-lessp :key #'car)))

(defun person-link (id &key possessive)
  (let ((entity (db id)))
    (html
      (:a :href (s+ (if (or (eql (getf entity :type) :person)
                            (eql (getf entity :type) :deleted-person-account))
                      "/people/"
                      "/groups/")
                    (username-or-id id))
          (str (getf entity :name))
          (when possessive (htm "'s "))))))

(defun group-link (id)
  (html
    (:a :href (s+ "/groups/" (username-or-id id)) (str (getf (db id) :name)))))

(defun name-list (ids &key (func #'person-link) (maximum-links 3) (links t))
  (let* ((name-count (length ids))
         (count-displayed (cond
                            ((= name-count (+ maximum-links 1))
                             (- name-count 2))
                            ((> name-count maximum-links)
                             maximum-links)
                            (t name-count)))
         (display-ids (subseq ids 0 count-displayed))
         (others (when (> name-count count-displayed)
                   (strcat (- name-count count-displayed) " others"))))
    (flet ((format-function (id)
             (if links (apply func (list id)) (db id :name))))
     (format nil
            *english-list*
            (aif others
              (append (mapcar #'format-function display-ids ) (list it))
              (mapcar #'format-function display-ids))))))

(defun name-list-all (ids &key stringp (conjunction :and))
  (format nil (case conjunction
                (:and *english-list*)
                (t *english-list-or*))
          (if stringp
            (loop for id in ids collect (db id :name))
            (mapcar #'person-link ids))))


(defun humanize-number (n)
  (let ((ones (cadr (multiple-value-list (floor n 10)))))
    (strcat n (cond
                ((= ones 1) "st")
                ((= ones 2) "nd")
                ((= ones 3) "rd")
                (t  "th")))))

(defun contact-opt-out-flash (id-list &key (userid *userid*) (item-type "message"))
  (let ((people-opt-outs)
        (group-opt-outs))
    (dolist (id id-list)
      (let ((entity (db id)))
        (when (or (not (getf entity :notify-message))
                  ;; it's possible to use FB to sign up w/out an email
                  (and (eql (getf entity :type) :person)
                       (not (car (getf entity :emails)))))
          (if (eql (getf entity :type) :person)
            (push id people-opt-outs)
            (push id group-opt-outs)))))
    (let* ((my-group-opt-outs (intersection group-opt-outs
                                           (mapcar #'car (groups-with-user-as-admin userid))))
           (self-opt-out (member userid people-opt-outs))
           (other-opt-outs (remove userid people-opt-outs))
           (other-group-opt-outs (set-difference group-opt-outs my-group-opt-outs)))
      (cond
        (my-group-opt-outs
         (flash (s+ "<p>" (name-list-all my-group-opt-outs)
                    (pluralize my-group-opt-outs " does " :plural-form " do " :hidenum t)
                    "not have any admins who have chosen to be notified when "
                    "people send messages to them through Kindista.</p>"
                    "<p>In order to be notified when someone replies "
                    "to this " item-type
                    " you must change your "
                    "<a href=\"/settings/communication\">"
                    "communication settings</a>.</p>")  :error t))
        (self-opt-out
          (flash (s+ "<p>You have chosen not to be receive email"
                     " notifiactions when people send "
                     "messages to you through Kindista.</p>"
                     "<p>In order to be notified when someone replies "
                     "to this " item-type
                     " you must change your "
                     "<a href=\"/settings/communication\">"
                     "communication settings</a>.</p>") :error t))
        (other-opt-outs
          (flash (s+ "<p>"
                     (name-list-all other-opt-outs)
                     (pluralize other-opt-outs " has " :plural-form " have " :hidenum t)
                     "chosen not to receive email notifications when other "
                     " Kindista members send them messages.</p>"
                     "<p>They will receive your message next time they log into "
                     "Kindista. "
                     "If this is an urgent matter, please use other means to " 
                     "contact them.</p>")
                 :error t))
        (other-group-opt-outs
          (flash (s+ "<p>The admins for "

                     (name-list-all other-group-opt-outs)
                     " have chosen not to receive email notifications when"
                     " other Kindista members send them messages.</p>"
                     "<p>They will receive your message next time they log into "
                     "Kindista. "
                     "If this is an urgent matter, please use other means to "
                     "contact them.</p>")
                 :error t))))))

(defun pending-flash (action)
  (flash (s+ "Your account hasn't been fully activated yet. "
             "If you haven't already done so, please post a couple offers. "
             "After we review your initial activity you will be able to "
             action ".")
         :error t))

(defun pending-disclaimer (&optional type)
  (when (getf *user* :pending)
    (html
      (:p :class "err"
        (:em (:strong "Please note: ")
          "This "
          (str (aif type it "item"))
          " will be displayed on Kindista after we have a chance to review "
          "your account. "
          "Posting your first offer shows us that you understand our "
          (:a :href "/terms" "Terms of Use")
          " and that you intend to be a contributing member "
          "of our community. "
          (unless (string= type "offer")
            (htm
              (:strong "You won't be able to send messages to other Kindista "
               "members until after we have a chance to review your first offer.")))))
      (:br))))

(defparameter *integrity-reminder*
  "Please don't be flaky. Clarity and integrity are essential for the wellbeing of our community. Be respectful of others and their time. Honor your word whenever possible. If plans change, notify the other party as soon as possible.")

(defmacro v-align-middle (content)
  `(html
     (:div :class "v-align-container"
       (:div :class "v-align-cell"
         ,content))))

(defun post-parameter-integer-list (name)
  (loop for pair in (post-parameters*)
        for i = (parse-integer (cdr pair) :junk-allowed t)
        when (and (string= (car pair) name) i)
        collect i))

(defun post-parameter-string-list (name &optional fn)
  (loop for pair in (post-parameters*)
        for s = (cdr pair)
        unless (string= s "")
        when (and (string= (car pair) name)
                  (if fn
                    (funcall fn s)
                    t))
        collect s))

(defun post-parameter-string (name)
  (awhen (post-parameter name) (unless (string= it "") it)))

(defun post-parameter-words (name)
  (awhen (post-parameter name) (unless (string= it "") (words-from-string it))))

(defun get-parameter-string (name)
  (awhen (get-parameter name) (unless (string= it "") it)))

(defun get-parameter-integer (name)
  (when (scan +number-scanner+ (get-parameter name))
    (parse-integer (get-parameter name))))

(defun get-parameter-integer-list (name)
  (remove nil
          (loop for pair in (get-parameters*)
                for i = (parse-integer (cdr pair) :junk-allowed t)
                when (and (string= (car pair) name) i)
                collect i)))

(defun post-parameter-float (name)
  (awhen (post-parameter name) (when (scan +float-scanner+ it) (read-from-string it))))

(defun possessive-name
  (owner-id
   &key (userid *userid*)
        linkp)
  (cond
    ((= owner-id userid) "your")
    (linkp (person-link owner-id :possessive t))
    (t (s+ (db owner-id :name) "'s"))))

(defun post-parameter-integer (name)
  (when (scan +number-scanner+ (post-parameter name))
    (parse-integer (post-parameter name))))

(defun rand-from-list (list)
  (when list
    (nth (random (length list)) list)))

(defun pluralize (list-or-num singular &key plural-form hidenum)
  "If the first argument is 1 (or its length is 1), returns the number and the second argument (the non-plural form of the word). If it is non-1, returns the number and the plural form of the word. If plural-form is non-nil (must be a string), returns that word instead of adding an s. if hidenum is non-nil, only returns the pluralized word without the number."
  (let ((num (if (integerp list-or-num) list-or-num (length list-or-num))))
    (s+
      (unless hidenum (strcat num " "))
      (if (= num 1)
        singular
        (aif plural-form it (strcat singular "s"))))))

(defun highlight-stems-in-text (list-of-stems text)
  (let* ((text (copy-seq text))
         (words (remove-duplicates
                  (split " "
                         (ppcre:regex-replace-all
                           *multispace-scanner*
                           (ppcre:regex-replace-all *nonword-scanner*
                                                    text
                                                    " ")
                           " "))
                  :test #'string=)))

     (dolist (word words)
       (when (find (stem word) list-of-stems :test #'equalp)
         (setf text
               (regex-replace-all word
                                  text
                                  (html (:span :class "highlight" (str word)))))))
    text))

