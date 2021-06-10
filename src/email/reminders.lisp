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

(defun send-reminder-email (userid title message)
  (let* ((data (db userid))
         (name (getf data :name))
         (email (first (getf data :emails)))
         (unsubscribe-key (getf data :unsubscribe-key))
         (text (s+ message
                   (unsubscribe-notice-ps-text
                     unsubscribe-key
                     email
                     "occasional email suggestions about how you can get the most out of Kindista"
                     :detailed-notification-description "these occasional suggestions" 
                     :unsub-type "reminders")))
         (html (s+ (html-email-base (nth-value 1 (markdown message :stream nil)))
                   (unsubscribe-notice-ps-html
                     unsubscribe-key
                     email
                     "occasional email suggestions about how you can get the most out of Kindista"
                     :detailed-notification-description "these occasional suggestions" 
                     :unsub-type "reminders"))))
    (when email
     (cl-smtp:send-email +mail-server+
                         "Kindista <info@kindista.org>"
                         (format nil "\"~A\" <~A>" name email)
                         title
                         text
                        :html-message html))))

(defun update-reminder-log (userid reminder-type)
  (amodify-db userid :activity-reminders (acons reminder-type
                                                (get-universal-time)
                                                (remove (assoc reminder-type it)
                                                        it))))

(defvar *last-reminder-email-time* 0)

(defun users-subscribed-to-notification (&optional notification-type)
  (let ((subscribed-users (list))
        (notification (or notification-type :notify-reminders)))
    (dolist (userid (remove-duplicates *active-people-index*))
      (let ((person (db userid)))
        (acase notification
          (:notify-reminders (when (or (getf person :notify-reminders)
                                       (getf person :notify-expired-invites))
            (push userid subscribed-users)))
          (:notify-inventory-digest
            (when (getf person :notify-inventory-digest)
              (push userid subscribed-users))))))
    (length subscribed-users)))

(defun get-send-all-reminders ()
  (when (or (getf *user* :admin)
            (server-side-request-p))
    (send-all-reminders)
    (see-other "/home")))

(defun send-all-reminders (&optional person-id)
  (declare (optimize (space 2) (speed 1) (debug 0) (safety 1)))
  (when
    (and (or *productionp* person-id)
             (< *last-reminder-email-time* (- (get-universal-time) 300)))
    (setf *last-reminder-email-time* (get-universal-time))
    (let ((complete-profile (read-file-into-string (s+ +markdown-path+ "reminders/complete-profile.md")))
          (closing (read-file-into-string (s+ +markdown-path+ "reminders/closing.md")))
          (first-gratitude (read-file-into-string (s+ +markdown-path+ "reminders/first-gratitude.md")))
          (first-invitations (read-file-into-string (s+ +markdown-path+ "reminders/first-invitations.md")))
          (first-offers (read-file-into-string (s+ +markdown-path+ "reminders/first-offers.md")))
          (first-requests (read-file-into-string (s+ +markdown-path+ "reminders/first-requests.md")))
          (minimal-activity (read-file-into-string (s+ +markdown-path+ "reminders/minimal-activity.md")))
          (more-gratitude (read-file-into-string (s+ +markdown-path+ "reminders/more-gratitude.md")))
          (more-invitees (read-file-into-string (s+ +markdown-path+ "reminders/more-invitees.md")))
          (more-offers (read-file-into-string (s+ +markdown-path+ "reminders/more-offers.md")))
          (more-requests (read-file-into-string (s+ +markdown-path+ "reminders/more-requests.md")))
          (no-avatar (read-file-into-string (s+ +markdown-path+ "reminders/no-avatar.md")))
          (offers-requests (read-file-into-string (s+ +markdown-path+ "reminders/offers-requests.md"))))

      (dolist (userid (or (awhen person-id (list it))
                          (remove-duplicates *active-people-index*)))
        (let* ((person (db userid))
               (name (getf person :name))
               (avatar (getf person :avatar))
               (greeting (format nil "Hi ~a,~c~c" name #\return #\linefeed))
               (reminders (getf person :activity-reminders))
               ;reminders is an assoc list where each entry is:
               ;  (reminder-type . time-reminder-was-sent)
               (invitee-count (invitee-count userid))
               (now (get-universal-time))
               (recent-reminder (first reminders))
               (recent-reminder-time (or (cdr recent-reminder) now))
               (recently-expired-invitations (recently-expired-invitations userid))
               (location (getf person :location))
               (activity (gethash userid *profile-activity-index*))
               (latest-gratitude (loop for result in activity
                                       when (and (eq (result-type result) :gratitude)
                                                 (= userid (first (result-people result))))
                                       return result))
               (latest-offer (find :offer activity :key #'result-type))
               (latest-request (find :request activity :key #'result-type)))

         (when (or (and (eql reminders nil)
                        ;first reminder after 1 day
                        (> (- now (getf person :created)) 86400))
                   ;remind people at most every 2 weeks
                   (> (- now recent-reminder-time) (* 2 +week-in-seconds+)))

           (if (and (getf person :notify-expired-invites)
                    (or (not (assoc :expired-invites reminders))
                        (> (- now
                              (cdr (assoc :expired-invites reminders)))
                           (* 4 +week-in-seconds+)))
                    recently-expired-invitations)
             ;;send notice to person that invitations they sent have expired
             (progn
               (let ((recently-expired-invite-emails (list)))
                 (dolist (invite recently-expired-invitations)
                   (let ((email (car invite))
                         (id (cdr invite)))
                      (asetf recently-expired-invite-emails (push email it))
                      (modify-db id :expired-notice-sent now)))
               (send-expired-invitations-reminder-email userid recently-expired-invite-emails)
               (update-reminder-log userid :expired-invites)))

             (when (getf person :notify-reminders)
               (cond

                 ; complete-profile
                 ((and (not location)
                       ;remind to finish profile at most every 3 weeks
                       (or (not (assoc :complete-profile reminders))
                           (> (- now (cdr (assoc :complete-profile reminders)))
                              (* 3 +week-in-seconds+))))
                    (send-reminder-email userid
                                         (s+ name ", please finish your Kindista profile.")
                                         (concatenate 'string greeting
                                                              complete-profile
                                                              first-invitations
                                                              first-offers
                                                              closing))
                    (update-reminder-log userid :complete-profile))

                 ; no-inventory
                 ((and (not latest-offer)
                       (not latest-request)
                           ;encourage people to add offers/requests twice a year
                       (or (not (assoc :no-inventory reminders))
                           (> (- now (cdr (assoc :no-inventory reminders)))
                              (* 13 +week-in-seconds+))))

                  (send-reminder-email userid
                                       "Getting started with offers and requests on Kindista"
                                       (concatenate 'string greeting
                                                            offers-requests
                                                            closing))
                  (update-reminder-log userid :no-inventory))

                 ; minimal-activity
                 ((and (or (not latest-offer)
                           (not latest-request)
                           (not latest-gratitude)
                           (not avatar)
                           (= invitee-count 0))
                       (or (not (assoc :minimal-activity reminders))
                           (> (- now (cdr (assoc :minimal-activity reminders)))
                              (* 5 +week-in-seconds+))))
                  (send-reminder-email
                    userid
                    "Making Kindista work for you"
                    (concatenate 'string greeting
                                         minimal-activity
                                         (or (unless latest-offer
                                                     first-offers)
                                             (unless latest-request
                                                     first-requests))
                                         (unless (> invitee-count
                                                    0)
                                           first-invitations)
                                         (unless latest-gratitude
                                                 first-gratitude)
                                         (unless avatar no-avatar)
                                         closing))
                    (update-reminder-log userid :minimal-activity))

                 ; more-gratitude
                 ((and latest-gratitude
                       (> (- now (result-time latest-gratitude)) (* 26 +week-in-seconds+))
                       (or (not (assoc :more-gratitude reminders))
                           (> (- now (cdr (assoc :more-gratitude reminders)))
                              (* 12 +week-in-seconds+))))
                  (send-reminder-email userid
                                       "Who are you grateful for these days?"
                                       (concatenate 'string greeting
                                                            more-gratitude
                                                            closing))
                  (update-reminder-log userid :more-gratitude))

                 ; more-offers
                 ((and latest-offer
                       (> (- now (result-time latest-offer)) (* 26 +week-in-seconds+))
                       (or (not (assoc :more-offers reminders))
                           (> (- now (cdr (assoc :more-offers reminders)))
                              (* 12 +week-in-seconds+))))
                  (send-reminder-email userid
                                       "Do you have anything new to offer on Kindista?"
                                       (concatenate 'string greeting
                                                            more-offers
                                                            closing))
                  (update-reminder-log userid :more-offers))

                 ; more-invitees
                 ((or (and (< invitee-count 10)
                           (or (not (assoc :more-invitees reminders))
                               (> (- now (cdr (assoc :more-invitees reminders)))
                                  (* 26 +week-in-seconds+))))
                      (and (or (< invitee-count 20)
                               (< (length (getf person :following)) 30))
                           (or (not (assoc :more-invitees reminders))
                               (> (- now (cdr (assoc :more-invitees reminders)))
                                  (* 52 +week-in-seconds+)))))
                  (send-reminder-email userid
                                       "Help Kindista grow by inviting more friends!"
                                       (concatenate 'string greeting
                                                            more-invitees
                                                            closing))
                  (update-reminder-log userid :more-invitees))

                 ; more-requests
                 ((and latest-request
                       (> (- now (result-time latest-request)) (* 26 +week-in-seconds+))
                       (or (not (assoc :more-requests reminders))
                           (> (- now (cdr (assoc :more-requests reminders)))
                              (* 12 +week-in-seconds+))))
                  (send-reminder-email userid
                                       "What could make your life easier or more enjoyable?"
                                       (concatenate 'string greeting
                                                            more-requests
                                                            closing))
                  (update-reminder-log userid :more-requests))
                 )))))))))
