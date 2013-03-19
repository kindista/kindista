(in-package :kindista)

(defvar *style-a* "color:#5c8a2f;
                   text-decoration:none;") 

(defvar *style-p* "margin-top:.9em;
                   margin-bottom:.9em;")

(defvar *style-quote-box* "border-collapse: collapse;
                           background: #ebf2e4;
                           margin: 8px;
                           border: thin solid #bac2b2;")

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun person-email-link (id)
  (html
    (:a :href (s+ "https://kindista.org/people/" (username-or-id id)) (str (getf (db id) :name)))))

(defun send-welcome-email (email token)
  (cl-smtp:send-email +mail-server+
                      "Kindista <noreply@kindista.org>"
                      email
                      "Welcome to Kindista!"
                      (welcome-email-text email token)
                      :html-message (welcome-email-html email token)))

(defun send-new-email (email token)
  (cl-smtp:send-email +mail-server+
                      "Kindista <noreply@kindista.org>"
                      email
                      "Verify your email address"
                      (new-email-text email token)
                      :html-message (new-email-html email token)))

(defun send-gratitude-notification-email (gratitude-id)
  (let* ((gratitude (db (parse-integer gratitude-id)))
         (from (getf gratitude :author))
         (to-list (getf gratitude :subjects)))
    (dolist (to to-list)
      (cl-smtp:send-email +mail-server+
                          "Kindista <noreply@kindista.org>"
                          (getf (db to) :email)
                          (s+ (getf (db from) :name) " has posted a statement of gratitude about you")
                          (gratitude-notification-email-text gratitude-id 
                                                             gratitude 
                                                             from)
                          :html-message (gratitude-notification-email-html gratitude-id gratitude from)))))

(defun send-gratitude-notification-test (gratitude-id)
  (let* ((gratitude (db (parse-integer gratitude-id)))
         (from (getf gratitude :author)))
      (cl-smtp:send-email +mail-server+
                          "Kindista <noreply@kindista.org>"
                          "benjamincrandall@gmail.com"
                          (s+ (getf (db from) :name) " has posted a statement of gratitude about you")
                          (gratitude-notification-email-text gratitude-id 
                                                             gratitude 
                                                             from)
                          :html-message (gratitude-notification-email-html gratitude-id gratitude from))))

(defun send-message-notification-email (from to)
  (cl-smtp:send-email +mail-server+
                      "Kindista <noreply@kindista.org>"
                      to
                      (s+ "New message from " from)
                      (message-notification-email-text from)
                      :html-message (message-notification-email-html from)))

(defun send-comment-mine-notification-email (from to)
  (cl-smtp:send-email +mail-server+
                      "Kindista <noreply@kindista.org>"
                      to
                      (s+ from " commented on your gift")
                      (comment-mine-notification-email-text from)
                      :html-message (comment-mine-notification-email-html from)))

(defun send-comment-other-notification-email (from to)
  (cl-smtp:send-email +mail-server+
                      "Kindista <noreply@kindista.org>"
                      to
                      (s+ from " commented on a gift you received")
                      (comment-other-notification-email-text from)
                      :html-message (comment-other-notification-email-html from)))

(defun send-circle-notification-email (from to)
  (cl-smtp:send-email +mail-server+
                      "Kindista <noreply@kindista.org>"
                      to
                      (s+ from " added you to their circles on Kindista!")
                      (circle-notification-email-text from)
                      :html-message (circle-notification-email-html from)))

