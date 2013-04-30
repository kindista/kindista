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

(defvar *style-a* "color:#5c8a2f;
                   text-decoration:none;")

(defvar *style-p* "margin-top:.9em;
                   margin-bottom:.9em;")

(defvar *style-quote-box* "border-collapse: collapse;
                           background: #ebf2e4;
                           margin: 8px;
                           border: thin solid #bac2b2;")

(defun person-email-link (id)
  (html
    (:a :href (strcat +base-url+ "people/" (username-or-id id)) (str (getf (db id) :name)))))

(defun person-name (id)
  (db id :name))

(defun html-email-base (content)
  (html
    (:html
      (:head
        (:style :type "text/css"
                      "a:hover {text-decoration:underline;} ")
        (:title "Kindista"))

      (:body :style "font-family: Ubuntu, Roboto, \"Segoe UI\", \"Helvetica Neue\", Tahoma, sans-serif;"
        (:table :cellspacing 0
                :cellpadding 0
                :style "border-collapse: collapse; width: 98%;"
                :border 0

          (:tr (:td :style "background: #fafafa;
                            border-bottom: 1px solid #eeeeee;
                            padding: 6px 6px 3px;"

                 (:a :href "http://kindista.org/"
                     :style "text-decoration: none;
                             color: #799f56;
                             font-size: 22px;
                             font-weight: 500;"
                     (:img :src "http://media.kindista.org/logo.png" :alt "kindista"))))

          (:tr (:td :style "padding: 10px;
                            color: #000000;
                            background: #ffffff;"
                 (str content))))))))

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

