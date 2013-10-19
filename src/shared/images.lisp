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

(defun create-image (path content-type)
  (let* ((suffix (cond
                  ((string= content-type "image/jpeg")
                   "jpg")
                  ((string= content-type "image/png")
                   "png")
                  ((string= content-type "image/gif")
                   "gif")
                  (t
                   (error "~S is not a supported content type" content-type))))
         (image (insert-db `(:type :image
                             :content-type ,content-type)))
         (filename (strcat image "." suffix)))
   (copy-file path (merge-pathnames *original-images* filename))
   (modify-db image :filename filename)
   (values image)))

(defun new-image-form (action next &key class on)
  (html
    (:form :method "post"
           :name "imageform"
           :class (or class "submit-image item")
           :action action
           :enctype "multipart/form-data"
      (:input :type "hidden" :name "next" :value next)
      (when on (htm (:input :type "hidden" :name "on" :value on)))
      (:span "Add a photo:")
      (:input :type "file"
              :name "image"
              :onchange (ps-inline (submit-image-form)))
      (:div :id "spinner" :class "spinner"))))

(defun rotate-image (id)
  (let* ((image (db id))
         (original-file (strcat *original-images* (getf image :filename))))
    (run-program *convert-path*
                 (list original-file "-rotate"  "90" original-file))
    (dolist (path (directory (strcat *images-path* id "-*.*")))
      (delete-file path))))

(defun delete-image (id)
  (dolist (path (directory (strcat *images-path* id "-*.*")))
    (delete-file path))
  (delete-file (first (directory (strcat *original-images* id ".*"))))
  (remove-from-db id))

(defun get-image-thumbnail (id maxwidth maxheight &key (filetype "jpg"))
  (let* ((image (db id))
         (filename (format nil "~d-~d-~d.~a" id maxwidth maxheight filetype))
         (filepath (merge-pathnames *images-path* filename)))
    (assert image)
    (unless (file-exists-p filepath)
      (run-program *convert-path*
                   (list (strcat *original-images* (getf image :filename))
                         "-scale"
                         (strcat maxwidth "x" maxheight)
                         (native-namestring filepath))))
    (strcat *images-base* filename)))

(defun get-avatar-thumbnail (userid maxwidth maxheight &key (filetype "jpg"))
  (aif (db userid :avatar)
    (get-image-thumbnail it maxwidth maxheight :filetype filetype)
    *avatar-not-found*))

(defun convert-old-avatars ()
  (dolist (pathname (cl-fad:list-directory +avatar-path+))
    (when (equalp (pathname-type pathname) "jpg")
      (let ((id (handler-case (parse-integer (pathname-name pathname)) (t () nil))))
        (when id
          (let ((imageid (create-image pathname "image/jpeg")))
            (copy-file pathname (merge-pathnames *images-path* (strcat imageid "-300-300.jpg"))) 
            (modify-db id :avatar imageid)))))))

(defun item-images-html (item-id &key url)
  (let* ((item (db item-id))
         (images (getf item :images))
         (url (or url (script-name*)))
         (by (case (getf item :type)
               ((or :offer :request)
                (getf item :by))
               (:gratitude
                 (getf item :author))))
         (adminp (group-admin-p by)))
    (html
      (:div :class "activity images"
        (when (and (or (eql by *userid*) adminp)
                   (< (length images) 6))
          (htm (:div :class "post-image"
                 (str (new-image-form "/image/new" url :on item-id)))))
          (dolist (image-id images)
            (htm
              (:div :class "activity-image"
                (:img :src (get-image-thumbnail image-id 300 300))
                (when (or (eql *userid* by)
                          adminp
                          (getf *user* :admin))
                  (htm
                    (:form :method "post" :action (strcat "/image/" image-id)
                      (:input :type "hidden" :name "item-id" :value item-id)
                      (:input :type "hidden" :name "next" :value (script-name*))
                      (:button :class "simple-link green"
                               :type "submit"
                               :name "rotate-image"
                               "Rotate")
                      (:button :class "simple-link red"
                               :type "submit"
                               :name "delete-image"
                               "Delete")))))))))))

(defun post-new-image ()
  (require-active-user
    (let* ((item-id (parse-integer (post-parameter "on")))
           (item (db item-id))
           (image (post-parameter "image"))
           (url (post-parameter "next"))
           (by (case (getf item :type)
                 ((or :offer :request)
                  (getf item :by))
                 (:gratitude (getf item :author))))
           (adminp (group-admin-p by)))

      (require-test ((or (eql *userid* by)
                         adminp
                         (getf *user* :admin))
                    (s+ "You can only add photos to items you have posted."))
        (cond
          ((> (length (getf item :images)) 4)
           (flash "You have already posted the maximum of 5 images to this item.  Please delete one to add another." :error t)
           (see-other url))
          (t
            (flet ((modify-item-images (item-id &key edited)
                     (handler-case
                       ;; hunchentoot returns a list containing
                       ;; (path file-name content-type) when the
                       ;; post-parameter is a file, i.e. (first it) = path
                       (amodify-db item-id :images (cons (create-image (first image) (third image)) it)
                                           :edited (when edited edited))
                       (t () (flash "Please use a .jpg, .png, or .gif" :error t)))))
              (let ((now (get-universal-time)))
                (if (eql *userid* by)
                  (progn
                    (refresh-item-time-in-indexes item-id :time now)
                    (modify-item-images item-id :edited now))
                  (modify-item-images item-id))))
            (see-other url)))))))

(defun post-existing-image (id)
  (require-active-user
    (let* ((item-id (parse-integer (post-parameter "item-id")))
           (item (db item-id))
           (image-id (parse-integer id))
           (next (post-parameter "next"))
           (by (case (getf item :type)
                 ((or :offer :request)
                  (getf item :by))
                 (:gratitude (getf item :author))))
           (adminp (group-admin-p by)))

      (require-test ((or (eql *userid* by)
                         adminp
                         (getf *user* :admin))
                    (s+ "You can only add photos to items you have posted."))
        (cond
          ((post-parameter "rotate-image")
           (rotate-image image-id)
           (see-other (referer)))
          ((post-parameter "delete-image")
           (confirm-delete :url (strcat "/image/" image-id)
                           :type "picture"
                           :item-id item-id
                           :image-id image-id
                           :next-url (referer)))
          ((post-parameter "really-delete")
           (amodify-db item-id :images (remove image-id it))
           (delete-image image-id)
           (flash "Your picture has been deleted!")
           (see-other next)))))))
