(in-package :kindista)

(defun geocode-address (address)
  (let* ((results (first
                    (cdr
                      (assoc
                        :results
                        (json:decode-json-from-string
                          (octets-to-string
                            (http-request
                              "http://maps.googleapis.com/maps/api/geocode/json"
                              :parameters `(("address" . ,address)
                                            ("sensor" . "false")))
                            :external-format :utf-8))))))

         (location (cdr (assoc :location (cdr (assoc :geometry results)))))
         city
         country
         state)

    (iter (for component in (cdr (assoc :address--components results)))
          (until (and city state country))
          (let ((types (cdr (assoc :types component))))
            (cond
              ((member "country" types :test #'string=)
               (setf country (cdr (assoc :short--name component))))

              ((member "administrative_area_level_1" types :test #'string=)
               (setf state (cdr (assoc :short--name component))))

              ((member "locality" types :test #'string=)
               (setf city (cdr (assoc :long--name component)))))))


    (values (cdr (assoc :lat location))
            (cdr (assoc :lng location))
            (cdr (assoc :formatted--address results))
            city
            state
            country)))

(defun static-google-map (&key (size "400x400") lat long (marker t) (zoom 13) (scale 2))
  (strcat "<img src=\"http://maps.googleapis.com/maps/api/staticmap?size="
           size
           "&scale="
           scale
           "&maptype=roadmap&center="
           lat
           ","
           long
           "&sensor=false&zoom="
           zoom
           (if marker
             (strcat "&markers=color:0x90B04B|" lat "," long)
             "")
           "\">"))

