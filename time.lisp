(in-package :kindista)


(let ((months (vector nil
                      "January"
                      "February"
                      "March"
                      "April"
                      "May"
                      "June"
                      "July"
                      "August"
                      "September"
                      "October"
                      "November"
                      "December")))

  (defun humanize-universal-time (then)
    (let* ((then-date (multiple-value-list (decode-universal-time then)))
           (now (get-universal-time))
           (now-date (multiple-value-list (decode-universal-time now))))
      (if (eql (sixth then-date) (sixth now-date))
        (let ((delta (- now then)))
          (cond
            ((< delta 60)
             "just now")
            ((< delta 120)
             "a minute ago")
            ((< delta 3300)
             (format nil "~a minutes ago" (round (/ delta 60))))
            ((< delta 5400)
             "about an hour ago")
            ((< delta 86400)
             (format nil "~a hours ago" (round (/ delta 3600))))
            ((< delta 172800)
             (format nil "yesterday at ~a:~a" (third then-date) (second then-date)))
            (t
             (format nil "~a ~a at ~a:~a" (elt months (fifth then-date))
                                          (fourth then-date)
                                          (third then-date)
                                          (second then-date)))))
        (format nil "~a ~a, ~a at ~a:~a" (elt months (fifth then-date))
                                         (fourth then-date)
                                         (sixth then-date)
                                         (third then-date)
                                         (second then-date))))))


