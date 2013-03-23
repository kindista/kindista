(in-package :kindista)

;(defstruct geoip-data
;  (city "" :type string)
;  (latitude 0.0 :type float)
;  (longitude 0.0 :type float))

;(defstruct geoip-node
;  (high nil :type geoip-node)
;  (low nil :type geoip-node)
;  (value nil :type geoip-data))

;(defvar *geoip-tree* (make-geoip-node))

(defun dotted-quad-to-int (dotted-quad)
  (let ((bytes (mapcar #'parse-integer (split "\\." dotted-quad))))
    (+ (ash (first bytes) 24)
       (ash (second bytes) 16)
       (ash (third bytes) 8)
       (fourth bytes))))

(defun index-geoip-tree (&key (in (pathname (s+ +db-path+ "GeoLiteCity-Blocks.csv")))
                              (out (pathname (s+ +db-path+ "geoip-tree"))))
  (let ((btree (b-tree:open out :type :string 
                                :minimum-degree 42
                                :if-does-not-exist :create)))

    (unwind-protect
      (cl-csv:read-csv in
                       :map-fn #'(lambda (row)
                                   (b-tree:insert btree
                                                  (second row)
                                                  (list (parse-integer (first row)) (parse-integer (third row))))))
      (b-tree:close btree))))

(defun search-geoip-tree (ip-addr &key (btree-path (s+ +db-path+ "geoip-tree")))
  (let ((btree (b-tree:open btree-path)))
    (b-tree::search>= btree ip-addr)
    (b-tree:close btree)))

;(defun geoip-query (dotted-quad)
;  (let ((int (dotted-quad-to-int dotted-quad))
;        (root *geoip-tree*))
;    (iter (for i from 0 to 31)
;          (aif (geoip-tree-node-value root)
;            (leave it)
;            (setf root (if (logbitp i int)
;                         (geoip-tree-node-high root)
;                         (geoip-tree-node-low root)))))))
