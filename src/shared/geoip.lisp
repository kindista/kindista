(in-package :kindista)

(defstruct geoip-data
  (city "" :type string)
  (latitude 0.0 :type float)
  (longitude 0.0 :type float))

(defstruct geoip-node
  (high nil :type geoip-node)
  (low nil :type geoip-node)
  (value nil :type geoip-data))

(defvar *geoip-tree* (make-geoip-node))

(defun dotted-quad-to-int (dotted-quad)
  (let ((bytes (mapcar #'parse-integer (split "\\." dotted-quad))))
    (+ (ash (first bytes) 24)
       (ash (second bytes) 16)
       (ash (third bytes) 8)
       (fourth bytes))))

(defun geoip-tree-insert (min max city latitude longitude)
  ; make a data node with the data
  ; create all nodes
  ;
  ; create node for min and node for max
  ; can depth-traverse from min to max
  ; not as fast

  (let ((root *geoip-tree*))
    (iter (for i from 0 to 31)
          ()
          
          )
    (iter (for i from 0 to 31)
          (let ((minbit (logbitp i min))
                (maxbit (logbitp i max)))
            (if (eq minbit maxbit)
              (if minbit
                (setf root (or (geoip-node-high root)
                               (setf (geoip-node-high root) (make-geoip-tree-node))))
                (setf root (or (geoip-tree-node-low root)
                               (setf (geoip-tree-node-low root) (make-geoip-tree-node)))))
            
            ))

        
        ))

  ; create all 32 levels and set value of min
  ; iterate from min to max
  ; identify common suffixes and trim redundant nodes
  ;
  ; 00000000000000000000000000010000 ?
  ;
  ; 00000000000000000000000000001000 *
  ;
  ; 00000000000000000000000000001001 *
  ; 0000000000000000000000000000101  *
  ; 000000000000000000000000000011   *
  ; 00000000000000000000000000010
  ; 000000000000000000000000000100   *
  ; 000000000000000000000000000101
  ; 0000000000000000000000000001010  *
  ;
  ; 00000000000000000000000001010101 *
  
  )

(defun geoip-query (dotted-quad)
  (let ((int (dotted-quad-to-int dotted-quad))
        (root *geoip-tree*))
    (iter (for i from 0 to 31)
          (aif (geoip-tree-node-value root)
            (leave it)
            (setf root (if (logbitp i int)
                         (geoip-tree-node-high root)
                         (geoip-tree-node-low root)))))))
