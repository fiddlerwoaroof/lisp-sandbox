(uiop:define-package :fwoar.zipfile
  (:mix :cl :fwoar.lisputils)
  (:export ))
(in-package :fwoar.zipfile)

(defun read-bytes (n s)
  (with (seq (make-array n :element-type 'serapeum:octet))
    (values seq
            (read-sequence seq s))))

(defun calculate-sizes (desc)
  (reduce #'+ desc
          :key #'cadr
          :initial-value 0))

(defun le->int (bytes)
  (cadr
   (reduce (op (destructuring-bind (count val) _1
                 (list (1+ count)
                       (+ val
                          (ash _2
                               (* count 8))))))
           bytes
           :initial-value (list 0 0))))

(defun get-extractable-bytes (desc &optional (bindings ()))
  (loop for ((name size . other) . rest) on (resolve-sizes desc bindings)
     until (symbolp size)
     collect (list* name size other) into extractable
     finally (return (values extractable
                             (append (serapeum:unsplice
                                      (when (symbolp size)
                                        (list* name size other)))
                                     rest))))) 

(defun resolve-sizes (desc extant-bindings)
  (declare (optimize (debug 3)))
  (loop with bindings = (copy-seq extant-bindings)
     for (name size . rest) in desc
     for resolved = (when (symbolp size)
                      (cdr (assoc size bindings)))
     when resolved do (push (cons name resolved)
                            bindings)
     if resolved collect (list* name resolved rest) into new-desc
     else collect (list* name size rest) into new-desc
     finally (return (values new-desc
                             (remove-duplicates (append (mapcar (op (apply #'cons (subseq _ 0 2)))
                                                                new-desc)
                                                        bindings)
                                                :key 'car
                                                :from-end t)))))

(defun extract-bytes (desc bytes)
  (loop
     with cur-idx = 0
     for (name size . rest) in desc
     for next-seq = (subseq bytes cur-idx
                            (+ cur-idx size))
     collect (cons name (if rest
                            (funcall (car rest) next-seq)
                            next-seq))
     do (incf cur-idx size)))

(defun parse-struct (desc s)
  (let* ((struct-size (calculate-sizes desc))
         (bytes (read-bytes struct-size s)))
    (extract-bytes desc bytes)))

(defun make-zipfile-stream (fn)
  (open fn :element-type '(unsigned-byte 8)))


(defun extract (raw-desc s &optional bindings)
  (multiple-value-bind (desc remainder) (get-extractable-bytes raw-desc bindings)
    (let ((next-segment (parse-struct desc s)))
      (if remainder
          (append next-segment
                  (extract remainder s (append next-segment bindings)))
          next-segment))))

(defparameter *zip-local-file-header*
  '((signature 4)
    (version 2)
    (flags 2)
    (compression 2 le->int)
    (mod-time 2)
    (mod-date 2)
    (crc-32 4)
    (compressed-size 4 le->int)
    (uncompressed-size 4 le->int)
    (file-name-length 2 le->int)
    (extra-field-length 2 le->int)
    (file-name file-name-length babel:octets-to-string)
    (extra-field extra-field-length)))

(defun decode-file-data (metadata s)
  (let ((crc-32 (le->int (cdr (assoc 'crc-32 metadata))))
        (compressed-size (cdr (assoc 'compressed-size metadata)))
        (uncompressed-size (cdr (assoc 'uncompressed-size metadata))))
    (when (= 0 (+ crc-32 compressed-size uncompressed-size))
      (error "bad zipfile: I don't support data descriptors yet..."))
    (format t "~&COMPRESSED-SIZE: ~a~%" compressed-size)
    (let ((compressed-data (read-bytes compressed-size s)))
      (format t "~&...~%")
      (values (serapeum:ecase-let (compression (cdr (assoc 'compression metadata)))
                (0 compressed-data)
                (8 (princ "decompress")
                   (chipz:decompress nil (chipz:make-dstate 'chipz:deflate) compressed-data))
                (t (error "unsupported compression ~a" compression)))
              metadata))))

(defun decode-a-file-if-name (pred s)
  (let ((metadata (extract *zip-local-file-header* s)))
    (values (if (funcall pred (cdr (assoc 'file-name metadata)))
                (decode-file-data metadata s)
                (progn (file-position s (+ (file-position s)
                                           (cdr (assoc 'compressed-size metadata))))
                       nil))
            metadata)))

(defun decode-a-file (s)
  (let ((metadata (extract *zip-local-file-header* s)))
    (decode-file-data metadata s)))
