(uiop:define-package :fwoar.zipfile
    (:mix :cl :fwoar.lisputils :fwoar.bin-parser)
  (:export ))
(in-package :fwoar.zipfile)

(defmacro defun* (name (&rest args) &body body)
  (let ((defs (cdr (assoc :where body))))
    `(defun ,name ,args
       (flet (,@defs)
         ,@(loop for form in body until (and (consp form) (eql :where (car form)))
                 collect form)))))

(defun make-zipfile-stream (fn)
  (open fn :element-type '(unsigned-byte 8)))

(defparameter *zip-local-file-header*
  '((signature 4)
    (version 2)
    (flags 2)
    (compression 2 fwoar.bin-parser:le->int)
    (mod-time 2)
    (mod-date 2)
    (crc-32 4)
    (compressed-size 4 fwoar.bin-parser:le->int)
    (uncompressed-size 4 fwoar.bin-parser:le->int)
    (file-name-length 2 fwoar.bin-parser:le->int)
    (extra-field-length 2 fwoar.bin-parser:le->int)
    (file-name file-name-length babel:octets-to-string)
    (extra-field extra-field-length)))

(defun decode-file-data (metadata s)
  (let ((crc-32 (fwoar.bin-parser:le->int (cdr (assoc 'crc-32 metadata))))
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
