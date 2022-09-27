(defpackage :fwoar.lisp-sandbox.ssdp-impl
  (:use :cl )
  (:export ))
(in-package :fwoar.lisp-sandbox.ssdp-impl)

(defun crlf-string (&rest lines)
  (with-output-to-string (s)
    (loop for line in lines
          do
             (etypecase line
               (string (princ line s))
               (cons (apply #'format s line)))
             (princ #.(coerce #(#\return #\newline)
                              'string)
                    s))))


(defun make-search (&optional (ST "upnp:rootdevice"))
  (crlf-string "M-SEARCH * HTTP/1.1"
               "HOST: 239.255.255.250:1900"
               "MAN:\"ssdp:discover\""
               "MX:3"
               (list "ST: ~a" st)
               ""))

(defun send-discover-root-device (sock)
  (sb-bsd-sockets:socket-send sock
                              (make-search)
                              nil
                              :address (list #(239 255 255 250) 1900)))

(defun send-search (sock &optional st)
  (sb-bsd-sockets:socket-send sock
                              (make-search st)
                              nil
                              :address (list #(239 255 255 250) 1900)))

(defun receive-discover-result (sock &optional blocking)
  (let ((buffer (make-array 4096 :element-type 'serapeum:octet)))
    (multiple-value-bind (buf read address port)
        (sb-bsd-sockets:socket-receive sock buffer nil :dontwait (not blocking))
      (declare (ignore read))
      (values (when buf
                (funcall (http-parse:make-parser (make-instance 'http-parse:http-response))
                         buf))
              address
              port))))

(defun addr->int (addr)
  (let ((addr (coerce addr 'vector)))
    (cffi:with-foreign-object (vbuf :int32)
      (dotimes (i 4)
        (setf (cffi:mem-ref vbuf :uint8 i) (aref addr i)))
      (cffi:mem-aref vbuf :int32))))

(defun setup-sockopt (socket addr)
  (setf (sb-bsd-sockets:sockopt-ip-multicast-loop socket) nil
        (sb-bsd-sockets:sockopt-multicast-if socket) (addr->int addr)
        (sb-bsd-sockets:sockopt-multicast-ttl socket) 2)
  socket)
