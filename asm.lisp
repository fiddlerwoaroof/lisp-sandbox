;;; How to make the popcnt instruction available
(defpackage "POPCNT"
  (:use "CL")
  (:export "POPCNT"))

(in-package "POPCNT")

(sb-c:defknown popcnt ((unsigned-byte 64)) (integer 0 64)
    (sb-c:foldable sb-c:flushable sb-c:movable)
  :overwrite-fndb-silently t)

(in-package "SB-VM")

(define-vop (popcnt:popcnt)
  (:policy :fast-safe)
  (:translate popcnt:popcnt)
  (:args (x :scs (unsigned-reg) :target r))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 3
    (unless (location= r x) ; only break the spurious dep. chain
      (inst xor r r))       ; if r isn't the same register as x.
    (inst popcnt r x)))

(in-package "POPCNT")

(defun popcnt (x)
  (declare (inline))
  (popcnt x))
