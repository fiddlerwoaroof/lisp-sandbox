(defpackage :fwoar.generic-fun-special-decl
  (:use :cl )
  (:export ))
(in-package :fwoar.generic-fun-special-decl)

(defgeneric foo (arg))
(let (*arg*)
  (declare (special *arg*))
  (defmethod foo :around (*arg*)
             (declare (special *arg*))
             (format t "~&*ARG* in :around: ~s~%" *arg*)
             (call-next-method))
  (defmethod foo :before (arg)
             (declare (ignore arg))
             (format t "~&*ARG* in :before: ~s~%" *arg*))
  (defmethod foo :after (arg)
             (declare (ignore arg))
             (format t "~&*ARG* in :after: ~s~%" *arg*))
  (defmethod foo ((arg string))
    (format t "~&*ARG* in method for string: ~s~%" *arg*)
    (let ((*arg* (parse-integer *arg*)))
      (declare (special *arg*))
      (format t "~&*ARG* in method for string, after rebinding: ~s~%" *arg*)
      (call-next-method)))
  (defmethod foo (arg)
    (declare (ignore arg))
    (format t "~&*ARG* in method for t: ~s~%" *arg*)))


#|
FWOAR.GENERIC-FUN-SPECIAL-DECL> (foo "4")
*ARG* in :around: "4"
*ARG* in :before: "4"
*ARG* in method for string: "4"
*ARG* in method for string, after rebinding: 4
*ARG* in method for t: 4
*ARG* in :after: "4"
|#

;; in: DEFMETHOD FOO :AROUND (T)
;;     (DEFMETHOD FWOAR.GENERIC-FUN-SPECIAL-DECL::FOO :AROUND
;;                (FWOAR.GENERIC-FUN-SPECIAL-DECL::*ARG*)
;;       (DECLARE (SPECIAL FWOAR.GENERIC-FUN-SPECIAL-DECL::*ARG*))
;;       (FORMAT T "~&*ARG* in :around: ~s~%" FWOAR.GENERIC-FUN-SPECIAL-DECL::*ARG*)
;;       (CALL-NEXT-METHOD))
;; --> PROGN EVAL-WHEN SB-PCL::%DEFMETHOD-EXPANDER 
;; --> SB-PCL::LOAD-DEFMETHOD LIST* LET* SB-INT:NAMED-LAMBDA FUNCTION 
;; --> SYMBOL-MACROLET SB-PCL::FAST-LEXICAL-METHOD-FUNCTIONS 
;; --> SB-PCL::BIND-FAST-LEXICAL-METHOD-FUNCTIONS FLET CALL-NEXT-METHOD 
;; --> BLOCK SB-PCL::FAST-CALL-NEXT-METHOD-BODY IF IF SB-PCL::BIND-ARGS 
;; ==>
;;   (LET* ((SB-PCL::.ARGS-TAIL. SB-PCL::CNM-ARGS)
;;          (FWOAR.GENERIC-FUN-SPECIAL-DECL::*ARG* (POP SB-PCL::.ARGS-TAIL.))
;;          (SB-PCL::.DUMMY0.))
;;     (DECLARE (IGNORABLE SB-PCL::.ARGS-TAIL. SB-PCL::.DUMMY0.))
;;     (SB-PCL::INVOKE-NARROW-EFFECTIVE-METHOD-FUNCTION SB-PCL::.NEXT-METHOD-CALL.
;;                                                      NIL :REQUIRED-ARGS
;;                                                      (FWOAR.GENERIC-FUN-SPECIAL-DECL::*ARG*)
;;                                                      :REST-ARG NIL))
;; 
;; caught STYLE-WARNING:
;;   using the lexical binding of the symbol (FWOAR.GENERIC-FUN-SPECIAL-DECL::*ARG*), not the
;;   dynamic binding, even though the name follows
;;   the usual naming convention (names like *FOO*) for special variables

;; in: DEFMETHOD FOO :AROUND (T)
;;     (DEFMETHOD FWOAR.GENERIC-FUN-SPECIAL-DECL::FOO :AROUND
;;                (FWOAR.GENERIC-FUN-SPECIAL-DECL::*ARG*)
;;       (DECLARE (SPECIAL FWOAR.GENERIC-FUN-SPECIAL-DECL::*ARG*))
;;       (FORMAT T "~&*ARG* in :around: ~s~%" FWOAR.GENERIC-FUN-SPECIAL-DECL::*ARG*)
;;       (CALL-NEXT-METHOD))
;; --> PROGN EVAL-WHEN SB-PCL::%DEFMETHOD-EXPANDER 
;; --> SB-PCL::LOAD-DEFMETHOD LIST* LET* SB-INT:NAMED-LAMBDA FUNCTION 
;; --> SYMBOL-MACROLET SB-PCL::FAST-LEXICAL-METHOD-FUNCTIONS 
;; --> SB-PCL::BIND-FAST-LEXICAL-METHOD-FUNCTIONS FLET CALL-NEXT-METHOD 
;; --> BLOCK SB-PCL::FAST-CALL-NEXT-METHOD-BODY IF IF SB-PCL::BIND-ARGS 
;; ==>
;;   (LET* ((SB-PCL::.ARGS-TAIL. SB-PCL::CNM-ARGS)
;;          (FWOAR.GENERIC-FUN-SPECIAL-DECL::*ARG* (POP SB-PCL::.ARGS-TAIL.))
;;          (SB-PCL::.DUMMY0.))
;;     (DECLARE (IGNORABLE SB-PCL::.ARGS-TAIL. SB-PCL::.DUMMY0.))
;;     (SB-PCL::INVOKE-NARROW-EFFECTIVE-METHOD-FUNCTION SB-PCL::.NEXT-METHOD-CALL.
;;                                                      NIL :REQUIRED-ARGS
;;                                                      (FWOAR.GENERIC-FUN-SPECIAL-DECL::*ARG*)
;;                                                      :REST-ARG NIL))
;; 
;; caught STYLE-WARNING:
;;   using the lexical binding of the symbol (FWOAR.GENERIC-FUN-SPECIAL-DECL::*ARG*), not the
;;   dynamic binding, even though the name follows
;;   the usual naming convention (names like *FOO*) for special variables
;; 
;; compilation unit finished
;;   caught 2 STYLE-WARNING conditions
