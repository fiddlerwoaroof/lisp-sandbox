(in-package :sockint)
;; MCAST_JOIN_GROUP
#+(or win32 windows)(defconstant mcast-join-group 41)
#+linux(defconstant mcast-join-group 42)
#+freebsd(defconstant mcast-join-group 80)
#+darwin(defconstant mcast-join-group 80) 

;; IP_MULTICAST_LOOP
#+(or win32 windows)(defconstant ip-multicast-loop 11)
#+linux(defconstant ip-multicast-loop 34)
#+freebsd(defconstant ip-multicast-loop 11)
#+darwin(defconstant ip-multicast-loop 11)

;; IPV6_MULTICAST_LOOP
#+(or win32 windows)(defconstant ip6-multicast-loop 11)
#+linux(defconstant ip6-multicast-loop 19)
#+freebsd(defconstant ip6-multicast-loop 19)
#+darwin(defconstant ip6-multicast-loop 19)

;; IP_MULTICAST_IF
#+(or win32 windows)(defconstant ip-multicast-if 9)
#+linux(defconstant ip-multicast-if 32)
#+freebsd(defconstant ip-multicast-if 9)
#+darwin(defconstant ip-multicast-if 9)

;; IPV6_MULTICAST_IF
#+(or win32 windows)(defconstant ip6-multicast-if 9)
#+linux(defconstant ip6-multicast-if 17)
#+freebsd(defconstant ip6-multicast-if 9)
#+darwin(defconstant ip6-multicast-if 9)

;; IP_MULTICAST_TTL
#+(or win32 windows)(defconstant ip-multicast-ttl 10)
#+linux(defconstant ip-multicast-ttl 33)
#+freebsd(defconstant ip-multicast-ttl 10)
#+darwin(defconstant ip-multicast-ttl 10)

;; IPV6_MULTICAST_HOPS
#+(or win32 windows)(defconstant ipv6-multicast-hops 10)
#+linux(defconstant ipv6-multicast-hops 18)
#+freebsd(defconstant ipv6-multicast-hops 10)
#+darwin(defconstant ipv6-multicast-hops 10)

(in-package :sb-bsd-sockets)

(define-socket-option-bool
    sockopt-ip-multicast-loop :ip sockint::ip-multicast-loop)
(define-socket-option-int
    sockopt-ip-multicast-if :ip sockint::ip-multicast-if)
(define-socket-option-int
    sockopt-ip-multicast-ttl :ip sockint::ip-multicast-ttl)


