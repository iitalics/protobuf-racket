#lang racket/base
(require "util.rkt" "varint.rkt"
         racket/contract/base
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(provide uint->sint/2c
         uint->sint/zz
         read-fixed64 read-sfixed64 read-double
         read-fixed32 read-sfixed32 read-float
         read-field-number+type)

;; convert unsigned integer to signed integer
;; using two's complement, e.g.
;;   (uint->sint/2c #x0005 16) = 5
;;   (uint->sint/2c #xffff 16) = -1
;;   (uint->sint/2c #xff02 16) = -254
;;
;; uint-sint/2c : nat? nat? -> exact-integer?
(define (uint->sint/2c x n-bits)
  (if (zero? (bitwise-and (arithmetic-shift 1 (sub1 n-bits)) x))
      x
      (- x (arithmetic-shift 1 n-bits))))

;; convert unsigned integer to signed integer
;; using "ZigZag", e.g.
;;   (uint->sint/zz 0) = 0
;;   (uint->sint/zz 1) = -1
;;   (uint->sint/zz 4294967294) = 2147483647
;;   (uint->sint/zz 4294967295) = -2147483648
(define (uint->sint/zz x)
  (if (even? x)
      (arithmetic-shift x -1)
      (- 0 (arithmetic-shift x -1) 1)))



(define-syntax define-fixed-bytes-reader
  (syntax-parser
    [(_ reader bits:nat (func (~datum _) arg-expr ... (~datum _)))
     #:with byts (datum->syntax this-syntax
                                (/ (syntax-e #'bits) 8))
     #'(define (reader [in (current-input-port)]
                       [tmp (make-bytes byts)])

         (unless (eq? (read-bytes! tmp in 0 byts) byts)
           (raise (exn:fail:read:eof "port ended while reading protobuf messge"
                                     (current-continuation-marks)
                                     '())))
         (func tmp
               arg-expr ...
               0 byts))]))

(define-fixed-bytes-reader read-fixed64 64
  (integer-bytes->integer _ #f #f _))
(define-fixed-bytes-reader read-sfixed64 64
  (integer-bytes->integer _ #t #f _))
(define-fixed-bytes-reader read-double 64
  (floating-point-bytes->real _ #f _))
(define-fixed-bytes-reader read-fixed32 32
  (integer-bytes->integer _ #f #f _))
(define-fixed-bytes-reader read-sfixed32 32
  (integer-bytes->integer _ #t #f _))
(define-fixed-bytes-reader read-float 32
  (floating-point-bytes->real _ #f _))


;; wire types as described by the protocol buffers guide. the deprecated
;; wire types ("start group" and "end group") are not included
(define wire-type?
  (or/c 'varint '64-bit '32-bit 'length-delim))


;; reads and returns two values: the field number, and the wire type
;; of the field. encoded using just a varint and a bit-shift.
;;
;; read-field-number+type : [input-port?] -> exact-integer? wire-type?
(define (read-field-number+type [in (current-input-port)])
  (define n (read-varint in))
  (define wire-type (bitwise-and n 7))
  (define field-num (arithmetic-shift n -3))
  (case wire-type
    [(0) (values field-num 'varint)]
    [(1) (values field-num '64-bit)]
    [(2) (values field-num 'length-delim)]
    [(5) (values field-num '32-bit)]
    [else
     (raise (exn:fail:read "encountered invalid or deprecated wire type"
                           (current-continuation-marks)
                           '()))]))
