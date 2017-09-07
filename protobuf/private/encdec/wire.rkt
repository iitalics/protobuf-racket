#lang racket/base
(require "util.rkt" "varint.rkt"
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(provide uint->sint/2c
         uint->sint/zz
         read-fixed64 read-sfixed64 read-double
         read-fixed32 read-sfixed32 read-float)

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
