#lang racket/base
(require "util.rkt" "varint.rkt"
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))


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


(module+ test
  (require rackunit)
  (check-equal? (uint->sint/2c #x0005 16) 5)
  (check-equal? (uint->sint/2c #xffff 16) -1)
  (check-equal? (uint->sint/2c #xff02 16) -254)
  (check-equal? (uint->sint/2c #xfffe1dc0 32) -123456)
  (check-equal? (uint->sint/zz 0) 0)
  (check-equal? (uint->sint/zz 1) -1)
  (check-equal? (uint->sint/zz 2) 1)
  (check-equal? (uint->sint/zz 3) -2)
  (check-equal? (uint->sint/zz 4294967294) 2147483647)
  (check-equal? (uint->sint/zz 4294967295) -2147483648)

  (check-eqv? (read-sfixed64 (open-input-bytes (bytes #xdb #x60 #x3d #x20 #xb2 #x5c #x3e #x5a))) 6502736832084402395)
  (check-eqv? (read-fixed64  (open-input-bytes (bytes #xdb #x60 #x3d #x20 #xb2 #x5c #x3e #x5a))) 6502736832084402395)
  (check-eqv? (read-sfixed32 (open-input-bytes (bytes #xdb #x60 #x3d #x20))) 540893403)
  (check-eqv? (read-fixed32  (open-input-bytes (bytes #xdb #x60 #x3d #x20))) 540893403)
  (check-= (read-double (open-input-bytes (bytes 232 193 151 25 23 253 219 192))) -28660.361 0.01)
  (check-= (read-float  (open-input-bytes (bytes 185 232 223 198))) -28660.361 0.01)
  (check-eqv? (read-sfixed64 (open-input-bytes (bytes #x16 #xc1 #xf7 #xd #xc3 #x43 #xd7 #x9e))) -7001052590692122346)
  (check-eqv? (read-fixed64  (open-input-bytes (bytes #xea #x3e #x8 #xf2 #x3c #xbc #x28 #x61))) 7001052590692122346)
  (check-eqv? (read-sfixed32 (open-input-bytes (bytes #x16 #xc1 #xf7 #xd))) 234340630)
  (check-eqv? (read-fixed32  (open-input-bytes (bytes #xea #x3e #x8 #xf2))) 4060626666)
  (check-= (read-double (open-input-bytes (bytes 176 3 133 186 255 245 180 64))) 5365.999 0.01)
  (check-= (read-float  (open-input-bytes (bytes 254 175 167 69))) 5365.999 0.01)
  (check-eqv? (read-sfixed64 (open-input-bytes (bytes #xfe #x72 #x38 #x14 #xc6 #x47 #xc2 #xdc))) -2539388323815197954)
  (check-eqv? (read-fixed64  (open-input-bytes (bytes #x2 #x8d #xc7 #xeb #x39 #xb8 #x3d #x23))) 2539388323815197954)
  (check-eqv? (read-sfixed32 (open-input-bytes (bytes #xfe #x72 #x38 #x14))) 339243774)
  (check-eqv? (read-fixed32  (open-input-bytes (bytes #x2 #x8d #xc7 #xeb))) 3955723522)
  (check-= (read-double (open-input-bytes (bytes 178 48 161 145 243 60 227 192))) -39399.612 0.01)
  (check-= (read-float  (open-input-bytes (bytes 157 231 25 199))) -39399.613 0.01)
  (check-eqv? (read-sfixed64 (open-input-bytes (bytes #x68 #x5 #x46 #xe2 #x48 #x42 #x32 #x46))) 5058178212291806568)
  (check-eqv? (read-fixed64  (open-input-bytes (bytes #x68 #x5 #x46 #xe2 #x48 #x42 #x32 #x46))) 5058178212291806568)
  (check-eqv? (read-sfixed32 (open-input-bytes (bytes #x68 #x5 #x46 #xe2))) -498727576)
  (check-eqv? (read-fixed32  (open-input-bytes (bytes #x98 #xfa #xb9 #x1d))) 498727576)
  (check-= (read-double (open-input-bytes (bytes 179 37 208 191 0 44 244 64))) 82624.047 0.01)
  (check-= (read-float  (open-input-bytes (bytes 6 96 161 71))) 82624.047 0.01)

  )
