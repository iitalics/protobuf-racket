#lang racket/base
(require math/flonum
         "util.rkt" "varint.rkt"
         (for-syntax racket/base
                     racket/syntax))


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
  (check-equal? (uint->sint/zz 4294967295) -2147483648))
