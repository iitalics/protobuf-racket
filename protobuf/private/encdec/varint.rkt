#lang racket/base
(require "util.rkt")
(provide decode-varint write-varint)

(define (msb-of x)
  (bitwise-and x #x80))
(define (lsbs-of x)
  (bitwise-and x #x7f))
(define (pop-lsbs x)
  (arithmetic-shift x -7))

;; decode-varint : bytes? pos? -> nat? pos?
(define (decode-varint bs i0)
  (let loop ([acc 0] [i i0])
    (check-len bs i 1)
    (define b (bytes-ref bs i))
    (define shf (* 7 (- i i0)))
    (define acc+ (+ acc (arithmetic-shift (lsbs-of b) shf)))
    (if (zero? (msb-of b))
        (values acc+ (add1 i))
        (loop acc+ (add1 i)))))

;; write-varint : exact-nonnegative-integer? [output-port?] -> void
(define (write-varint x [out (current-output-port)])
  (let loop ([x x])
    (define msb (if (>= x 128) #x80 #x00))
    (write-byte (+ msb (lsbs-of x)) out)
    (unless (zero? msb)
      (loop (pop-lsbs x)))))
