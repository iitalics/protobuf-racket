#lang racket/base
(require "util.rkt")
(provide decode-varint read-varint write-varint)

(define (msb-of x)
  (bitwise-and x #x80))
(define (lsbs-of x)
  (bitwise-and x #x7f))
(define (pop-lsbs x)
  (arithmetic-shift x -7))

;; decode-varint : bytes? pos? -> pos? nat?
(define (decode-varint bs i0)
  (let loop ([i i0] [acc 0])
    (check-len bs i 1)
    (define b (bytes-ref bs i))
    (define shf (* 7 (- i i0)))
    (define acc+ (+ acc (arithmetic-shift (lsbs-of b) shf)))
    (if (zero? (msb-of b))
        (values (add1 i) acc+)
        (loop (add1 i) acc+))))

;; read-varint : [input-port?] -> (or/c eof-object? exact-nonnegative-integer?)
(define-reader-from-decoder read-varint decode-varint)

;; write-varint : exact-nonnegative-integer? [output-port?] -> void
(define (write-varint x [out (current-output-port)])
  (let loop ([x x])
    (define msb (if (>= x 128) #x80 #x00))
    (write-byte (+ msb (lsbs-of x)) out)
    (unless (zero? msb)
      (loop (pop-lsbs x)))))
