#lang racket/base
(require "util.rkt")
(provide read-varint
         write-varint)

(define (msb-of x)
  (bitwise-and x #x80))
(define (lsbs-of x)
  (bitwise-and x #x7f))
(define (pop-lsbs x)
  (arithmetic-shift x -7))

;; read-varint : [input-port?] -> exact-nonnegative-integer?
(define (read-varint [in (current-input-port)])
  (define-values (int _)
    (for/fold ([acc 0] [shf 0])
              ([b (in-port must-read-byte in)])
      #:final (zero? (msb-of b))
      (values
       (+ acc (arithmetic-shift (lsbs-of b) shf))
       (+ shf 7))))

  int)

;; write-varint : exact-nonnegative-integer? [output-port?] -> void?
(define (write-varint x [out (current-output-port)])
  (let loop ([x x])
    (define msb (if (>= x 128) #x80 #x00))
    (write-byte (+ msb (lsbs-of x)) out)
    (unless (zero? msb)
      (loop (pop-lsbs x)))))
