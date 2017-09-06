#lang racket/base
(provide read-varint)

(define (must-read-byte in)
  (define b (read-byte in))
  (if (eof-object? b)
      (raise (exn:fail:read:eof "port ended while reading varint"
                                (current-continuation-marks)
                                '()))
      b))

(define (msb-of x)
  (bitwise-and x #x80))
(define (lsbs-of x)
  (bitwise-and x #x7f))

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
