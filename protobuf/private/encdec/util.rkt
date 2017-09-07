#lang racket/base
(provide (all-defined-out))

(define (must-read-byte in)
  (define b (read-byte in))
  (if (eof-object? b)
      (raise (exn:fail:read:eof "port ended while reading protobuf message"
                                (current-continuation-marks)
                                '()))
      b))
