#lang racket/base
(provide (all-defined-out))

;; write-X : X [output-port?] -> void

;; decode-X : bytes? pos? -> X pos?
;; decodes an object, returning that object and the
;; new position in the byte stream

(define pos?
  exact-nonnegative-integer?)

;; check-len : bytes? pos? nat? -> pos?
;;
;; if 'bs' has at least 'n' bytes following position 'i',
;; then returns 'i + n', otherwise raises an exception
(define (check-len bs i n)
  (when (> (+ i n) (bytes-length bs))
    (raise (exn:fail:read:eof "not enough data to decode from given bytes"
                              (current-continuation-marks)
                              '())))
  (+ i n))
