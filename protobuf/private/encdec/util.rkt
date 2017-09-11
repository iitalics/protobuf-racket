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


;; (define-reader-from-decoder reader decoder)
;;   decoder : bytes? pos? -> X pos?
;;
;; generate a function 'reader' which reads bytes from a port
;; and parses them with the given decoder function, such that
;;   reader : [input-port?] -> (or/c eof-object? X)
;;
;; NOTE: this traverses the entire contents of the port, and is
;; thus not suitable for use on e.g. a TCP connection port.
(define-syntax-rule (define-reader-from-decoder reader decoder)
  (define reader
    (let ([dec decoder])
      (λ ([in (current-input-port)])
        (let loop ([siz 256])
          (define bs (peek-bytes siz 0 in))
          (cond
            [(eof-object? bs)
             eof]

            [(= (bytes-length bs) siz)
             (loop (* 2 siz))]

            [else
             (let-values ([(x n) (dec bs 0)])
               (read-bytes! bs in 0 n)
               x)]))))))
