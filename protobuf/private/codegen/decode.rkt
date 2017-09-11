#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx))

(provide check-encoding
         message-decoder)

(define (check-encoding actual expected fld-name)
  (unless (eq? actual expected)
    (raise (exn:fail:read (format "field ~v encoded incorrectly"
                                  fld-name)
                          (current-continuation-marks)
                          '()))))


(define-syntax message-decoder
  (syntax-parser
    [(_) (error "message-decoder unimplemented")]))
