#lang racket/base
(module+ test
  (require rackunit
           "../private/encdec/varint.rkt"
           (for-syntax racket/base syntax/parse))

  (define-syntax varint-test
    (syntax-rules (=> <= <=>)
      [(_ [in-b ...] => out)
       (check-equal? (read-varint (open-input-bytes
                                   (bytes in-b ...)))
                     out
                     (format "~a => ~a" (list in-b ...) out))]))

  (for ([i '(0 1 2 3 8 16 32 60 100 127)])
    (varint-test [i] => i))

  (for* ([i '(0 1 8 60 127)]
         [j '(1 8 60 127)])
    (varint-test [(+ i 128) j] => (+ i (* j 128))))

  (varint-test [172 2] => 300)
  (varint-test [206 194 241 5] => 12345678)

  )
