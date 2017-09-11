#lang racket/base
(module+ test
  (require rackunit racket/port
           "../private/encdec/varint.rkt"
           (for-syntax racket/base syntax/parse))

  (define-syntax varint-test
    (syntax-rules (=> <= <=>)
      [(_ [in-b ...] => out)
       (let-values ([(x i) (decode-varint (bytes in-b ...) 0)])
         (check-equal? x out (format "~a => ~a" (list in-b ...) out))
         (check-equal? i (length '(in-b ...))))]

      [(_ [out-b ...] <= in)
       (let ([port (open-output-bytes)])
         (write-varint in port)
         (check-equal? (get-output-bytes port)
                       (bytes out-b ...)
                       (format "~a <= ~a" (list out-b ...) in)))]

      [(_ lh <=> rh)
       (begin (varint-test lh <= rh)
              (varint-test lh => rh))]))


  (for ([i '(0 1 2 3 8 16 32 60 100 127)])
    (varint-test [i] <=> i))

  (for* ([i '(0 1 8 60 127)]
         [j '(1 8 60 127)])
    (varint-test [(+ i 128) j] <=> (+ i (* j 128))))

  (varint-test [172 2] <=> 300)
  (varint-test [206 194 241 5] <=> 12345678)

  (check-exn exn:fail:read?
             (λ ()
               (varint-test [128] => 0)))

  (with-input-from-bytes (bytes 172 2 144 3)
    (λ ()
      (for ([x (in-port read-varint)]
            [y (in-list '(300 400))])
        (check-equal? x y x))))

  )
