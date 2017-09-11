#lang racket/base
(module+ test
  (require rackunit
           racket/port
           "../private/encdec/wire.rkt"
           "../private/encdec/varint.rkt")

  (check-equal? (uint->sint/2c #x0005 16) 5)
  (check-equal? (uint->sint/2c #xffff 16) -1)
  (check-equal? (uint->sint/2c #xff02 16) -254)
  (check-equal? (uint->sint/2c #xfffe1dc0 32) -123456)
  (check-equal? (uint->sint/zz 0) 0)
  (check-equal? (uint->sint/zz 1) -1)
  (check-equal? (uint->sint/zz 2) 1)
  (check-equal? (uint->sint/zz 3) -2)
  (check-equal? (uint->sint/zz 4294967294) 2147483647)
  (check-equal? (uint->sint/zz 4294967295) -2147483648)


  (define-syntax-rule (decode-test check-fn dec-fn bytes-expr
                                   expected-output-expr msg ...)
    (let*-values ([(bs) bytes-expr]
                  [(i out) (dec-fn bs 0)])
      (check-fn out expected-output-expr msg ...)
      (check-equal? i (bytes-length bs))))

  (decode-test check-eqv? decode-sfixed64 (bytes #x87 #xc7 #xe2 #xdd #x28 #xe4 #xdf #x8) 639480536282351495)
  (decode-test check-eqv? decode-fixed64  (bytes #x87 #xc7 #xe2 #xdd #x28 #xe4 #xdf #x8) 639480536282351495)
  (decode-test check-eqv? decode-sfixed32 (bytes #x87 #xc7 #xe2 #xdd) -572340345)
  (decode-test check-eqv? decode-fixed32  (bytes #x79 #x38 #x1d #x22) 572340345)
  (decode-test check-= decode-double (bytes 239 183 49 19 132 214 47 193) -1043266.037 0.01)
  (decode-test check-= decode-float  (bytes 33 180 126 201) -1043266.062 0.01)
  (decode-test check-eqv? decode-sfixed64 (bytes #x12 #xa3 #x77 #x81 #x32 #xd4 #xf0 #xb9) -5048301868896771310)
  (decode-test check-eqv? decode-fixed64  (bytes #xee #x5c #x88 #x7e #xcd #x2b #xf #x46) 5048301868896771310)
  (decode-test check-eqv? decode-sfixed32 (bytes #x12 #xa3 #x77 #x81) -2122865902)
  (decode-test check-eqv? decode-fixed32  (bytes #xee #x5c #x88 #x7e) 2122865902)
  (decode-test check-= decode-double (bytes 10 114 155 43 19 174 244 64) 84705.198 0.01)
  (decode-test check-= decode-float  (bytes 153 112 165 71) 84705.195 0.01)
  (decode-test check-eqv? decode-sfixed64 (bytes #x5e #x4b #x3c #x7c #x83 #x6d #x7e #x96) -7602518709462348962)
  (decode-test check-eqv? decode-fixed64  (bytes #xa2 #xb4 #xc3 #x83 #x7c #x92 #x81 #x69) 7602518709462348962)
  (decode-test check-eqv? decode-sfixed32 (bytes #x5e #x4b #x3c #x7c) 2084326238)
  (decode-test check-eqv? decode-fixed32  (bytes #xa2 #xb4 #xc3 #x83) 2210641058)
  (decode-test check-= decode-double (bytes 155 43 15 185 96 248 230 64) 47043.023 0.01)
  (decode-test check-= decode-float  (bytes 6 195 55 71) 47043.023 0.01)
  (decode-test check-eqv? decode-sfixed64 (bytes #x15 #x51 #xf #x32 #x90 #x86 #xf #xef) -1220609020120837867)
  (decode-test check-eqv? decode-fixed64  (bytes #xeb #xae #xf0 #xcd #x6f #x79 #xf0 #x10) 1220609020120837867)
  (decode-test check-eqv? decode-sfixed32 (bytes #x15 #x51 #xf #x32) 839864597)
  (decode-test check-eqv? decode-fixed32  (bytes #xeb #xae #xf0 #xcd) 3455102699)
  (decode-test check-= decode-double (bytes 74 144 82 94 85 114 246 64) 91941.336 0.01)
  (decode-test check-= decode-float  (bytes 171 146 179 71) 91941.336 0.01)

  (decode-test check-equal? decode-length-delim #"\x04\x07\x08\x09\x0a" (bytes 7 8 9 10))
  (decode-test check-equal? decode-length-delim #"\x02\x0b\x0c" (bytes 11 12))
  (check-exn exn:fail:read?
             (λ ()
               (decode-length-delim #"\x06abcd" 0)))


  (define (->varint n)
    (with-output-to-bytes
      (λ () (write-varint n))))

  (decode-test check-equal? decode-field-number+type
               (->varint 8) '(1 . varint))

  (decode-test check-equal? decode-field-number+type
               (->varint 21) '(2 . 32-bit))

  (decode-test check-equal? decode-field-number+type
               (->varint 66) '(8 . length-delim))

  (decode-test check-equal? decode-field-number+type
               (->varint 9) '(1 . 64-bit))

  )
