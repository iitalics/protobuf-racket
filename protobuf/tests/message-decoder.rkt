#lang racket/base
(module+ test
  (require rackunit
           "../private/codegen/decode.rkt")

  (struct person (name age) #:transparent)

  (define decode-person
    (message-decoder
     person
     []
     (1 "Person.name" 'string "")
     (2 "Person.age" 'int32 0)))

  (check-equal? (decode-person #"" 0) (person "" 0))
  (check-equal? (decode-person (bytes 10 4 ; #1, length delim, 4 bytes
                                      77 105 108 111 ; name = "Milo"
                                      16 ; #2, varint
                                      19) ; age = 19
                               0)
                (person "Milo" 19))

  (check-equal? (decode-person (bytes 10 3 106 101 102 ; name = "jef"
                                      10 4 77 105 108 111) ; name = "Milo"
                               0)
                (person "Milo" 0))

  )
