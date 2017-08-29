#lang racket
(module+ test
  (require rackunit
           syntax/parse
           syntax/stx
           "utils.rkt"
           "../private/compiler.rkt"
           "../private/codegen.rkt")

  (define-syntax-rule (check-free-id=? arg ...)
    (check free-identifier=? arg ...))

  (parameterize ([all-descriptors (make-hash)])

    (check-not-exn
     (λ ()
       (let ([impls
              (codegen-root/tmp "syntax = 'proto3';"
                                "package test1;"
                                "enum Color {"
                                "  Red = 0;"
                                "  Green = 1;"
                                "  Blue = 2;"
                                "}"
                                #:implementations '(".test1.Color"))])

         (define impl:Color (first impls))

         (syntax-parse (implement impl:Color)
           #:literals (λ define-values case else quote)
           #:datum-literals (Red Green Blue)
           [(define-values (E-> ->E E? def-E)
              (_ (λ (_) (case _
                          [(Red) '0]
                          [(Green) '1]
                          [(Blue) '2]
                          [else #f]))
                 (λ (_) (case _
                          [(0) 'Red]
                          [(1) 'Green]
                          [(2) 'Blue]
                          [else #f]))
                 (_ 'Red 'Green 'Blue)
                 (begin0 'Red 'Green 'Blue)))

            (check-free-id=? #'E? (implementation-pred-id impl:Color))
            (check-free-id=? #'def-E (implementation-default-id impl:Color))]

           [s
            (fail (format "impl syntax: ~v"
                          (syntax->datum #'s)))]))))

    ))
