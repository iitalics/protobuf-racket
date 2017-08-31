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
       (let ([impls (implement/tmp '(".test1.Color")
                      "syntax = 'proto3';"
                      "package test1;"
                      "enum Color {"
                      "  Red = 0;"
                      "  Green = 1;"
                      "  Blue = 2;"
                      "}")])

         (define impl:Color (first impls))

         (syntax-parse (implement impl:Color)
           #:literals (λ define-values case else quote begin0)
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
            (fail (format "impl enum syntax: ~v"
                          (syntax->datum #'s)))]))))


    (check-not-exn
     (λ ()
       (let ([impls (implement/tmp '(".test2.Posn")
                      "syntax = 'proto3';"
                      "package test2;"
                      "message Posn {"
                      "  uint32 x = 1;"
                      "  string y = 2;"
                      "}")])

         (define impl:Posn (first impls))

         (syntax-parse (implement impl:Posn)
           #:literals (begin struct define quote)
           ;#:datum-literals (Red Green Blue)
           [(begin
              (struct %m (%x %y))
              (define (%make-m #:x [:id '0] #:y [:id '""])
                (%m~ :id :id))

              (define M? %m?)
              (define def-M (%make-m~)))

            (check-free-id=? #'M? (implementation-pred-id impl:Posn))
            (check-free-id=? #'def-M (implementation-default-id impl:Posn))
            (check-free-id=? #'%make-m #'%make-m~)
            (check-free-id=? #'%m #'%m~)]

           [s
            (fail (format "impl msg syntax: ~v"
                          (syntax->datum #'s)))]))))

    (check-not-exn
     (λ ()
       (let ([impls (implement/tmp '(".test2.Polygon")
                      "syntax = 'proto3';"
                      "package test2;"
                      "message Polygon {"
                      "  repeated Posn vertices = 1;"
                      "  test1.Color fill = 2;"
                      "}")])

         (define impl:Polygon (first impls))

         (syntax-parse (implement impl:Polygon)
           #:literals (begin struct define quote)
           ;#:datum-literals (Red Green Blue)
           [(begin
              (struct %m (%verts %fill))
              (define (%make-m #:vertices [_ '()] #:fill [_ :id])
                (_ :id :id))

              (define M? %m?)
              (define def-M (%make-m~)))

            'ok]

           [s
            (fail (format "impl msg syntax: ~v"
                          (syntax->datum #'s)))]))))



    ))
