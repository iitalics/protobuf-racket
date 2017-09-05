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
           #:literals (begin define-values make-struct-type define quote)
           ;#:datum-literals (Red Green Blue)
           [(begin
              (define-values (%type-m %m %m? %get %set!)
                (make-struct-type ':id #f 2 0))
              (define (%make-m #:x [:id 0] #:y [:id ""])
                (%m~ :id :id))
              (define (%get-x _) (%get~ _ 0))
              (define (%get-y _) (_     _ 1))
              (define M? %m?~)
              (define def-M (%make-m~)))

            (check-free-id=? #'M? (implementation-pred-id impl:Posn))
            (check-free-id=? #'def-M (implementation-default-id impl:Posn))
            (check-free-id=? #'%make-m #'%make-m~)
            (check-free-id=? #'%m #'%m~)
            (check-free-id=? #'%m? #'%m?~)
            (check-free-id=? #'%get #'%get~)]

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
           #:literals (begin define-values make-struct-type define quote)
           ;#:datum-literals (Red Green Blue)
           [(begin
              (define-values (%type-m %m %m? %get %set!)
                (make-struct-type ':id #f 2 0))
              (define (%make-m #:vertices [:id '()] #:fill [:id %def-color])
                (%m~ :id :id))
              (define (%get-v _) (%get~ _ 0))
              (define (%get-f _) (_     _ 1))
              (define M? %m?~)
              (define def-M (%make-m~)))

            (check-free-id=? #'%def-color
                             (implementation-default-id
                              (get-or-queue-impl ".test1.Color")))]

           [s
            (fail (format "impl msg syntax: ~v"
                          (syntax->datum #'s)))]))))


    (check-not-exn
     (λ ()
       (let ([impls (implement/tmp '(".test3.Move")
                      "syntax = 'proto3';"
                      "package test3;"
                      "message Move {"
                      "  oneof speed {"
                      "    float fwd = 1;"
                      "    float bwd = 2;"
                      "    float turn = 3;"
                      "  }"
                      "  oneof duration {"
                      "    float sec = 4;"
                      "    uint32 ms = 5;"
                      "  }"
                      "  bool precise = 6;"
                      "}")])

         (define impl:Move (first impls))
         (syntax-parse (implement impl:Move)
           #:literals (begin define-values make-struct-type define quote and error)
           [(begin
              (define-values (_ ...)
                (make-struct-type ':id #f 5 0))

              (define (%make-m #:precise [%p #f]
                               #:speed-case [%sc #f]
                               #:duration-case [%dc #f]
                               #:speed [%s (and %sc~ (error '%sc-err))]
                               #:duration [%d (and _ (error '_))])
                (_ %sc~~ %s~ %dc~ %d~ %p~))
              (define (%speed-case _)    (%get  _ 0))
              (define (%duration-case _) (%get~ _ 2))
              (define (%precise _)       (_     _ 4))
              (define M? _)
              (define def-M (_)))

            (check-free-id=? #'%p #'%p~)
            (check-free-id=? #'%sc #'%sc~)
            (check-free-id=? #'%sc #'%sc~~)
            (check-free-id=? #'%d #'%d~)
            (check-free-id=? #'%dc #'%dc~)
            (check-free-id=? #'%get #'%get~)
            (check-equal? (syntax-e #'%sc-err)
                          "keyword argument #:speed must be supplied when #:speed-case is")]

           [s
            (fail (format "impl msg syntax: ~v"
                          (syntax->datum #'s)))]))))

    ))
