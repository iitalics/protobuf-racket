#lang racket/base

(module+ test
  (require rackunit
           "../private/scope.rkt"
           (for-syntax racket/base syntax/parse))

  (struct mock (subscopes leaves)
    #:constructor-name make-mock-scope
    #:methods gen:scope
    [(define (subscope s name)
       (cond
         [(assoc name (mock-subscopes s)) => cdr]
         [else #f]))

     (define (scope-leaf s name)
       (cond
         [(assoc name (mock-leaves s)) => cdr]
         [else #f]))])

  (define-syntax mock-scope
    (syntax-parser
     #:datum-literals (=> ->)
     [(_ (~or (~seq ss:str => ss-rhs:expr)
              (~seq lf:str -> lf-rhs:expr)) ...)
      #'(make-mock-scope (list (cons ss ss-rhs) ...)
                         (list (cons lf lf-rhs) ...))]))

  (define <test.pkg1>
    (mock-scope "A" -> 'pkg1-A
                "B" -> 'pkg1-B))

  (define <test.pkg2.sub>
    (mock-scope "C" -> 'pkg2-sub-C))

  (define <test.pkg2>
    (mock-scope "sub" => <test.pkg2.sub>
                "A" -> 'pkg2-A
                "C" -> 'pkg2-C))

  (define <test>
    (mock-scope "pkg1" => <test.pkg1>
                "pkg2" => <test.pkg2>))

  (define <<base>>
    (mock-scope "test" => <test>))

  (define s@<<base>> (list <<base>>))
  (define s@<test> (list <test> <<base>>))
  (define s@<test.pkg1> (list <test.pkg1> <test> <<base>>))
  (define s@<test.pkg2> (list <test.pkg2> <test> <<base>>))
  (define s@<test.pkg2.sub> (list <test.pkg2.sub> <test.pkg2> <test> <<base>>))

  (check-equal? (resolve-type "A" s@<test.pkg1>) 'pkg1-A)
  (check-equal? (resolve-type "pkg1.A" s@<test.pkg1>) 'pkg1-A)
  (check-equal? (resolve-type "test.pkg1.A" s@<test.pkg1>) 'pkg1-A)
  (check-equal? (resolve-type ".test.pkg1.A" s@<test.pkg1>) 'pkg1-A)

  (check-equal? (resolve-type "A" s@<test.pkg2>) 'pkg2-A)
  (check-equal? (resolve-type "pkg1.A" s@<test.pkg2>) 'pkg1-A)
  (check-equal? (resolve-type "test.pkg1.A" s@<test.pkg2>) 'pkg1-A)
  (check-equal? (resolve-type ".test.pkg1.A" s@<test.pkg2>) 'pkg1-A)

  (check-equal? (resolve-type "C" s@<test.pkg2>) 'pkg2-C)
  (check-equal? (resolve-type "pkg2.C" s@<test.pkg2>) 'pkg2-C)
  (check-equal? (resolve-type "test.pkg2.C" s@<test.pkg2>) 'pkg2-C)
  (check-equal? (resolve-type ".test.pkg2.C" s@<test.pkg2>) 'pkg2-C)

  (check-equal? (resolve-type "C" s@<test.pkg2.sub>) 'pkg2-sub-C)
  (check-equal? (resolve-type "pkg2.C" s@<test.pkg2.sub>) 'pkg2-C)
  (check-equal? (resolve-type "pkg2.sub.C" s@<test.pkg2.sub>) 'pkg2-sub-C)

  (check-equal? (resolve-type "pkg1.C" s@<test>) 'pkg1-C)
  (check-equal? (resolve-type "test.pkg1.C" s@<test>) 'pkg1-C)
  (check-equal? (resolve-type ".test.pkg1.C" s@<test>) 'pkg1-C)

  (for ([x (list "test.pkg1.A"
                 "test.pkg1.B"
                 "test.pkg2.A"
                 "test.pkg2.C"
                 "test.pkg2.sub.C")])
    (for ([p (list s@<<base>>
                   s@<test>
                   s@<test.pkg1>
                   s@<test.pkg2>
                   s@<test.pkg2.sub>)])
      (check-equal? (resolve-type x p)
                    (resolve-type (string-append "." x) p))))

  )
