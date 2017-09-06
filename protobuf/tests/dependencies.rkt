#lang racket/base

(module+ test
  (require "../private/compiler/dependencies.rkt"
           "../private/compiler/ast.rkt"
           rackunit
           racket/path
           (for-syntax racket/base syntax/parse))

#|
    top   requires:  sub2, sub1
    sub1  requires:  extra
    sub2  requires:  extra, sub1
    extra requires:  (none)
dependecy order:    extra, sub1, sub2, top

    bad1   requires:  bad2
    bad2   requires:  bad3
    bad3   requires:  bad1
cycle detected
|#

  (define-syntax dep-test
    (syntax-parser
      [(_ #:input (in-path:str ...)
          #:order (out-path:str ...))
       #'(let-values ([(in-asts sorted-asts)
                       (parse+dependencies (list in-path ...))])
           (check-equal? (map ast-source-file-path sorted-asts)
                         (list (resolve-file out-path) ...)))]

      [(_ #:input (in-path:str ...)
          #:error exn?)
       #'(check-exn exn? (λ () (parse+dependencies (list in-path ...))))]))

  (parameterize ([extra-proto-paths '("files/deps")])

    (dep-test
     #:input ("top.proto" "extra.proto")
     #:order ("extra.proto"
              "sub1.proto"
              "sub2.proto"
              "top.proto"))

    (dep-test
     #:input ("nonexistant.proto")
     #:error (λ (e)
               (and (exn:fail:filesystem? e)
                    (equal? (exn-message e) "cannot find protobuf file \"nonexistant.proto\""))))

    (dep-test
     #:input ("bad1.proto")
     #:error (λ (e)
               (and (exn:fail:user? e)
                    (equal? (exn-message e)
                            "cyclic dependencies found between \"bad1.proto\" and \"bad3.proto\""))))

    ))
