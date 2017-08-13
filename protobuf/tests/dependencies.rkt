#lang racket/base

(module+ test
  (require "../private/dependencies.rkt"
           "../private/ast.rkt"
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
       #'(check-equal? (map ast-source
                            (parse+dependencies (list in-path ...)))
                       (list (resolve-file out-path) ...))]

      [(_ #:input (in-path:str ...)
          #:error exn?)
       #'(check-exn exn? (λ ()
                           (parse+dependencies (list in-path ...))))]))

  (dep-test
   #:input ("files/top.proto" "files/extra.proto")
   #:order ("files/extra.proto"
            "files/sub1.proto"
            "files/sub2.proto"
            "files/top.proto"))

  (parameterize ([extra-proto-paths '("files")])
    (dep-test
     #:input ("top.proto" "extra.proto")
     #:order ("extra.proto"
              "sub1.proto"
              "sub2.proto"
              "top.proto")))

  (dep-test
   #:input ("files/nonexistant.proto")
   #:error (λ (e)
             (and (exn:fail:filesystem? e)
                  (equal? (exn-message e) "cannot find protobuf file \"files/nonexistant.proto\""))))

  (dep-test
   #:input ("files/bad1.proto")
   #:error (λ (e)
             (and (exn:fail:user? e)
                  (equal? (exn-message e)
                          "cyclic dependencies found between \"bad1.proto\" and \"bad3.proto\""))))

  )
