#lang racket/base

(module+ test
  (require racket
           rackunit
           racket/file
           "../private/compiler.rkt"
           "../private/descriptors.rkt"
           "../private/dependencies.rkt")

  ;; parse the ast:root for a file with the given string lines as contents
  (define (parse-root/tmp . strs)
    (let ([tmp-path (make-temporary-file "protobuftest~a.proto")])

      (with-output-to-file tmp-path
        #:exists 'replace
        (λ ()
          (for-each displayln strs)))

      ;; dynamic wind so that we always delete the temp file
      ;; if anything goes wrong
      (dynamic-wind
        void
        (λ ()
          (let-values ([(init-asts sorted-asts)
                        (parse+dependencies (list tmp-path))])
            (first init-asts)))
        (λ ()
          (delete-file tmp-path)))))

  (define (compile-root/tmp . strs)
    (compile-root (apply parse-root/tmp strs)))


  (parameterize ([all-descriptors (make-hash)])

    (check-exn (λ (e) (regexp-match? #px"name \\.test1\\.A\\.x already used"
                                     (exn-message e)))
               (λ () (compile-root/tmp
                      "syntax = 'proto3';"
                      "package test1;"
                      "message A {"
                      "  uint32 x = 1;"
                      "  enum x { }"
                      "}")))

    (check-not-exn
     (λ () (let* ([r (compile-root/tmp
                      "syntax = 'proto3';"
                      "package test2;"
                      "message A {"
                      "  uint32 x = 1;"
                      "  enum E {"
                      "    V = 0;"
                      "  }"
                      "}")]

                  [A (hash-ref (all-descriptors) ".test2.A")]
                  [A.x (hash-ref (all-descriptors) ".test2.A.x")]
                  [A.E (hash-ref (all-descriptors) ".test2.A.E")]
                  [A.V (hash-ref (all-descriptors) ".test2.A.V")])

             (check-pred dsctor:message? A)
             (check-pred dsctor:field? A.x)
             (check-pred dsctor:enum? A.E)
             (check-pred dsctor:enum-value? A.V)
             (check-equal? (dsctor:field-number A.x) 1)
             (check-equal? (dsctor:enum-value-number A.V) 0)
             (check-equal? (dsctor:message-nested-enums A) '(".test2.A.E"))
             (check-equal? (dsctor:enum-values A.E) '(".test2.A.V"))
             )))


    ))
