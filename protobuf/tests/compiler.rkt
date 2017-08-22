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
               (λ () (compile-root/tmp "syntax = 'proto3';"
                                       "package test1;"
                                       "message A {"
                                       "  uint32 x = 1;"
                                       "  enum x { }"
                                       "}")))


    (check-not-exn
     (λ () (let* ([r (compile-root/tmp "syntax = 'proto3';"
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
             (check-false (dsctor:field-repeated? A.x))
             (check-equal? (dsctor:field-number A.x) 1)
             (check-equal? (dsctor:field-type A.x) 'uint32)
             (check-equal? (dsctor:enum-value-number A.V) 0)
             (check-equal? (dsctor:message-nested-enums A) '(".test2.A.E"))
             (check-equal? (dsctor:enum-values A.E) '(".test2.A.V"))
             )))


    (check-not-exn
     (λ () (let* ([r (compile-root/tmp "syntax = 'proto3';"
                                       "package test3;"
                                       "message A {"
                                       "  repeated uint32 x = 1;"
                                       "  oneof O {"
                                       "    uint32 y = 2;"
                                       "    uint32 z = 3;"
                                       "  }"
                                       "}")]

                  [A (hash-ref (all-descriptors) ".test3.A")]
                  [A.x (hash-ref (all-descriptors) ".test3.A.x")]
                  [A.y (hash-ref (all-descriptors) ".test3.A.y")]
                  [A.z (hash-ref (all-descriptors) ".test3.A.z")]
                  [A.O (hash-ref (all-descriptors) ".test3.A.O")])

             (check-pred dsctor:message? A)
             (check-pred dsctor:field? A.x)
             (check-pred dsctor:oneof? A.O)
             (check-equal? (dsctor:message-fields A) '(".test3.A.x" ".test3.A.y" ".test3.A.z"))
             (check-equal? (dsctor:field-oneof A.x) #f)
             (check-equal? (dsctor:field-oneof A.y) ".test3.A.O")
             (check-equal? (dsctor:field-oneof A.z) ".test3.A.O")
             (check-true (dsctor:field-repeated? A.x))
             (check-false (dsctor:field-repeated? A.z))
             )))


    (check-not-exn
     (λ () (let* ([r (compile-root/tmp "syntax = 'proto3';"
                                       "package test4;"
                                       "message A {"
                                       "  map<uint32, string> m = 1;"
                                       "  map<sint32, bytes> q = 2;"
                                       "}")]

                  [A (hash-ref (all-descriptors) ".test4.A")]
                  [A.m (hash-ref (all-descriptors) ".test4.A.m")]
                  [A.q (hash-ref (all-descriptors) ".test4.A.q")])

             (define (entry-tests <label> field-dsc #:key key-ty #:val val-ty)
               (let* ([entry (dsctor:field-type field-dsc)]
                      [Entry (hash-ref (all-descriptors) entry)]
                      [Entry.key (hash-ref (all-descriptors) (string-append entry ".key"))]
                      [Entry.value (hash-ref (all-descriptors) (string-append entry ".value"))])

                 (check-true (regexp-match? #px"\\(hidden\\d+\\)\\.test4\\.A\\.MapFieldEntry" entry))
                 (check-pred dsctor:message? Entry <label>)
                 (check-pred dsctor:field? Entry.key <label>)
                 (check-pred dsctor:field? Entry.value <label>)
                 (check-equal? (dsctor-name Entry) "MapFieldEntry")
                 (check-true (dsctor:field-repeated? field-dsc) <label>)
                 (check-false (dsctor:field-repeated? Entry.key) <label>)
                 (check-false (dsctor:field-repeated? Entry.value) <label>)
                 (check-equal? (dsctor:field-type Entry.key) key-ty <label>)
                 (check-equal? (dsctor:field-number Entry.key) 1 <label>)
                 (check-equal? (dsctor:field-type Entry.value) val-ty <label>)
                 (check-equal? (dsctor:field-number Entry.value) 2 <label>)))

             (entry-tests "A { map<> m }" A.m #:key 'uint32 #:val 'string)
             (entry-tests "A { map<> q }" A.q #:key 'sint32 #:val 'bytes)
             (check-equal? (dsctor:message-fields A) '(".test4.A.m" ".test4.A.q")))))


    ))
