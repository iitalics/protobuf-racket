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
             (check-equal? (dsctor:message-nested-enums A) (list A.E))
             (check-equal? (dsctor:enum-values A.E) (list A.V))
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
             (check-equal? (dsctor:message-fields A) (list A.x A.y A.z))
             (check-equal? (dsctor:field-oneof A.x) #f)
             (check-equal? (dsctor:field-oneof A.y) A.O)
             (check-equal? (dsctor:field-oneof A.z) A.O)
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
               (let* ([Entry (dsctor:field-type field-dsc)]
                      [Entry.key (first (dsctor:message-fields Entry))]
                      [Entry.value (second (dsctor:message-fields Entry))])

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
             (check-equal? (dsctor:message-fields A) (list A.m A.q)))))


    (check-not-exn
     (λ ()
       (let* ([r (compile-root/tmp "syntax = 'proto3';"
                                   "package test5;"
                                   "option java_package = 'com.cadlib.test5';"
                                   "message Part {"
                                   "  option deprecated = false;"
                                   "  enum Shape {"
                                   "    option allow_alias = true;"
                                   "    Plane = 0;"
                                   "    Flat = 0 [deprecated=true];"
                                   "    Cylinder = 1;"
                                   "  }"
                                   "  string name = 1 [ctype=STRING_PIECE];"
                                   "  repeated Shape shapes = 2 [packed=true];"
                                   "  map<uint32,float> dimension = 3;"
                                   "}")])

         (define checks
           `([".test5.Part"         "deprecated" #f]
             [".test5.Part.Shape"   "allow_alias" #t]
             [".test5.Part.Flat"    "deprecated" #t]
             [".test5.Part.name"    "ctype" STRING_PIECE]
             [".test5.Part.shapes"  "packed" #t]))

         (for ([chk (in-list checks)])
           (check-equal? (dsctor-option (hash-ref (all-descriptors) (car chk))
                                        (cadr chk)
                                        'UNSET)
                         (caddr chk)
                         (~a chk)))

         (define dim.Entry
           (dsctor:field-type (hash-ref (all-descriptors) ".test5.Part.dimension")))
         (check-true (dsctor-option dim.Entry "map_entry" #f)))))


    (check-not-exn
     (λ ()
       (let* ([r (compile-root/tmp "syntax = 'proto3';"
                                   "package test6;"
                                   "message Reservations {"
                                   "  reserved 1, 2, 4 to 8, 15 to max;"
                                   "  reserved 'a', 'b';"
                                   "}")])

         (define msg (hash-ref (all-descriptors) ".test6.Reservations"))
         (define yes '(1 2 4 5 6 7 8 15 16 17 30 70 999 "a" "b"))
         (define no '(3 9 10 11 12 13 14 "c" "d"))

         (for ([tg (in-list yes)])
           (check-true (dsctor:message-tag-reserved? msg tg) (format "yes: ~a" tg)))
         (for ([tg (in-list no)])
           (check-false (dsctor:message-tag-reserved? msg tg) (format "no: ~a" tg))))))


    (check-not-exn
     (λ ()
       (let* ([r (compile-root/tmp "syntax = 'proto3';"
                                   "package test7;"
                                   "message A {"
                                   "  enum B { X = 0; }"
                                   "  uint32 f1 = 1;"
                                   "  A f2 = 2;"
                                   "  B f3 = 3;"
                                   "  A.B f4 = 4;"
                                   "  test7.B f5 = 5;"
                                   "  .test7.B f6 = 6;"
                                   "  .test7.B.A f7 = 7;"
                                   "  map<string,B> f8 = 8;"
                                   "}"
                                   "message B { enum A { Y = 0; } }")])

         (define checks
           '([".test7.A.f2"   ".test7.A"]
             [".test7.A.f3"   ".test7.A.B"]
             [".test7.A.f4"   ".test7.A.B"]
             [".test7.A.f5"   ".test7.B"]
             [".test7.A.f6"   ".test7.B"]
             [".test7.A.f7"   ".test7.B.A"]))

         (for ([chk (in-list checks)])
           (check-equal? (dsctor:field-type (hash-ref (all-descriptors) (car chk)))
                         (hash-ref (all-descriptors) (cadr chk))
                         (~a chk)))

         (define f8.Entry
           (dsctor:field-type (hash-ref (all-descriptors) ".test7.A.f8")))
         (define f8.Entry.value
           (second (dsctor:message-fields f8.Entry)))

         (check-equal? (dsctor:field-type f8.Entry.value)
                       (hash-ref (all-descriptors) ".test7.A.B")))))


    (check-exn
     (λ (e) (regexp-match? #px"cannot find type \"E\" in scope \".test8.A.B.C\""
                           (exn-message e)))
     (λ ()
       (compile-root/tmp "syntax = 'proto3';"
                         "package test8;"
                         "message A {"
                         "  message B {"
                         "    message C {"
                         "      E e = 1;"
                         "    }"
                         "    message D {"
                         "      enum E { V = 0; }"
                         "    }"
                         "  }"
                         "}")))


    (for ([code (in-list '("message A { bytes x = 1; reserved 'x'; }"
                           "message B { fixed64 x = 3; reserved 2 to 10; }"
                           "message C { double x = 3; sint32 y = 3; }"
                           "enum D { D1 = 0; D2 = 3; D3 = 3; }"
                           "enum E { E1 = 1; E2 = 0; }"
                           "message F { float x = 0; }"))]
          [msg (in-list '(#px"field name \"x\" is reserved"
                         #px"field number 3 is reserved"
                         #px"field number 3 used by multiple fields"
                         #px"cannot alias enum value number 3 without \"allow_alias\" enabled"
                         #px"first enum value must be number 0"
                         #px"field numbers must be positive"))]
          [i (in-naturals 1)])

      (check-exn
       (λ (e) (regexp-match? msg (exn-message e)))
       (λ () (compile-root/tmp "syntax = 'proto3';"
                               "package test9;"
                               code))
       (format "test9 #~a" i)))


    (check-not-exn
     (λ ()
       (let ([r (compile-root/tmp "syntax = 'proto3';"
                                  "package test10;"
                                  "message Tree {"
                                  "  string value = 1;"
                                  "  repeated Tree children = 2;"
                                  "}")])

         (let* ([Tree (hash-ref (all-descriptors) ".test10.Tree")]
                [Tree.value (hash-ref (all-descriptors) ".test10.Tree.value")]
                [Tree.children (hash-ref (all-descriptors) ".test10.Tree.children")])

           (check-equal? (dsctor:field-type Tree.children) Tree)
           (check-equal? (dsctor:message-fields Tree) (list Tree.value Tree.children))))))

    ))
