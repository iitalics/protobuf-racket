#lang racket/base

(module+ test
  (require "../private/dependencies.rkt"
           "../private/compiler.rkt"
           "../private/ast.rkt"
           racket
           rackunit)

  (parameterize ([extra-proto-paths '("files/compiler")])

    (define (parse->descriptor path)
      (with-clean-compile
        (compile-root (first (parse+dependencies (list path))))))

    (define (parse->descriptors paths)
      (with-clean-compile
        (map compile-root (parse+dependencies paths))))

    (define ((exn-matches pred pat) e)
      (and (pred e)
           (regexp-match? pat (exn-message e))))


    ;; test name conflicts
    (for ([i '(1 2 3 4 5 6 7 8)])
      (check-exn (exn-matches exn:fail:compile? #px"already bound in file \"files/compiler/name-conflicts")
                 (λ () (parse->descriptor
                        (format "name-conflicts~a.proto" i)))
                 (format "name-conflict-~a" i)))


    ;; test messages & basic fields
    (check-not-exn
     (λ ()
       (let* ([fd (parse->descriptor "msgs1.proto")]
              [A (first (send fd get-message-types))]
              [A-fields (send A get-fields)]
              [B (first (send A get-nested-types))]
              [B-fields (send B get-fields)])
         (check-equal? (send A get-full-name) "foo.bar.A")
         (check-equal? (send B get-full-name) "foo.bar.A.B")
         (check-equal? (send (first A-fields) get-name) "x")
         (check-equal? (send (first A-fields) get-label) 'optional)
         (check-equal? (send (first A-fields) get-number) 1)
         (check-equal? (send (second A-fields) get-name) "y")
         (check-equal? (send (second A-fields) get-label) 'repeated)
         (check-equal? (send (second A-fields) get-number) 2)
         (check-equal? (send (first B-fields) get-name) "z"))))

    ;; test oneofs
    (check-not-exn
     (λ ()
       (let* ([fd (parse->descriptor "msgs2.proto")]
              [A (first (send fd get-message-types))]
              [O (first (send A get-oneofs))]
              [fields (send A get-fields)])
         (check-equal? (send A get-full-name) "A")
         (check-equal? (send (first fields) get-parent-oneof) #f)
         (check-equal? (send (second fields) get-parent-oneof) #f)
         (check-equal? (send (third fields) get-parent-oneof) O)
         (check-equal? (send (third fields) get-label) 'optional)
         (check-equal? (send (fourth fields) get-parent-oneof) O)
         (check-equal? (send (fourth fields) get-label) 'optional))))

    ;; test map fields
    (check-not-exn
     (λ ()
       (let* ([fd (parse->descriptor "msgs3.proto")]
              [A (first (send fd get-message-types))]
              [tbl (first (send A get-fields))]
              [Entry (send tbl get-type)]
              [Entry.key (first (send Entry get-fields))]
              [Entry.value (second (send Entry get-fields))])
         (check-equal? (send tbl get-number) 4)
         (check-equal? (send tbl get-label) 'repeated)
         (check-true (send (send Entry get-options) is-map-entry?))
         (check-equal? (send Entry.key get-number) 1)
         (check-equal? (send Entry.key get-type) 'int32)
         (check-equal? (send Entry.value get-number) 2)
         (check-equal? (send Entry.value get-type) 'string))))

    ;; test reserved fields
    (check-not-exn
     (λ ()
       (let* ([fd (parse->descriptor "msgs4.proto")]
              [A (first (send fd get-message-types))])
         (check-true (send A index-reserved? 1))
         (check-true (andmap (λ (i) (send A index-reserved? i)) '(3 4 5 6 7 8 9 10 15)))
         (check-false (ormap (λ (i) (send A index-reserved? i)) '(11 12 13 14 16 17 18 19)))
         (check-true (andmap (λ (i) (send A index-reserved? i)) '(20 21 22 999 99999)))
         (check-false (send A name-reserved? "x"))
         (check-true (send A name-reserved? "y"))
         (check-true (send A name-reserved? "z")))))

    ;; test enums
    (check-exn (exn-matches exn:fail:compile? #px"first enum field must be number 0")
               (λ () (parse->descriptor "enums1.proto"))
               "enums1")

    (check-exn (exn-matches exn:fail:compile? #px"enum must contain at least one field")
               (λ () (parse->descriptor "enums2.proto"))
               "enums1")

    (check-not-exn
     (λ ()
       (let* ([fd (parse->descriptor "enums3.proto")]
              [E (first (send fd get-enum-types))]
              [E-vals (send E get-values)])
         (check-equal? (send (first E-vals) get-name) "No")
         (check-equal? (send (first E-vals) get-number) 0)
         (check-equal? (send (second E-vals) get-name) "Yes")
         (check-equal? (send (second E-vals) get-number) 1)
         (check-equal? (send (third E-vals) get-name) "Maybe")
         (check-equal? (send (third E-vals) get-number) 2))))


    ;; test types
    (define (type-of f) (send f get-type))

    (check-not-exn
     (λ ()
       (let* ([fd (parse->descriptor "types1.proto")]
              [A (first (send fd get-message-types))])
         (check-equal? (map type-of (send A get-fields))
                       '(bool int32 uint32 sint32 fixed32
                         sfixed32 int64 uint64 sint64 fixed64
                         sfixed64 float double string bytes)))))

    (check-not-exn
     (λ ()
       (let* ([fd (parse->descriptor "types2.proto")]
              [A (first (send fd get-message-types))]
              [B (second (send fd get-message-types))]
              [C (first (send B get-nested-types))])
         (check-equal? (type-of (first (send A get-fields))) B)
         (check-equal? (type-of (first (send B get-fields))) A)
         (check-equal? (type-of (first (send C get-fields))) A))))

    (check-not-exn
     (λ ()
       (let* ([fd (parse->descriptor "types3.proto")]
              [A (first (send fd get-message-types))]
              [A.E (first (send A get-nested-enums))]
              [B (second (send fd get-message-types))]
              [B.E (first (send B get-nested-enums))])
         (check-equal? (type-of (first  (send A get-fields))) A.E)
         (check-equal? (type-of (second (send A get-fields))) B.E)
         (check-equal? (type-of (first  (send B get-fields))) B.E)
         (check-equal? (type-of (second (send B get-fields))) A.E)
         (check-equal? (type-of (third  (send B get-fields))) B.E))))

    (check-exn (exn-matches exn:fail:compile? #px"cannot resolve type C in scope test.compiler.types4.B")
               (λ () (parse->descriptor "types4.proto")))

    (check-not-exn
     (λ ()
       (let* ([fd-list (parse->descriptors '("types5.proto"))]
              [inside.fd (first fd-list)]
              [fd (second fd-list)]
              [inside.SW (first (send inside.fd get-message-types))]
              [SW (first (send fd get-message-types))]
              [Store (second (send fd get-message-types))]
              [Cust (third (send fd get-message-types))])
         ; Sandwich.description
         (check-equal? (type-of (second (send SW get-fields))) inside.SW)
         ; Store.offers
         (check-equal? (type-of (first (send Store get-fields))) SW)
         ; Customer.last_order
         (check-equal? (type-of (first (send Cust get-fields))) SW)
         ; Customer.favorite
         (check-equal? (type-of (second (send Cust get-fields))) inside.SW))))

    (check-not-exn
     (λ ()
       (let* ([fd (parse->descriptor "types6.proto")]
              [A (first (send fd get-message-types))]
              [B (second (send fd get-message-types))]
              [stuff (first (send B get-fields))]
              [Entry (send stuff get-type)]
              [key (first (send Entry get-fields))]
              [value (second (send Entry get-fields))])
         (check-true (send (send Entry get-options) is-map-entry?))
         (check-equal? (type-of key) 'uint32)
         (check-equal? (type-of value) A))))


    ;; test options
    (check-not-exn
     (λ ()
       (let* ([fd (parse->descriptor "options1.proto")]
              [file-opts (send fd get-options)]
              [DrawMode (first (send fd get-enum-types))]
              [Polygon (first (send fd get-message-types))]
              [point_x (first (send Polygon get-fields))])

         (check-equal? (send file-opts get-java-package) "draw.simple.types")
         (check-equal? (send file-opts get-java-outer-classname) "draw.simple.types.Graphics")
         (check-true (send file-opts is-java-string-utf8-checked?))
         (check-equal? (send file-opts get-go-package) "simpledraw")
         (check-equal? (send file-opts get-objc-class-prefix) "SD")
         (check-equal? (send file-opts get-c#-namespace) "Draw.Simple")
         (check-equal? (send file-opts get-swift-prefix) "SimpleDraw")
         (check-equal? (send file-opts get-php-class-prefix) ":(")
         (check-true (send (send DrawMode get-options) is-alias-allowed?))
         (check-true (send (send Polygon get-options) is-message-set-wire-format?))
         (check-true (send (send Polygon get-options) is-no-standard-accessor?))
         (check-true (send (send point_x get-options) is-packed?)))))

    (check-exn (exn-matches exn:fail:compile? #px"expected string value")
               (λ () (parse->descriptor "options2.proto")))

    (check-exn (exn-matches exn:fail:compile? #px"expected boolean value")
               (λ () (parse->descriptor "options3.proto")))

    (check-not-exn
     (λ ()
       (let* ([fd (parse->descriptor "options4.proto")]
              [A (first (send fd get-message-types))]
              [x-opts (send (first (send A get-fields)) get-options)])
         (check-equal? (send x-opts get-js-type) 'number))))

    (check-exn (exn-matches exn:fail:compile? #px"expected JSType enum value")
               (λ () (parse->descriptor "options5.proto")))

    (check-exn (exn-matches exn:fail:compile? #px"invalid option \"\\(invalid\\.opt\\)\\.io\\.n\"")
               (λ () (parse->descriptor "options6.proto")))

    ))
