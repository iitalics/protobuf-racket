#lang racket/base

(module+ test
  (require "../private/dependencies.rkt"
           "../private/compiler.rkt"
           racket/list
           racket/class
           rackunit)

  (parameterize ([extra-proto-paths '("files/compiler")])

    (define (parse->descriptor path)
      (with-clean-compile
        (compile-root (first (parse+dependencies (list path))))))

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
         (check-true (andmap (λ (i) (send A index-reserved? i)) '(3 4 5 6 7 8 9 10 11)))
         (check-false (ormap (λ (i) (send A index-reserved? i)) '(11 12 13 14 15 16 17 18 19)))
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

    ))
