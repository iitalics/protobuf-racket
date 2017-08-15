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
         (check-equal? (send (first fields) get-parent-oneof) #f)
         (check-equal? (send (second fields) get-parent-oneof) #f)
         (check-equal? (send (third fields) get-parent-oneof) O)
         (check-equal? (send (third fields) get-label) 'optional)
         (check-equal? (send (fourth fields) get-parent-oneof) O)
         (check-equal? (send (fourth fields) get-label) 'optional))))

    ;; test enums
    (check-exn (exn-matches exn:fail:compile? #px"first enum field must be number 0")
               (λ () (parse->descriptor "enums1.proto"))
               "enums1")

    (check-not-exn
     (λ ()
       (let* ([fd (parse->descriptor "enums2.proto")]
              [E (first (send fd get-enum-types))]
              [F (second (send fd get-enum-types))]
              [E-vals (send E get-values)])
         (check-equal? (send (first E-vals) get-name) "No")
         (check-equal? (send (first E-vals) get-number) 0)
         (check-equal? (send (second E-vals) get-name) "Yes")
         (check-equal? (send (second E-vals) get-number) 1)
         (check-equal? (send (third E-vals) get-name) "Maybe")
         (check-equal? (send (third E-vals) get-number) 2)

         (check-true (send (send F get-options) is-alias-allowed?)))))

    ))
