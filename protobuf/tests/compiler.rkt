#lang racket/base

(module+ test
  (require "../private/dependencies.rkt"
           "../private/compiler.rkt"
           racket/list
           racket/class
           rackunit)

  (parameterize ([extra-proto-paths '("files/compiler")])

    (define (parse->descriptor . paths)
      (with-clean-compile
        (map ast->descriptor (parse+dependencies paths))))

    (define ((exn-matches pred pat) e)
      (and (pred e)
           (regexp-match? pat (exn-message e))))


    ;; test name conflicts
    (for ([i '(1 2 3 4 5 6 7 8)])
      (check-exn (exn-matches exn:fail:compile? #px"already bound in file \"files/compiler/name-conflicts")
                 (λ () (parse->descriptor
                        (format "name-conflicts~a.proto" i)))
                 (format "name-conflict-~a" i)))

    ;; test enums
    (check-exn (exn-matches exn:fail:compile? #px"first enum field must be number 0")
               (λ () (parse->descriptor "enums1.proto"))
               "enums1")

    (check-not-exn
     (λ ()
       (let* ([fd (first (parse->descriptor "enums2.proto"))]
              [E (first (send fd get-enum-types))]
              [F (second (send fd get-enum-types))]
              [E-vals (send E get-values)])
         (check-equal? (send (first E-vals) get-name) "No")
         (check-equal? (send (first E-vals) get-number) 0)
         (check-equal? (send (second E-vals) get-name) "Yes")
         (check-equal? (send (second E-vals) get-number) 1)
         (check-equal? (send (third E-vals) get-name) "Maybe")
         (check-equal? (send (third E-vals) get-number) 2)

         (check-equal? (send (send F get-options) is-alias-allowed?) #t))))

    ))
