#lang racket/base

(module+ test
  (require "../private/dependencies.rkt"
           "../private/compiler.rkt"
           rackunit)

  (parameterize ([extra-proto-paths '("files/compiler")])

    (define (resolve+parse+compile . paths)
      (parameterize ([current-file-desc-pool (make-hash)])
        (map compile-root (parse+dependencies paths))))

    (define ((exn-matches pred pat) e)
      (and (pred e)
           (regexp-match? pat (exn-message e))))


    ;; test name conflicts

    (for ([filename '("name-conflicts1.proto"
                      "name-conflicts2.proto"
                      "name-conflicts3.proto")])
      (check-exn (exn-matches exn:fail:compile? #px"encountered multiple types named \"A\"")
                 (λ () (resolve+parse+compile filename))))

    (for ([filename '("name-conflicts4.proto"
                      "name-conflicts5.proto"
                      "name-conflicts6.proto"
                      "name-conflicts7.proto")])
      (check-exn (exn-matches exn:fail:compile? #px"encountered multiple fields named \"x\"")
                 (λ () (resolve+parse+compile filename))))

    (check-not-exn (λ () (resolve+parse+compile "name-conflicts8.proto")))

    ))
