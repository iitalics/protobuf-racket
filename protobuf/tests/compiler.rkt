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

    (check-exn (exn-matches exn:fail:compile? #px"encountered multiple types named \"A\"")
               (λ ()
                 (resolve+parse+compile "name-conflicts1.proto")))

    (check-exn (exn-matches exn:fail:compile? #px"encountered multiple types named \"A\"")
               (λ ()
                 (resolve+parse+compile "name-conflicts2.proto")))

    (check-exn (exn-matches exn:fail:compile? #px"encountered multiple types named \"B\"")
               (λ ()
                 (resolve+parse+compile "name-conflicts3.proto")))

    ))
