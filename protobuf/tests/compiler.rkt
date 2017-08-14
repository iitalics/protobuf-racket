#lang racket/base

(module+ test
  (require "../private/dependencies.rkt"
           "../private/compiler.rkt"
           rackunit)

  (parameterize ([extra-proto-paths '("files/compiler")])

    (define (resolve+parse+compile . paths)
      (parameterize ([current-file-desc-pool (make-hash)]
                     [current-names-in-use (make-hash)])
        (map compile-root (parse+dependencies paths))))

    (define ((exn-matches pred pat) e)
      (and (pred e)
           (regexp-match? pat (exn-message e))))


    ;; test name conflicts

    (for ([i (in-range 1 9)])
      (check-exn (exn-matches exn:fail:compile? #px"already bound in ")
                 (λ () (resolve+parse+compile
                        (format "name-conflicts~a.proto" i)))
                 (format "name-conflict-~a" i)))

    ;(check-not-exn (λ () (resolve+parse+compile "name-conflicts9.proto")))

    ))
