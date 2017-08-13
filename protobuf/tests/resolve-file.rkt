#lang racket/base

(module+ test
  (require rackunit
           racket/path
           "../private/dependencies.rkt")

  (define (cd [p "."])
    (simple-form-path p))

  ;; test (resolve-file)

  (define path-dummy (cd "dep-tests/dummy1.txt"))

  (parameterize ([default-proto-paths '("dep-tests/nested")]
                 [extra-proto-paths '("dep-tests")])
    (check-equal? (current-proto-paths)
                  (list (cd)
                        (cd "dep-tests")
                        (cd "dep-tests/nested"))))

  (check-equal? (resolve-file "dep-tests/dummy1.txt") path-dummy)
  (check-equal? (resolve-file "../tests/dep-tests/dummy1.txt") path-dummy)

  (parameterize ([extra-proto-paths '("dep-tests")])
    (check-equal? (resolve-file "dummy1.txt") path-dummy))

  (parameterize ([default-proto-paths '("dep-tests/nested")]
                 [extra-proto-paths '("dep-tests")])
    (check-equal? (resolve-file "dummy1.txt") path-dummy))

  (check-equal? (resolve-file "/etc/hostname")
                (string->path "/etc/hostname"))

  )
