#lang racket/base

(module+ test
  (require rackunit
           racket/path
           "../private/compiler/dependencies.rkt")

  (define (cd [p "."])
    (simple-form-path p))

  ;; test (resolve-file)

  (define path-dummy (cd "files/dummy1.txt"))

  (parameterize ([default-proto-paths '("files/nested")]
                 [extra-proto-paths '("files")])
    (check-equal? (current-proto-paths)
                  (list (cd)
                        (cd "files")
                        (cd "files/nested"))))

  (check-equal? (resolve-file "files/dummy1.txt") path-dummy)
  (check-equal? (resolve-file "../tests/files/dummy1.txt") path-dummy)

  (parameterize ([extra-proto-paths '("files")])
    (check-equal? (resolve-file "dummy1.txt") path-dummy))

  (parameterize ([default-proto-paths '("files/nested")]
                 [extra-proto-paths '("files")])
    (check-equal? (resolve-file "dummy1.txt") path-dummy))

  (check-equal? (resolve-file "/etc/hostname")
                (string->path "/etc/hostname"))

  )
