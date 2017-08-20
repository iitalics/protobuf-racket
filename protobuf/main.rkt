#lang racket/base

(module+ test
  (require rackunit))

;; Code here

(module+ test
  ;; Tests to be run with raco test
  )

(module+ main
  (require "private/codegen.rkt"
           racket/class)

  (load-protobuf "main.proto"
    #:extra-proto-path "tests/files"
    #:export ([tests.files.Color color]
              [tests.files.Point point]))


  (define pt (new point% [x 3] [y 4]))

  (displayln (send pt get-x))
  (displayln (send pt get-color))


  )
