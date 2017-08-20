#lang racket/base

(module+ test
  (require rackunit))

;; Code here

(module+ test
  ;; Tests to be run with raco test
  )

(module+ main
  (require "private/codegen.rkt")

  (load-protobuf "main.proto"
    #:extra-proto-path "tests/files"
    #:export ([tests.files.Color color]))

  (displayln (number->color 0)) ; Red

  )
