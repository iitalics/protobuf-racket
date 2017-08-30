#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

(provide generate-protobuf)

(begin-for-syntax
  (define-syntax-class maybe-renamed
    #:attributes (orig into)
    (pattern orig:id #:with into #'orig)
    (pattern [orig:id into:id])))

(define-syntax generate-protobuf
  (syntax-parser
    [(_ (~or (~seq #:source source-file:str)
             (~seq #:extra-proto-path extra-proto-path:str)
             ) ...
        to-gen:maybe-renamed ...+)

     (error "unimplemented")]))
