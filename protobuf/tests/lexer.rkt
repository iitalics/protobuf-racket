#lang racket

(module+ test
  (require rackunit
           parser-tools/lex
           "../private/lexer.rkt"
           (for-syntax syntax/parse))

  (define-syntax lex-test
    (syntax-parser
      [(_ in-str-lines ...
          test
          [out ...])
       #:with ((strs ...) ...) #'((in-str-lines "\n") ...)
       #'(check-not-exn
          (λ ()
            (test (for/list ([p (in-tokens
                                 (open-input-string
                                  (string-append strs ... ...)))])
                    (position-token-token p))
                  (list out ...))))]))

  ;; test keywords
  (lex-test "message option package import public"
            "true false"
            "syntax proto3"
            "int32 uint32 sint32 int64 uint64 sint64"
            "fixed32 sfixed32 fixed64 sfixed64 bool string bytes"
            check-equal?
            [
             'KW-message 'KW-option 'KW-package 'KW-import 'KW-public
             'KW-true 'KW-false
             'KW-syntax 'KW-proto3
             'KW-int32 'KW-uint32 'KW-sint32 'KW-int64 'KW-uint64 'KW-sint64
             'KW-fixed32 'KW-sfixed32 'KW-fixed64 'KW-sfixed64 'KW-bool 'KW-string 'KW-bytes
             ])

  ;; test identifiers
  (lex-test "foobar Foobar foo_bar uint32 uint32foo"
            check-equal?
            [
             (token-IDENT "foobar")
             (token-IDENT "Foobar")
             (token-IDENT "foo_bar")
             'KW-uint32
             (token-IDENT "uint32foo")
             ])

  ;; test numbers
  (lex-test "1234 0123 0x123 0 4.0 5.0e3"
            check-equal?
            [
             (token-INTLIT 1234)
             (token-INTLIT 83)
             (token-INTLIT 291)
             (token-INTLIT 0)
             (token-FLOATLIT 4.0)
             (token-FLOATLIT 5000.0)
             ])
  )
