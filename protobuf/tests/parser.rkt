#lang racket

(module+ test
  (require rackunit
           "../private/lexer.rkt"
           "../private/parser.rkt"
           "../private/ast.rkt"
           parser-tools/lex
           (for-syntax racket/base
                       syntax/parse
                       syntax/stx
                       racket/syntax
                       racket/sequence))

  (define-for-syntax test-id 0)
  (define-syntax parse-test
    (syntax-parser
      [(_ (token ...) ast ...)
       ; generate real expressions for tokens, which includes implicit EOF
       #:with [token+eof ...] #'[token ... EOF]
       #:with [token-expr ...]
       (stx-map (syntax-parser
                  [TOK:id #''TOK]
                  [(TOK:id val)
                   #:with token-TOK (format-id #'TOK "token-~a" #'TOK)
                   #'(token-TOK val)])
                #'[token+eof ...])

       ; generate random source locations for each token
       #:with [($k-src ln col) ...] (for/list ([x (in-syntax #'[token+eof ...])]
                                               [k (in-naturals 1)])
                                      (list
                                       (format-id this-syntax "$~a-src" k)
                                       k
                                       (random 2 100)))
       #:with [k-pos ...] (generate-temporaries #'[$k-src ...])

       ; fake name for source file
       #:with test-name (begin (set! test-id (add1 test-id))
                               (format "parse-test-#~a.proto" test-id))

       #'(let ([$k-src (make-srcloc 'test-name 'ln 'col 1 #f)] ...)
           (let ([parsed-asts
                  (let ([k-pos (make-position 1 'ln 'col)] ...)
                    (parameterize ([current-parse-source-path 'test-name])
                      (parse-ast/sequence
                       (list (position-token token-expr k-pos k-pos) ...))))])
             (for ([expected (in-list (list ast ...))]
                   [parsed (in-list parsed-asts)])
               (check-equal? parsed expected))))]))




  (parse-test [
               KW-syntax EQ (STRINGLIT "proto3") SEMI
               KW-package (IDENT "foo") DOT (IDENT "bar") SEMI
               ]
              (ast:package $5-src "foo.bar"))

  (parse-test [
               KW-syntax EQ (STRINGLIT "proto3") SEMI
               KW-import (STRINGLIT "foo/bar") SEMI
               KW-import KW-public (STRINGLIT "foo/baz") SEMI
               ]
              (ast:import $5-src "foo/bar" #f)
              (ast:import $8-src "foo/baz" #t))


  )
