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
                  [TOK:id #''TOK]            ; TOK      =>  'TOK
                  [x:str #'(token-IDENT 'x)] ; "ID"     =>  (token-IDENT "ID")
                  [(TOK:id val)              ; (TOK x)  =>  (token-TOK x)
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
               KW-package "foo" DOT "bar" SEMI
               ]
              (ast:package $5-src "foo.bar"))

  (parse-test [
               KW-syntax EQ (STRINGLIT "proto3") SEMI
               KW-import (STRINGLIT "foo/bar") SEMI
               KW-import KW-public (STRINGLIT "foo/baz") SEMI
               ]
              (ast:import $5-src "foo/bar" #f)
              (ast:import $8-src "foo/baz" #t))

  (parse-test [
               KW-syntax EQ (STRINGLIT "proto3") SEMI
               KW-option "option1" DOT "x" EQ KW-true SEMI
               KW-option LP "option2" DOT "y" RP DOT "z" EQ MINUS (INTLIT 3) SEMI
               ]
              (ast:option $5-src #f '("option1" "x") #t)
              (ast:option $12-src "option2.y" '("z") -3))

  (parse-test [
               KW-syntax EQ (STRINGLIT "proto3") SEMI
               KW-message "Msg1" LC
               KW-repeated KW-int32 "x" EQ (INTLIT 3) SEMI
               DOT "test" DOT "Msg2" "y" EQ (INTLIT 4) SEMI
               KW-reserved (INTLIT 3)
               COMMA (INTLIT 4) KW-to (INTLIT 6)
               COMMA (INTLIT 10) KW-to KW-max SEMI
               KW-reserved (STRINGLIT "z") COMMA (STRINGLIT "w") SEMI
               RC
               ]
              (ast:message $5-src
                           "Msg1"
                           (list (ast:field $8-src "x" 3 'repeated 'int32 '())
                                 (ast:field $14-src "y" 4 'optional ".test.Msg2" '()))
                           '() '() '() '()
                           (list 3
                                 (ast:range $25-src 4 6)
                                 (ast:range $29-src 10 'max)
                                 "z"
                                 "w")
                           '()))

  (parse-test [
               KW-syntax EQ (STRINGLIT "proto3") SEMI
               KW-enum "Enum1" LC
               "A" EQ (INTLIT 0) SEMI
               "B" EQ (INTLIT 6) SEMI
               RC
               ]
              (ast:enum $5-src
                        "Enum1"
                        (list (ast:enum-val $8-src "A" 0 '())
                              (ast:enum-val $12-src "B" 6 '()))
                        '()))

  )
