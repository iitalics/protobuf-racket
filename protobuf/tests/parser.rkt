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
      [(_ (token ...)
          (~optional (~seq #:package exp-pkg-name:str) #:defaults ([exp-pkg-name #'""]))
          (~optional (~seq #:imports exp-imports:expr ...) #:defaults ([(exp-imports 1) '()]))
          (~optional (~seq #:messages exp-msgs:expr ...) #:defaults ([(exp-msgs 1) '()]))
          (~optional (~seq #:enums exp-enums:expr ...) #:defaults ([(exp-enums 1) '()]))
          (~optional (~seq #:options exp-opts:expr ...) #:defaults ([(exp-opts 1) '()])))

       ; generate real expressions for tokens, which includes implicit EOF
       #:with [token+eof ...] #'[token ... EOF]
       #:with [token-expr ...]
       (stx-map (syntax-parser
                  [TOK:id #''TOK]             ; TOK      =>  'TOK
                  [x:str #'(token-IDENT 'x)]  ; "ID"     =>  (token-IDENT "ID")
                  [n:nat #'(token-INTLIT 'n)] ; n        =>  (token-INTLIT n)
                  [(TOK:id val)               ; (TOK x)  =>  (token-TOK x)
                   #:with token-TOK (format-id #'TOK "token-~a" #'TOK)
                   #'(token-TOK val)])
                #'[token+eof ...])

       ; generate random source locations for each token
       #:with [($k-src ln col) ...] (for/list ([x (in-syntax #'[token+eof ...])]
                                               [k (in-naturals 1)])
                                      (list (format-id this-syntax "$~a-src" k)
                                            (datum->syntax this-syntax k)
                                            (datum->syntax this-syntax (random 2 100))))
       #:with [k-pos ...] (generate-temporaries #'[$k-src ...])

       ; fake name for source file
       #:with test-name (begin (set! test-id (add1 test-id))
                               (datum->syntax this-syntax
                                              (format "parse-test-#~a.proto" test-id)))

       ; things to check
       #:with ([accessor expecteds ...] ...)
       #'( [ast:root-imports    exp-imports ...]
           [ast:root-messages   exp-msgs ...]
           [ast:root-enums      exp-enums ...]
           [ast:root-options    exp-opts ...] )

       #'(let ([$k-src (make-srcloc test-name ln col 1 #f)] ...)
           ; parse it
           (let ([ast
                  (let ([k-pos (make-position 1 ln col)] ...)
                    (parameterize ([current-parse-source test-name])
                      (parse-ast/sequence
                       (list (position-token token-expr k-pos k-pos) ...))))])
             (check-equal? (ast:root-package ast) exp-pkg-name)
             (for ([x (in-list (accessor ast))]
                   [y (list expecteds ...)])
               (check-equal? x y)) ...))]))




  (parse-test [
               KW-syntax EQ (STRINGLIT "proto3") SEMI
               KW-package "foo" DOT "bar" SEMI
               ]
              #:package "foo.bar")

  (parse-test [
               KW-syntax EQ (STRINGLIT "proto3") SEMI
               KW-import (STRINGLIT "foo/bar") SEMI
               KW-import KW-public (STRINGLIT "foo/baz") SEMI
               ]
              #:imports
              (ast:import $5-src "foo/bar" #f)
              (ast:import $8-src "foo/baz" #t))

  (parse-test [
               KW-syntax EQ (STRINGLIT "proto3") SEMI
               KW-option "option1" DOT "x" EQ KW-true SEMI
               KW-option LP "option2" DOT "y" RP DOT "z" EQ MINUS 3 SEMI
               ]
              #:options
              (ast:option $5-src #f '("option1" "x") #t)
              (ast:option $12-src "option2.y" '("z") -3))

  (parse-test [
               KW-syntax EQ (STRINGLIT "proto3") SEMI
               KW-message "Msg1" LC
               KW-repeated KW-int32 "x" EQ 3 SEMI
               DOT "test" DOT "Msg2" "y" EQ 4 SEMI
               KW-reserved 3
               COMMA 4 KW-to 6
               COMMA 10 KW-to KW-max SEMI
               KW-reserved (STRINGLIT "z") COMMA (STRINGLIT "w") SEMI
               RC
               ]
              #:messages
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
               "A" EQ 0 SEMI
               "B" EQ 6 SEMI
               RC
               ]
              #:enums
              (ast:enum $5-src
                        "Enum1"
                        (list (ast:enum-val $8-src "A" 0 '())
                              (ast:enum-val $12-src "B" 6 '()))
                        '()))

  (parse-test [
               KW-syntax EQ (STRINGLIT "proto3") SEMI
               KW-message "A" LC
               KW-message "B" LC
               KW-message "C" LC
               RC
               RC
               KW-reserved 2 SEMI
               KW-enum "D" LC
               "o" EQ 0 SEMI
               RC
               RC
               ]
              #:messages
              (ast:message $5-src
                           "A"
                           '() '() '()
                           (list (ast:message $8-src
                                              "B"
                                              '() '() '()
                                              (list (ast:message $11-src
                                                                 "C"
                                                                 '() '() '()
                                                                 '()' () '() '()))
                                              '() '() '()))
                           (list (ast:enum $19-src
                                           "D"
                                           (list (ast:enum-val $22-src "o" 0 '()))
                                           '()))
                           (list 2)
                           '()))

  (parse-test [
               KW-syntax EQ (STRINGLIT "proto3") SEMI
               KW-message "A" LC
               KW-option "opt" EQ (FLOATLIT 3.0) SEMI
               KW-int32 "x" EQ 0
               LB "iopt1" EQ KW-true
               COMMA "iopt2" EQ (STRINGLIT "yes") RB SEMI
               RC
               KW-enum "B" LC
               "o" EQ 0 LB LP "iopt3" RP EQ 7 RB SEMI
               KW-option "opt" EQ KW-false SEMI
               RC
               ]
              #:messages
              (ast:message $5-src
                           "A"
                           (list (ast:field $13-src
                                            "x"
                                            0
                                            'optional
                                            'int32
                                            (list (ast:option $18-src #f '("iopt1") #t)
                                                  (ast:option $22-src #f '("iopt2") "yes"))))
                           '() '() '() '() '()
                           (list (ast:option $8-src #f '("opt") 3.0)))
              (ast:enum $28-src
                        "B"
                        (list (ast:enum-val $31-src
                                            "o"
                                            0
                                            (list (ast:option $35-src "iopt3" '() 7))))
                        (list (ast:option $42-src #f '("opt") #f))))

  (parse-test [
               KW-syntax EQ (STRINGLIT "proto3") SEMI
               KW-message "A" LC
               KW-map LT KW-fixed32 COMMA KW-string GT "tbl" EQ 0 SEMI
               KW-oneof "one" LC
               KW-uint32 "i" EQ 1 SEMI
               KW-double "d" EQ 2 SEMI
               RC
               RC
               ]
              #:messages
              (ast:message $5-src
                           "A"
                           '()
                           (list (ast:oneof $18-src
                                            "one"
                                            (list (ast:field $21-src
                                                             "i"
                                                             1
                                                             'optional
                                                             'uint32
                                                             '())
                                                  (ast:field $26-src
                                                             "d"
                                                             2
                                                             'optional
                                                             'double
                                                             '()))))
                           (list (ast:map-field $8-src
                                                "tbl"
                                                0
                                                'fixed32
                                                'string
                                                '()))
                           '() '() '() '()))

  )
