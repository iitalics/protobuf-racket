#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax)
         "lexer.rkt" "ast.rkt"
         parser-tools/lex
         parser-tools/yacc
         racket/match)


(define current-parse-source-path
  (make-parameter #f))


(define (position->srcloc pos)
  (make-srcloc (current-parse-source-path)
               (position-line pos)
               (position-col pos)
               (position-offset pos)
               #f))

(define (raise-parse-error tok-ok? tok-name tok-value
                           start-pos end-pos)
  (let* ([off1 (position-offset start-pos)]
         [off2 (position-offset end-pos)]
         [line (position-line start-pos)]
         [col (position-col start-pos)]
         [src (make-srcloc (current-parse-source-path)
                           line
                           col
                           off1
                           (- off2 off1))]
         [msg (format "invalid token: ~a"
                      (protobuf-token->string tok-name))])
    (raise (exn:fail:read msg
                          (current-continuation-marks)
                          (list src src)))))


(define-syntax define-parser
  (syntax-parser
    [(_ parse #:start <start> #:end END
        rule ...)
     #:with parse/gen (format-id this-syntax "~a/generator" #'parse)
     #:with parse/seq (format-id this-syntax "~a/sequence" #'parse)
     #:with parse/port (format-id this-syntax "~a/port" #'parse)
     #'(begin
         (define parse/gen
           (parser (src-pos)
                   (tokens protobuf-tokens protobuf-empty-tokens)
                   (start <start>)
                   (end END)
                   (error raise-parse-error)
                   (grammar rule ...)))

         (define (parse/seq seq)
           (let-values ([(any? gen) (sequence-generate seq)])
             (parse/gen gen)))

         (define (parse/port port)
           (port-count-lines! port)
           (parse/seq (in-protobuf-tokens port #:include-eof? #t)))

         (define (parse x)
           (cond
             [(or (path? x) (string? x))
              (parameterize ([current-parse-source-path x])
                (with-input-from-file x
                  (λ () (parse/port (current-input-port)))))]

             [(input-port? x) (parse/port x)]
             [(procedure? x) (parse/gen x)]
             [else (error "invalid parse input:" x)])))]))

(define-syntax $1-src
  (syntax-parser
    [(_)
     #:with $1-start-pos (datum->syntax this-syntax '$1-start-pos)
     #'(position->srcloc $1-start-pos)]))

;; generalized partition on many predicates
;;   e.g.
;;     (distinguish '(-3 -2 -1 0 1 2 3)
;;                   (λ (x) (> x 0))
;;                   (λ (x) (< x 0)))
;;     ->
;;    '((1 2 3) (-3 -2 -1) (0))
;;   the first list returned obeys the first predicate
;;   the second list returned obeys the second predicate
;;   etc.
;;   the last list returned fails all of the predicates
(define (distinguish lst #:reverse? [rev? #f]  . preds)
  (let ([out (make-vector (add1 (length preds)) '())])
    (for ([x (in-list (if rev? lst (reverse lst)))])
      (let ([idx (or (for/first ([p (in-list preds)]
                                 [i (in-naturals)]
                                 #:when (p x))
                       i)
                     (length preds))])
        (vector-set! out idx
                     (cons x (vector-ref out idx)))))
    (vector->list out)))



;; using as reference:
;;   https://developers.google.com/protocol-buffers/docs/reference/proto3-spec

;; TODO: recursive decent, or maybe look into parser combinator libraries.
;;       that would produce better error messages, and working with parser-tools/yacc
;;       kinda sucks anyways.

(define-parser parse-ast
  #:start <file>
  #:end EOF
  ;;   file
  (<file>
   [(<syntax> <toplevels>)
    (begin (printf "the protocol version is: ~a\n" $1)
           $2)])

  (<syntax>
   [(KW-syntax EQ STRINGLIT SEMI) $3])


  ;; common
  (<full-ident>
   [(<full-ident> DOT IDENT) (string-append $1 "." $3)]
   [(IDENT) $1])

  (<type-ident>
   [(DOT <full-ident>) (string-append "." $2)]
   [(<full-ident>) $1])

  (<constant>
   [(INTLIT) $1]
   [(FLOATLIT) $1]
   [(PLUS INTLIT) $2]
   [(PLUS FLOATLIT) $2]
   [(MINUS INTLIT) (- $2)]
   [(MINUS FLOATLIT) (- $2)]
   [(STRINGLIT) $1]
   [(KW-true) #t]
   [(KW-false) #f])


  ;;   toplevels
  (<toplevels>
   [(<toplevels> <import>) (cons $2 $1)]
   [(<toplevels> <package>) (cons $2 $1)]
   [(<toplevels> <option>) (cons $2 $1)]
   [(<toplevels> <message>) (cons $2 $1)]
   [(<toplevels> <enum>) (cons $2 $1)]
   [(<toplevels> <empty>) $1]
   [() '()])

  (<import>
   [(KW-import STRINGLIT SEMI) (ast:import ($1-src) $2 #f)]
   [(KW-import KW-public STRINGLIT SEMI) (ast:import ($1-src) $3 #t)])

  (<package>
   [(KW-package <full-ident> SEMI) (ast:package ($1-src) $2)])

  (<empty>
   [(SEMI) (void)])


  ;;   types
  (<key-type>
   [(KW-int32) 'int32]
   [(KW-int64) 'int64]
   [(KW-uint32) 'uint32]
   [(KW-uint64) 'uint64]
   [(KW-sint32) 'sint32]
   [(KW-sint64) 'sint64]
   [(KW-fixed32) 'fixed32]
   [(KW-fixed64) 'fixed64]
   [(KW-sfixed32) 'sfixed32]
   [(KW-sfixed64) 'sfixed64]
   [(KW-bool) 'bool]
   [(KW-string) 'string])

  (<type>
   [(<key-type>) $1]
   [(KW-float) 'float]
   [(KW-double) 'double]
   [(KW-bytes) 'bytes]
   [(<type-ident>) $1])


  ;;   options
  (<option-name>
   [(LP <full-ident> RP) (list $2)]
   [(IDENT) (list $1 #f)]
   [(<option-name> DOT IDENT) (cons $3 $1)])

  (<option>
   [(KW-option <option-name> EQ <constant> SEMI)
    (let* ([parts (reverse $2)])
      (ast:option ($1-src)
                  (car parts)
                  (cdr parts)
                  $4))])

  (<inline-option>
   [(<option-name> EQ <constant>)
    (let* ([parts (reverse $1)])
      (ast:option ($1-src)
                  (car parts)
                  (cdr parts)
                  $3))])

  (<inline-options>
   [(<inline-option>) (list $1)]
   [(<inline-option> COMMA <inline-options>) (cons $1 $3)])

  (<field-options>
   [(LB <inline-options> RB) $2]
   [() '()])


  ;;   message
  (<message>
   [(KW-message IDENT LC <msg-elems> RC)
    (match (distinguish $4 #:reverse? #t
                        ast:field?
                        ast:message?
                        ast:enum?
                        ast:option?)
      [(list fields messages enums options other)
       (ast:message ($1-src)
                    $2
                    fields
                    messages
                    enums
                    (foldr append '() other)
                    options)])])

  (<msg-elems>
   [(<msg-elems> <field>) (cons $2 $1)]
   [(<msg-elems> <enum>) (cons $2 $1)]
   [(<msg-elems> <message>) (cons $2 $1)]
   [(<msg-elems> <option>) (cons $2 $1)]
   ; TODO: oneof
   ; TODO: map-field
   [(<msg-elems> <reserved>) (cons $2 $1)]
   [(<msg-elems> <empty>) $1]
   [() '()])

  (<field>
   [(<field-label> <type> IDENT EQ INTLIT <field-options> SEMI)
    (ast:field ($1-src) $3 $5 $1 $2 $6)])

  (<field-label>
   [(KW-repeated) 'repeated]
   [() 'optional])

  (<reserved>
   [(KW-reserved <names> SEMI) $2]
   [(KW-reserved <ranges> SEMI) $2])

  (<names>
   ; google's BNF is wrong here, but according to the parser and code
   ; examples, you should describe field names using string literals
   [(STRINGLIT) (list $1)]
   [(STRINGLIT COMMA <names>) (cons $1 $3)])

  (<ranges>
   [(<range>) (list $1)]
   [(<range> COMMA <ranges>) (cons $1 $3)])

  [<range>
   [(INTLIT KW-to INTLIT) (ast:range ($1-src) $1 $3)]
   [(INTLIT KW-to KW-max) (ast:range ($1-src) $1 'max)]
   [(INTLIT) $1]]


  ;;    enum
  (<enum>
   [(KW-enum IDENT LC <enum-elems> RC)
    (match (distinguish $4 #:reverse? #t
                        ast:enum-val?)
      [(list values options)
       (ast:enum ($1-src)
                 $2
                 values
                 options)])])

  (<enum-elems>
   [(<enum-elems> <option>) (cons $2 $1)]
   [(<enum-elems> <enum-val>) (cons $2 $1)]
   [(<enum-elems> <empty>) $1]
   [() '()])

  (<enum-val>
   [(IDENT EQ INTLIT <field-options> SEMI)
    (ast:enum-val ($1-src) $1 $3 $4)])

  )
