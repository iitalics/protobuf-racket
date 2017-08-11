#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax)
         "lexer.rkt" "ast.rkt"
         parser-tools/lex
         parser-tools/yacc)


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
                          (list src)))))


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


;; using as reference:
;;   https://developers.google.com/protocol-buffers/docs/reference/proto3-spec

;; TODO: recursive decent, or maybe look into parser combinator libraries.
;;       that would produce better error messages, and working with parser-tools/yacc
;;       kinda sucks anyways.

(define-parser parse-ast
  #:start <file>
  #:end EOF
  (<file>
   [(<syntax> <toplevels>)
    (begin (printf "the protocol version is: ~a\n" $1)
           $2)])

  (<syntax>
   [(KW-syntax EQ STRINGLIT SEMI) $3])

  (<toplevels>
   [(<toplevels> <import>) (cons $2 $1)]
   [(<toplevels> <empty>) $1]
   [() '()])

  (<import>
   [(KW-import STRINGLIT SEMI) (ast:import ($1-src) $2 #f)]
   [(KW-import KW-public STRINGLIT SEMI) (ast:import ($1-src) $3 #t)])

  (<empty>
   [(SEMI) (void)])

  )
