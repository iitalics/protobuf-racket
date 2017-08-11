#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax)
         "lexer.rkt" "ast.rkt"
         parser-tools/lex
         parser-tools/yacc)


(define current-parse-source-path
  (make-parameter #f))

(define (raise-parse-error tok-ok? tok-name tok-value
                           start-pos end-pos)
  (let ([src (make-srcloc (current-parse-source-path)
                          #f #f
                          start-pos
                          (- end-pos start-pos))]
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


(define-parser parse-ast
  #:start <file>
  #:end EOF
  (<file> [(KW-syntax EQ IDENT SEMI)
           (format "the protocol version is: ~a" $3)]))
