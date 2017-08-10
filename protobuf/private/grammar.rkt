#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     racket/syntax)
         racket/stream
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide tokenize ; (port -> position-token)
         in-tokens
         protobuf-tokens
         protobuf-empty-tokens)


;; using as reference:
;;   https://developers.google.com/protocol-buffers/docs/reference/proto3-spec

;    letters and digits
(define-lex-abbrevs
  [letter (:or (:/ #\A #\Z)
               (:/ #\a #\z))]
  [dec-dig (:/ #\0 #\9)]
  [oct-dig (:/ #\0 #\7)]
  [hex-dig (:or (:/ #\0 #\9)
                (:/ #\A #\F)
                (:/ #\a #\f))])

;    identifiers
(define-lex-abbrev ident
  (:seq letter
        (:* (:or letter
                 dec-dig
                 #\_))))

;    numbers
(define-lex-abbrevs
  [dec-lit (:seq (:/ #\1 #\9) (:* dec-dig))]
  [oct-lit (:seq #\0 (:* oct-dig))]
  [hex-lit (:seq #\0 (:or #\x #\X) (:* hex-dig))]
  [float-lit (:seq (:+ dec-dig) #\. (:* dec-dig)
                   (:? (:or #\e #\E)
                       (:? (:or #\+ #\-))
                       (:+ dec-dig)))])

;    other
(define-lex-abbrev comment
  (:seq "//"
        (complement (:: any-string #\newline any-string))
        (:or #\newline nothing)))



;; generate lookup tables for lexer for delimeters / keywords
(define-syntax define-delims/kws
  (syntax-parser
    [(_ group-id
        #:delims delim-hash delim-lex-abbrev ((del TOK) ...)
        #:keywords kw-hash (kw ...))
     #:with (KW-kw ...) (stx-map (λ (x) (format-id #'x "KW-~A" x)) #'[kw ...])
     #:with (kw/str ...) (stx-map (λ (x) (symbol->string (syntax->datum x))) #'[kw ...])
     #'(begin
         (define delim-hash (make-hash '([del . TOK] ...)))
         (define kw-hash (make-hash '([kw/str . KW-kw] ...)))
         (define-empty-tokens group-id (TOK ... KW-kw ...))
         (define-lex-abbrev delim-lex-abbrev (union del ...)))]))


(define-tokens protobuf-tokens
  (IDENT INTLIT FLOATLIT))

(define-delims/kws protobuf-empty-tokens
  #:delims protobuf-delims delim
  ([#\; SEMI]
   [#\, COMMA]
   [#\( LP] [#\) RP]
   [#\[ LB] [#\] RB]
   [#\{ LC] [#\} RC]
   [#\< LT] [#\> GT]
   [#\+ PLUS]
   [#\- MINUS]
   [#\= EQ])
  #:keywords protobuf-keywords
  (syntax
   proto2 proto3
   true false
   import weak public
   package option
   message enum oneof map
   reserved optional repeated
   double float int32 int64 uint32
   sint32 sint64 fixed32 fixed64 sfixed32 sfixed64
   bool string bytes
   service rpc))


;;    actual lexer here
(define tokenize
  (lexer-src-pos
   [(:or (:+ whitespace)
         comment)
    (return-without-pos (tokenize input-port))]

   [delim
    (hash-ref protobuf-delims
              (string-ref lexeme 0))]

   [float-lit
    (token-FLOATLIT (string->number lexeme))]

   [(:or dec-lit
         oct-lit
         hex-lit)
    (token-INTLIT (string->number lexeme))]

   [ident
    (hash-ref protobuf-keywords
              lexeme
              (λ () (token-IDENT lexeme)))]

   [(eof)
    (raise 'eof)]))

(define (in-tokens [port (current-input-port)])
  (let/ec k
    (let ([t (with-handlers ([(λ (x) (eq? 'eof x))
                              (λ (e) (k '()))])
               (tokenize port))])
      (stream-cons t (in-tokens port)))))
