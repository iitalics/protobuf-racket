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
         protobuf-empty-tokens
         token-IDENT
         token-INTLIT
         token-FLOATLIT)

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
  [hex-lit (:seq #\0 (:or #\x #\X) (:* hex-dig))])

(define-lex-abbrevs
  [dec-pt (:seq #\. (:+ dec-dig))]
  [exponent (:seq (:or #\e #\E)
                  (:? (:or #\+ #\-))
                  (:+ dec-dig))])

(define-lex-abbrev float-lit
  (:seq (:+ dec-dig)
        (:or (:seq dec-pt exponent)
             exponent
             dec-pt)))


;    other
(define-lex-abbrev comment
  (:or (:seq "//"
             (complement (:: any-string #\newline any-string))
             #\newline)
       (:seq "//"
             (complement (:: any-string #\newline any-string)))))



;; generate lookup tables for lexer for delimeters / keywords
(define-syntax define-delims/kws
  (syntax-parser
    [(_ group-id
        #:delims delim-hash delim-lex-abbrev ((del TOK) ...)
        #:keywords kw-hash (kw ...)
        #:other (OTHER ...))
     #:with (KW-kw ...) (stx-map (λ (x) (format-id x "KW-~A" x)) #'[kw ...])
     #:with (kw/str ...) (stx-map (λ (x) (symbol->string (syntax->datum x))) #'[kw ...])
     #'(begin
         (define delim-hash (make-hash '([del . TOK] ...)))
         (define kw-hash (make-hash '([kw/str . KW-kw] ...)))
         (define-empty-tokens group-id (TOK ... KW-kw ... OTHER ...))
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
   true false
   import weak public
   package option
   message enum oneof map
   reserved required optional repeated
   double float int32 int64 uint32 uint64 sint32 sint64
   fixed32 fixed64 sfixed32 sfixed64
   bool string bytes
   service rpc)

  #:other (EOF))


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

   [dec-lit (token-INTLIT (string->number lexeme))]
   [oct-lit (token-INTLIT (string->number lexeme 8))]
   [hex-lit (token-INTLIT (string->number (substring lexeme 2) 16))]

   [ident
    (hash-ref protobuf-keywords
              lexeme
              (λ () (token-IDENT lexeme)))]

   [(eof) (token-EOF)]))

(define (EOF? x)
  (equal? (position-token-token x) 'EOF))

(define (in-tokens [port (current-input-port)]
                   #:include-eof? [inc-eof? #f])
  (make-do-sequence
   (λ ()
     (values tokenize
             values
             port
             #f
             (λ (t) (if (EOF? t) inc-eof? #t))
             (λ (p t) (not (EOF? t)))))))
