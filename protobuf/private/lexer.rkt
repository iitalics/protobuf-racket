#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     racket/syntax)
         racket/stream
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide tokenize ; (port -> position-token)
         in-protobuf-tokens
         protobuf-tokens
         protobuf-empty-tokens
         token-IDENT
         token-INTLIT
         token-FLOATLIT
         token-STRINGLIT
         protobuf-token->string)

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
             (complement (:seq any-string #\newline any-string))
             #\newline)
       (:seq "//"
             (complement (:seq any-string #\newline any-string)))))

(define-lex-abbrevs
  [chr-esc (:seq #\\ (:or #\a #\b #\f #\n #\r #\t #\v #\' #\" #\\))]
  [hex-esc (:seq #\\ #\x hex-dig hex-dig)]
  [oct-esc (:seq #\\ oct-dig oct-dig oct-dig)])


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
   [#\= EQ]
   [#\. DOT])

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


(define-tokens protobuf-tokens
  (IDENT INTLIT FLOATLIT STRINGLIT))


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

   [(:or #\' #\")
    (unescape input-port start-pos (string-ref lexeme 0))]

   [(eof) (token-EOF)]))


(define (unescape port start-pos quot)
  (define lex
    (lexer
     [(:or chr-esc
           oct-esc
           hex-esc)
      (unescape-single lexeme)]


     [(:or #\' #\")
      (if (eqv? (string-ref lexeme 0) quot)
          eof
          (string-ref lexeme 0))]

     [(:& any-char
          (complement (:or #\newline #\nul)))
      (string-ref lexeme 0)]

     [(eof) (error "unclosed quote in string literal")]))

  (token-STRINGLIT
   (list->string
    (for/list ([c (in-port lex port)]) c))))


;; assuming that the given string is a valid escape sequence (e.g. \n or \x41)
;; unescape-single : string -> char
;;   (unescape-single "\\x41") = #\A
;;   (unescape-single "\\141") = #\a
;;   etc.
(define (unescape-single s)
  (case (string-ref s 1)
    [(#\a) #\u7]
    [(#\b) #\backspace]
    [(#\f) #\page]
    [(#\n) #\newline]
    [(#\r) #\return]
    [(#\t) #\tab]
    [(#\v) #\vtab]
    [(#\x) (integer->char (string->number (substring s 2) 16))]
    [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
     (integer->char (string->number (substring s 1) 8))]
    [else (string-ref s 1)]))




;; tokens as a sequence
(define (in-protobuf-tokens [port (current-input-port)]
                            #:include-eof? [inc-eof? #f])
  (make-do-sequence
   (λ ()
     (values tokenize
             values
             port
             #f
             (λ (t) (if (EOF? t) inc-eof? #t))
             (λ (p t) (not (EOF? t)))))))

(define (EOF? x)
  (equal? (position-token-token x) 'EOF))


;; pretty printing
(define (protobuf-token->string sym)
  (case sym
    [(IDENT) "<identifier>"]
    [(INTLIT) "<integer>"]
    [(FLOATLIT) "<float>"]
    [(STRINGLIT) "<string>"]
    [else
     "<other (TODO)>"]))
