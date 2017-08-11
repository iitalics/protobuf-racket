#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/contract)

(provide (contract-out
          (struct ast ([loc srcloc?])))
         ast:options?
         ast:type?
         empty-options)


(struct ast (loc) #:transparent)

(define-syntax-rule (define-ast-struct struct-name ([field contract] ...))
  (begin
    (define-struct (struct-name ast)
      (field ...)
      #:transparent)
    (provide (contract-out
              (struct (struct-name ast)
                ([loc srcloc?]
                 [field contract] ...))))))


(define-ast-struct ast:option
  ([extension     (or/c #f string?)]
   [names         (listof string?)]))

(define empty-options (hash))

(define ast:options?
  (hash/c ast:option? any/c))

(define-ast-struct ast:file
  ([path             path?]
   [package          ast:package?]
   [imports          (listof ast:import?)]
   [message-types    (listof ast:msg-type?)]
   [enum-types       (listof ast:enum-type?)]
   [options          ast:options?]))

(define-ast-struct ast:package
  ([name             string?]))

(define-ast-struct ast:import
  ([path             string?]
   [public?          boolean?]))

(define-ast-struct ast:msg-type
  ([name             string?]
   [fields           (listof ast:field?)]
   [nested-msgs      (listof ast:msg-type?)]
   [nested-enums     (listof ast:enum-type?)]
   [reserved         (listof (or/c exact-integer? string?))]
   [options          ast:options?]))

(define-ast-struct ast:field
  ([name             string?]
   [number           exact-integer?]
   [label            (or/c 'unknown 'optional 'required 'repeated)]
   [type             ast:type?]
   [options          ast:options?]))

(define-ast-struct ast:enum-type
  ([name             string?]
   [values           (listof ast:enum-val?)]
   [options          ast:options?]))

(define-ast-struct ast:enum-val
  ([name             string?]
   [number           exact-integer?]
   [options          ast:options?]))

(define ast:type?
  (or/c ast:msg-type?
        ast:enum-type?
        'int32 'uint32 'sint32 'fixed32 'sfixed32 'float
        'int64 'uint64 'sint64 'fixed64 'sfixed64 'double
        'bool 'string 'bytes))
