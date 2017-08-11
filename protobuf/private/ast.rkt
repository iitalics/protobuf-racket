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
   [names         (listof string?)]
   [value         any/c]))

(define empty-options '())

(define ast:options?
  (listof ast:option?))

(define-ast-struct ast:file
  ([path             path?]
   [package          ast:package?]
   [imports          (listof ast:import?)]
   [messages         (listof ast:message?)]
   [enums            (listof ast:enum?)]
   [options          ast:options?]))

(define-ast-struct ast:package
  ([name             string?]))

(define-ast-struct ast:import
  ([path             string?]
   [public?          boolean?]))

(define-ast-struct ast:message
  ([name             string?]
   [fields           (listof ast:field?)]
   [nested-msgs      (listof ast:message?)]
   [nested-enums     (listof ast:enum?)]
   [reserved         (listof (or/c exact-integer?
                                   string?
                                   ast:range?))]
   [options          ast:options?]))

(define-ast-struct ast:field
  ([name             string?]
   [number           exact-integer?]
   [label            (or/c 'unknown 'optional 'required 'repeated)]
   [type             ast:type?]
   [options          ast:options?]))

(define-ast-struct ast:enum
  ([name             string?]
   [values           (listof ast:enum-val?)]
   [options          ast:options?]))

(define-ast-struct ast:enum-val
  ([name             string?]
   [number           exact-integer?]
   [options          ast:options?]))

(define-ast-struct ast:range
  ([min              exact-integer?]
   [max              (or/c exact-integer? 'max)]))

(define-ast-struct ast:map-type
  ([key-type         ast:type?]
   [val-type         ast:type?]))

(define ast:type?
  (or/c 'int32 'uint32 'sint32 'fixed32 'sfixed32 'float
        'int64 'uint64 'sint64 'fixed64 'sfixed64 'double
        'bool 'string 'bytes
        string?             ; unresolved type name
        ast:map-type?))
