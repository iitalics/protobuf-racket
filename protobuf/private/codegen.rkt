#lang racket
(require "descriptors.rkt"
         "compiler.rkt"
         racket/syntax
         syntax/parse
         syntax/stx
         (for-template racket/base
                       racket/contract/base))

(provide (struct-out implementation)
         (struct-out renaming)
         current-impl-queue
         get-or-queue-impl
         implement)


;; a struct representing an implementation of a protobuf type
;; 'name' is the fully qualified name of the type, the '*-id'
;; fields are the names of identifiers of interest (e.g. type
;; predicate, default value
(struct implementation
  (name
   pred-id
   default-id
   [exports #:mutable]))

;; an identifier, and a "template" (format string) for how
;; to rename the identifier when exporting
;; the identifier is usually a temporary, and the fmt will
;; be something like "~a->number" (format string taking one
;; parameter that is to be the name of the type)
(struct renaming
  (id fmt))


;; list of impls that need to be actually defined during
;; this codegen
;;
;; current-impl-queue : (parameterof (listof impl?))
(define current-impl-queue
  (make-parameter (make-hash)))


;; find the 'implementation' object for the given
;; descriptor (by its name), or create one and add it
;; to current-impl-queue.
;;
;; TODO: will look for the impl in a syntax binding
;; named 'protobuf:<name>'
;;
;; get-or-queue-impl : fq-name? -> impl?
(define (get-or-queue-impl fq)
  (cond
    [(hash-ref (current-impl-queue) fq #f)
     => values]

    [else
     (let ([impl (implementation fq
                                 (generate-temporary #'%impl:pred)
                                 (generate-temporary #'%impl:default)
                                 '())])
       (hash-set! (current-impl-queue) fq impl)
       impl)]))


;; actually implement the given 'implementation',
;; setting the exports, and returning the syntax
;; that defines all of the new identifiers
;;
;; implement : implementation? -> stx?
(define (implement impl)
  (define dsc
    (hash-ref (all-descriptors)
              (implementation-name impl)))

  (define-values (exports stx)
    (cond
      [(dsctor:enum? dsc) (implement-enum impl dsc)]
      [(dsctor:message? dsc) (implement-message impl dsc)]
      [else
       (error "idk how to implement"
              (implementation-name impl))]))

  (set-implementation-exports! impl exports)
  stx)


;; implement an enum type. returns the list of exports,
;; as well as the syntax to define all the new identifiers.
;;
;; defines:
;;   {enum}?           : any -> bool
;;   default-{enum}    : symbol
;;   {enum}->number    : symbol/any -> integer, or #f if invalid
;;   number->{enum}    : integer/any -> symbol, or #f if invalid
;;
;; implement-enum : implementation? dsctor:enum? -> (listof renaming?) stx?
(define (implement-enum impl descriptor)
  (match-define (struct dsctor:enum (_ _ _ vals))
    descriptor)

  (syntax-parse (generate-temporaries #'(  %enum->number %number->enum  ))
    [(  e->n n->e  )
     #:with e? (implementation-pred-id impl)
     #:with def-e (implementation-default-id impl)

     #:with ([enum-val-sym enum-val-num] ...)
            (map (λ (ev)
                   (list (string->symbol (dsctor-name ev))
                         (dsctor:enum-value-number ev)))
                 vals)

     (values
      (list (renaming #'e->n "~a->number")
            (renaming #'n->e "number->~a")
            (renaming #'e?   "~a?")
            (renaming #'def-e "default-~a"))

      #'(define-values (e->n n->e e? def-e)
          (values (λ (x)
                    (case x
                      [(enum-val-sym) 'enum-val-num] ...
                      [else #f]))

                  (λ (n) ;; TODO: add contract?
                    (case n
                      [(enum-val-num) 'enum-val-sym] ...
                      [else #f]))

                  (or/c 'enum-val-sym ...)
                  (begin0 'enum-val-sym ...))))]))




;; implement a message type. returns the list of exports,
;; as well as the syntax to define all the new identifiers.
;;
;; defines:
;;                                                    [TODO]
;;   {msg}?                   : any -> bool
;;   default-{msg}            : msg
;;   make-{msg}               : #:{field} field ... -> msg
;;
;;   for regular fields:                              [TODO]
;;     {msg}-{field}          : msg -> field
;;
;;   for oneofs:                                      [TODO]
;;     (same as regular fields)
;;       +
;;     {msg}-has-{oneof}?     : msg -> bool
;;     {msg}-{oneof}-case     : msg -> symbol
;;
;;   for repeated fields:                             [TODO]
;;     {msg}-{field}          : msg -> (listof field)
;;
;;   for map fields:                                  [TODO]
;;     {msg}-{field}          : msg -> (immutable-hash key => val)
;;
;; implement-enum : implementation? dsctor:enum? -> (listof renaming?) stx?
(define (implement-message impl descriptor)
  (match-define (struct dsctor:message (_ _ _ fields oneofs _ _ _ _))
    descriptor)

  (syntax-parse (generate-temporaries #'(  %msg-strct %mk-msg %msg? %get %set  ))
    [(  msg-strct make-strct strct? idx-get idx-set!  )
     #:with m? (implementation-pred-id impl)
     #:with def-m (implementation-default-id impl)
     #:with mk-m (generate-temporary #'%mk-m)

     #:do [(define (regular-field dsc idx)
             (with-syntax ([init-var (generate-temporary #'%msg-init)]
                           [default (type-default-stx (dsctor:field-type dsc))]
                           [KW (string->keyword (dsctor-name dsc))])
               (list #'[KW [init-var default]]
                     #'init-var)))

           (define (repeated-field dsc idx)
             (with-syntax ([init-var (generate-temporary #'%msg-init)]
                           [KW (string->keyword (dsctor-name dsc))])
               (list #'[KW [init-var '()]]
                     #'init-var)))
           ]

     #:with fullname-sym (string->symbol (implementation-name impl))
     #:with (field-i ...) (range (length fields))
     #:with (m-get ...) (generate-temporaries #'(field-i ...))

     #:with [([in-arg ...] init-expr) ...]
            (for/list ([field-dsc (in-list fields)]
                       [idx (in-syntax #'[field-i ...])]
                       #:when (not (dsctor:field-oneof field-dsc)))
              (cond
                [(dsctor:field-repeated? field-dsc)
                 (repeated-field field-dsc idx)]
                [else
                 (regular-field field-dsc idx)]))

     (values
      (list* (renaming #'m? "~a?")
             (renaming #'def-m "default-~a")
             (renaming #'mk-m "make-~a")
             (stx-map (λ (id field-dsc)
                        (renaming id (format "~~a-~a" (dsctor-name field-dsc))))
                      #'[m-get ...]
                      fields))

      #`(begin
          (define-values (msg-strct make-strct strct? idx-get idx-set!)
            (make-struct-type 'fullname-sym #f #,(length fields) 0))

          (define (mk-m in-arg ... ...)
            (make-strct init-expr ...))

          (define (m-get m)
            (idx-get m 'field-i)) ...

          (define m? strct?)
          (define def-m (mk-m))))]))



(define (builtin-type-default-stx ty)
  (case ty
    [(bool)     #''#f]
    [(string)   #''""]
    [(bytes)    #''#""]
    [(float double) #''0.0]
    [else       #''0]))

(define (type-default-stx ty)
  (cond
    [(string? ty)
     (implementation-default-id (get-or-queue-impl ty))]
    [else
     (builtin-type-default-stx ty)]))
