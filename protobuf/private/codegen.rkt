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
     #:with e-default (implementation-default-id impl)

     #:with ([enum-val-sym enum-val-num] ...)
            (map (λ (ev)
                   (list (string->symbol (dsctor-name ev))
                         (dsctor:enum-value-number ev)))
                 vals)

     (values (list (renaming #'e->n "~a->number")
                   (renaming #'n->e "number->~a")
                   (renaming #'e?   "~a?")
                   (renaming #'e-default "default-~a"))

             #'(define-values (e->n n->e e? e-default)
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
