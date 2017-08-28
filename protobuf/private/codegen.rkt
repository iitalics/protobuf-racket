#lang racket
(require "descriptors.rkt"
         "compiler.rkt"
         racket/syntax
         syntax/parse
         syntax/stx)

(provide (struct-out implementation)
         (struct-out renaming)
         codegen-file)


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
  (make-parameter '()))


;; find the 'implementation' object for the given
;; descriptor (by its name), or create one and add it
;; to current-impl-queue.
;;
;; TODO: will look for the impl in a syntax binding
;; named 'protobuf:<name>'
;;
;; get-or-queue-impl : string? -> impl?
(define (get-or-queue-impl name)
  (cond
    [(findf (λ (impl) (equal? (implementation-name impl) name))
            (current-impl-queue))
     => values]

    [else
     (let ([impl (implementation name
                                 (generate-temporary #'%impl:pred)
                                 (generate-temporary #'%impl:default)
                                 '())])
       (current-impl-queue (cons impl (current-impl-queue)))
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
;; implement-enum : implementation? dsctor:enum? -> (listof renaming?) stx?
(define (implement-enum impl descriptor)
  (match-define (struct dsctor:enum (loc short-name opts vals))
    descriptor)

  (syntax-parse (generate-temporaries #'[%enum->number %number->enum %enum?])
    [(e->n n->e)
     #:with e? (implementation-pred-id impl)
     #:with e-default (implementation-default-id impl)

     #:with ([v-name v-num] ...)
            (map (λ (ev)
                   (list (dsctor-name ev)
                         (dsctor:enum-value-number ev)))
                 vals)

     #:with [first-v-name . _] #'[v-name ...]

     (values (list (renaming #'e->n "~a->number")
                   (renaming #'n->e "number->~a")
                   (renaming #'e?   "~a?")
                   (renaming #'e-defalut "default-~a"))

             #'(define-values (e->n n->e e? e-default)
                 (values (λ (x)
                           (case x
                             [(v-name) 'v-num] ...
                             [else #f]))

                         (λ (n)
                           (case n
                             [(v-num) 'v-name] ...
                             [else #f]))

                         (or/c 'v-name ...)
                         'first-v-name)))]))


(define (codegen-file file-dsc)
  (values #'() '()))
