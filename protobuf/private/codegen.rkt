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


;; impls that need must be generated, mapped by the name of the
;; descriptor being implemented.
;;
;; current-impl-queue : (parameterof (hash fq-name? => impl?))
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
;;   {msg}?                   : any -> bool
;;   default-{msg}            : msg
;;   make-{msg}               : #:{field} val ... -> msg
;;
;;   for regular fields:
;;     {msg}-{field}          : msg -> val
;;
;;   for oneofs:
;;     {msg}-{oneof}-case     : msg -> symbol
;;     {msg}-has-{field}?     : msg -> bool
;;     {msg}-{field}          : msg -> val
;;
;;   for repeated fields:
;;     {msg}-{field}          : msg -> (listof val)
;;
;;   for map fields:
;;     {msg}-{field}          : msg -> (immutable-hash key => val)
;;     {msg}-{field}-ref      : msg key [val/thunk] -> val   [TODO]
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

     #:do [(define-values (oneof-fields regular-fields)
             (partition dsctor:field-oneof fields))

           (define map-fields
             (filter map-field? fields))

           (define fld-index-start
             (* 2 (length oneofs)))]

     #:with [(oo-index
              oo-kw
              oo-kw-case
              oo-init-arg
              oo-init-arg-case
              oo-no-arg-msg) ...]
            (for/list ([dsc (in-list oneofs)]
                       [i (in-naturals)])
              (list #`#,(* 2 i)
                    (string->keyword (dsctor-name dsc))
                    (string->keyword (format "~a-case" (dsctor-name dsc)))
                    (generate-temporary #'%oo-init)
                    (generate-temporary #'%oo-init-case)
                    (format "keyword argument #:~a must be supplied when #:~a-case is"
                            (dsctor-name dsc)
                            (dsctor-name dsc))))

     #:with [(fld-index
              fld-kw
              fld-init-arg
              fld-default-expr) ...]
            (for/list ([dsc (in-list regular-fields)]
                       [i (in-naturals fld-index-start)])
              (list #`#,i
                    (string->keyword (dsctor-name dsc))
                    (generate-temporary #'%msg-init)
                    (type-default-stx (dsctor:field-type dsc)
                      #:repeated? (dsctor:field-repeated? dsc))))

     #:with [(oofld-index
              oofld-oneof-index
              oofld-case-name
              oofld-default-expr) ...]
            (for/list ([dsc (in-list oneof-fields)])
              (define i
                (index-of oneofs (dsctor:field-oneof dsc)))
              (list #`#,(add1 (* 2 i))
                    #`#,      (* 2 i)
                    (string->symbol (dsctor-name dsc))
                    (type-default-stx (dsctor:field-type dsc))))

     #:with [(mapfld-index
              mapfld-val-default-expr) ...]
            (for/list ([dsc (in-list regular-fields)]
                       [i (in-naturals fld-index-start)]
                       #:when (map-field? dsc))
              (let* ([entry-dsc (hash-ref (all-descriptors) (dsctor:field-type dsc))]
                     [value-dsc (second (dsctor:message-fields entry-dsc))])
                (list #`#,i
                      (type-default-stx (dsctor:field-type value-dsc)))))

     #:with [(kw+arg ...) ...]
            #'[(fld-kw [fld-init-arg fld-default-expr]) ...
               (oo-kw-case [oo-init-arg-case #f]) ...
               (oo-kw [oo-init-arg (and oo-init-arg-case
                                        (error 'oo-no-arg-msg))]) ...]

     #:with [(oo-inits ...) ...]
            #'[(oo-init-arg-case oo-init-arg) ...]

     #:with fullname-sym (string->symbol (implementation-name impl))
     #:with (m-get-oo-case ...) (generate-temporaries oneofs)
     #:with (m-get-fld ...) (generate-temporaries regular-fields)
     #:with (m-has-oofld? ...) (generate-temporaries oneof-fields)
     #:with (m-get-oofld ...) (generate-temporaries oneof-fields)
     #:with (m-mapfld-ref ...) (generate-temporaries map-fields)

     (values
      (append (list (renaming #'m? "~a?")
                    (renaming #'def-m "default-~a")
                    (renaming #'mk-m "make-~a"))

              (stx-map (λ (id dsc) (renaming id (format "~~a-~a" (dsctor-name dsc))))
                       #'[     m-get-fld ...   m-get-oofld ...]
                       (append regular-fields  oneof-fields))

              (stx-map (λ (id dsc) (renaming id (format "~~a-~a-case" (dsctor-name dsc))))
                       #'[m-get-oo-case ...]
                       oneofs)

              (stx-map (λ (id dsc) (renaming id (format "~~a-has-~a?" (dsctor-name dsc))))
                       #'[m-has-oofld? ...]
                       oneof-fields)

              (stx-map (λ (id dsc) (renaming id (format "~~a-~a-ref" (dsctor-name dsc))))
                       #'[m-mapfld-ref ...]
                       map-fields))

      #`(begin
          (define-values (msg-strct make-strct strct? idx-get idx-set!)
            (make-struct-type 'fullname-sym
                              #f
                              #,(+ (* 2 (length oneofs))
                                   (length regular-fields))
                              0))

          (define (mk-m kw+arg ... ...)
            (make-strct oo-inits ... ...
                        fld-init-arg ...))

          (define (m-get-oo-case m)
            (idx-get m oo-index)) ...

          (define (m-get-fld m)
            (idx-get m fld-index)) ...

          (define (m-has-oofld? m)
            (eq? 'oofld-case-name
                 (idx-get m oofld-oneof-index))) ...

          (define (m-get-oofld m)
            (if (m-has-oofld? m)
                (idx-get m oofld-index)
                oofld-default-expr)) ...

          (define (m-mapfld-ref m k [v (λ () mapfld-val-default-expr)])
            (hash-ref (idx-get m mapfld-index)
                      k v)) ...

          (define m? strct?)
          (define def-m (mk-m))))]))


(define (builtin-type-default-stx ty)
  (case ty
    [(bool)     #'#f]
    [(string)   #'""]
    [(bytes)    #'#""]
    [(float double) #'0.0]
    [else       #'0]))

(define (type-default-stx ty #:repeated? [repeated? #f])
  (cond
    [repeated?    (if (map-entry-type? ty) #'#hash() #''())]
    [(string? ty) (implementation-default-id (get-or-queue-impl ty))]
    [else         (builtin-type-default-stx ty)]))

(define (map-field? dsc)
  (and (dsctor:field-repeated? dsc)
       (map-entry-type? (dsctor:field-type dsc))))

(define (map-entry-type? ty)
  (and (string? ty)
       (dsctor-option (hash-ref (all-descriptors) ty)
                      "map_entry"
                      #f)))
