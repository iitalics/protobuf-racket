#lang racket
(require "ast.rkt"
         "dependencies.rkt"
         "descriptors.rkt")

(provide (struct-out exn:fail:compile)
         fq-name?
         ur-name?
         all-descriptors
         recursive-descent
         compile-root)


;; compiler-specific exception type
(define-struct (exn:fail:compile exn:fail:read) ())

;; raise compiler error with string formatting
(define (raise-compile-error loc msg . fmts)
  (raise (make-exn:fail:compile
          (apply format (cons msg fmts))
          (current-continuation-marks)
          (list loc loc))))




;; fq-name = fully qualified name, e.g. ".google.protobuf.Any"
(define (fq-name? s)
  (and (string? s)
       (string-prefix? s ".")))

;; ur-name = unresolved name, e.g. "protobuf.Any"
(define (ur-name? s)
  (string? s))


;; string? string? -> string?
(define (qualify-in-scope name [scope (current-scope)])
  (string-append scope "." name))

;; break scope into subscopes, e.g.
;;   (in-subscopes ".a.b.c") => ".a", ".a.b", ".a.b.c"
;;
;; in-subscopes : string? -> (sequenceof string?)
(define (in-subscopes scope)
  (in-sequences (for/stream ([c (in-string scope 1)]
                             [i (in-naturals 1)]
                             #:when (char=? #\. c))
                  (substring scope 0 i))
                (in-value scope)))


;; descriptors that are finished being compiled
;;   (e.g. from a previous pass)
;;
;; fq-name? => dsctor?
(define all-descriptors
  (make-parameter (make-hash)))

;; descriptors being compiled that need additional passes
;; (listof (cons fq-name? dsctor?))
(define current-unresolved-descriptors
  (make-parameter '()))

;; current scope string, e.g. "" or ".google.protobuf"
;; (or/c "" fq-name?)
(define current-scope
  (make-parameter ""))

;; fully-qualified name of current oneof
;; for use in compiling fields nested in a oneof
;; (or/c #f fq-name?)
(define current-oneof-fq-name
  (make-parameter #f))



(define (add-unresolved! dsc
                         [fq-name (qualify-in-scope (dsctor-name dsc))]
                         [loc (dsctor-loc dsc)])
  (cond
    [(hash-ref (all-descriptors) fq-name #f)
     =>
     (λ (prev-dsc)
       (raise-compile-error
        loc
        "name ~a already used by file ~a"
        fq-name
        (shorten-file-path (dsctor-source-file-path prev-dsc))))]

    [else
     (hash-set! (all-descriptors) fq-name dsc)
     (current-unresolved-descriptors
      (cons (cons fq-name dsc)
            (current-unresolved-descriptors)))
     fq-name]))


(define (recursive-descent ast)
  (error 'unimplemented))

(define (compile-root ast)
  (dsctor:file (ast-loc ast)
               #f
               (ast:root-options ast)
               (ast:root-package ast)
               '() '() '() '() '()))



#|
-- first pass: (recursive decent)

instantiate skeleton descriptors (as described above)
check for name conflicts
insert (cons AST dsc) into current-unresolved-descriptors

dsctor-options = (listof <a st:option>)
dsctor:message-fields/oneofs = (listof <fq-name>)
dsctor:message-nested-* = (listof <fq-name>)
dsctor:message-reserved-* = #f
dsctor:field-type = <uqname>


-- second pass: (iterate current-unresolved-descriptors)

parse options
parse reserved fields, check against in-use names
check for duplicate field/value numbers
resolve types

dsctor-options = (hash string? => any)
dsctor:reserved-names = (setof string?)
dsctor:reserved-index? = (-> integer? boolean?)
dsctor:field-type = <fq-name>


-- third pass: (iterate current-unresolved-descriptors)

dsctor:message-fields = (listof dsctor:field?)
dsctor:message-oneofs = (listof dsctor:oneof?)

create finalized descriptors and insert into all-descriptors
create dsctor:file and return it

|#
