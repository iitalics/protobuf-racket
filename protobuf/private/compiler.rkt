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

(define (shorten-file-path p)
  (let ([paths (list p (find-relative-path (current-directory) p))])
    (argmin string-length
            (map path->string paths))))



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


;; first pass: (recursive decent)
;;
;; instantiate skeleton descriptors (as described above)
;; check for name conflicts
;; insert (cons AST dsc) into current-unresolved-descriptors
;;
;;   dsctor-options = (listof <a st:option>)
;;   dsctor:message-fields/oneofs = (listof <fq-name>)
;;   dsctor:message-nested-* = (listof <fq-name>)
;;   dsctor:message-reserved-* = #f
;;   dsctor:field-type = <uqname>
;;
;; recursive-descent : ast? -> fq-name?
(define (recursive-descent ast)
  (match ast

    ;; -[ message ]-
    [(struct ast:message (loc name field-asts oneof-asts map-field-asts
                          nested-msg-asts nested-enum-asts reserved opts))

     (define fq-name (qualify-in-scope name))

     ;; parameterize current-scope before message creates its own scope
     (add-unresolved!
      (parameterize ([current-scope fq-name])
        (let* ([fields (map recursive-descent field-asts)]
               [oneofs (map recursive-descent oneof-asts)]
               [oneof-fields
                ;; traverse fields nested in oneofs
                (for/list ([oo-fq (in-list oneofs)]
                           [oo-ast (in-list oneof-asts)])
                  (parameterize ([current-oneof-fq-name oo-fq])
                    (map recursive-descent
                         (ast:oneof-fields oo-ast))))]
               [all-fields (append* fields oneof-fields)]
               [nested-msgs (map recursive-descent nested-msg-asts)]
               [nested-enums (map recursive-descent nested-enum-asts)])

          (dsctor:message loc
                          name
                          opts
                          all-fields
                          oneofs
                          nested-msgs
                          nested-enums
                          #f #f ; reserved names/indices
                          ))))]

    ;; -[ field ]-
    [(struct ast:field (loc name number label type opts))
     (add-unresolved!
      (dsctor:field loc
                    name
                    opts
                    type
                    number
                    (eq? label 'repeated)
                    (current-oneof-fq-name)))]


    ;; -[ oneof ]-
    [(struct ast:oneof (loc name field-asts))
     (add-unresolved!
      (dsctor:oneof loc name '()))]


    ;; TODO: map field


    ;; -[ enum ]-
    [(struct ast:enum (loc name val-asts opts))
     (let ([vals (map recursive-descent val-asts)])
       (add-unresolved!
        (dsctor:enum loc name opts vals)))]


    ;; -[ enum-val ]-
    [(struct ast:enum-val (loc name number opts))
     (add-unresolved!
      (dsctor:enum-value loc name opts number))]))




(define (compile-root root-ast)
  (define pkg (ast:root-package root-ast))

  ;; TODO: dependency set

  (parameterize ([current-scope (if (equal? pkg "")
                                    ""
                                    (string-append "." pkg))]
                 [current-unresolved-descriptors '()]
                 [current-oneof-fq-name #f])

    ;; first pass
    (for-each recursive-descent (ast:root-messages root-ast))
    (for-each recursive-descent (ast:root-enums root-ast))

    (dsctor:file (ast-loc root-ast)
                 #f
                 (ast:root-options root-ast)
                 (ast:root-package root-ast)
                 ;; TODO: second pass
                 '() '() ; deps / public-deps
                 '() ; messages
                 '() ; enums
                 (map cdr (current-unresolved-descriptors)))))





#|
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
