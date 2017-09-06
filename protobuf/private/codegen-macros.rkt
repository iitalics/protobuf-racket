#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     (only-in racket/syntax format-id)
                     (only-in racket/sequence in-syntax)
                     "descriptors.rkt"
                     "codegen.rkt"
                     "compiler/dependencies.rkt"
                     "compiler/compiler.rkt"))

(provide generate-protobuf)

(begin-for-syntax
  (define-syntax-class maybe-renamed
    #:attributes (orig into)
    (pattern orig:id #:with into #'orig)
    (pattern [orig:id into:id])))

(define-syntax generate-protobuf
  (syntax-parser
    [(_ (~or (~seq #:source source-file:str)
             (~seq #:extra-proto-path extra-proto-path:str)
             ) ...
        to-gen:maybe-renamed ...+)

     ;; TODO: (parameterize ([all-descriptors (make-hash)])  ... )

     #:do [; parse input files: path-string? -> ast?
           (define-values (given-asts all-sorted-asts)
             (parameterize ([extra-proto-paths
                             (syntax->datum #'(extra-proto-path ...))])
               (parse+dependencies
                (syntax->datum #'(source-file ...)))))

           ; compile the ASTs into descriptors: ast? -> dsctor?
           (for-each compile-root all-sorted-asts)

           (define-values (impls-to-export impl=>stx)
             (parameterize ([current-impl-queue (make-hash)])
               ; queue up initial descriptors to be code generated
               ; these are the ones we'll export
               (define impls-to-export
                 (for/list ([src (in-syntax #'[to-gen ...])]
                            [orig-id (in-syntax #'[to-gen.orig ...])])
                   (get-or-queue-impl
                    (id->type-dsctor-name orig-id #:source-stx src))))

               ; keeping polling the queue while there are more things
               ; to implement.
               ; TODO: more efficient way? (e.g. not as much hashing & an actual queue)
               (define impl=>stx (make-hash))
               (let poll ()
                 (cond
                   [(eq? (hash-count impl=>stx)
                         (hash-count (current-impl-queue)))
                    (values impls-to-export impl=>stx)]

                   [else
                    (for ([impl (in-hash-values (current-impl-queue))])
                      (hash-ref! impl=>stx
                                 impl
                                 (λ () (implement impl))))
                    (poll)]))))]

     ; extract the syntax for all the generated impls
     #:with [impl-stx ...] (hash-values impl=>stx)

     ; create rename transformers from exported identifiers
     ;   renaming? -> syntax?
     #:with [rename-stx ...]
     (for/list ([impl (in-list impls-to-export)]
                [into-id (in-syntax #'[to-gen.into ...])]
                #:when #t ; this causes the next sequence to be nested instead of parallel
                [rnm (in-list (implementation-exports impl))])
       (with-syntax ([old-id (renaming-id rnm)]
                     [new-id (format-id into-id (renaming-fmt rnm) into-id)])
         #'(define-syntax new-id
             (make-rename-transformer #'old-id))))

     #'(begin
         impl-stx ...
         rename-stx ...)]))


(define-for-syntax (id->type-dsctor-name id #:source-stx src)
  (let* ([sym (syntax-e id)]
         [str (symbol->string sym)]
         [fq (string-append "." str)]
         [dsc (hash-ref (all-descriptors) fq #f)])

    (cond
      [(or (dsctor:enum? dsc)
           (dsctor:message? dsc)
           )
       fq]

      [(not dsc)
       (raise-syntax-error #f
                           (format "no such protobuf type ~v" str))]

      [dsc
       (raise-syntax-error #f
                           (format "~v is not a valid protobuf type (enum or message)" str))])))
