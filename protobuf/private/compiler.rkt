#lang racket/base
(require "ast.rkt"
         "dependencies.rkt"
         "descriptors.rkt"
         racket/class
         racket/set
         racket/match)

(provide current-file-desc-pool
         compile-root
         (struct-out exn:fail:compile))


(define-struct (exn:fail:compile exn:fail:read) ())

(define (raise-compile-error loc msg . fmts)
  (raise (make-exn:fail:compile
          (apply format (cons msg fmts))
          (current-continuation-marks)
          (list loc loc))))


(define current-file-desc-pool
  (make-parameter (make-hash)))

(define (get-file-desc resolved-path)
  (hash-ref (current-file-desc-pool)
            resolved-path))


;; fully compile root AST into file-descriptor% object
;; additionally adds it to the file desc pool
(define (compile-root root)
  (define fd (new file-descriptor%
                  [path (ast-source root)]
                  [package (ast:root-package root)]))

  (define deps
    (for/list ([imp (in-list (ast:root-imports root))])
      (get-file-desc (unbox (ast:import-resolved-path-box imp)))))

  (send fd set-dependencies deps)
  (send fd set-public-dependencies
        (for/list ([imp (in-list (ast:root-imports root))]
                   [dep (in-list deps)]
                   #:when (ast:import-public? imp))
          dep))

  (verify-no-type-conflicts
   (ast:root-messages root) ast:message-name
   (ast:root-enums root) ast:enum-name)

  (send fd set-message-types
        (map ast:message->descriptor%
             (ast:root-messages root)))

  (send fd set-enum-types
        (map ast:enum->enum-descriptor%
             (ast:root-enums root)))

  ;; TODO: compile file options
  ;; TODO: compile message fields
  ;; TODO: compile enum values

  (hash-set! (current-file-desc-pool)
             (ast-source root)
             root)
  fd)


;; convert ast to descriptor%
;; does not compile any fields, just nested types.
(define (ast:message->descriptor% ast)
  (define msgd (new descriptor%
                  [name (ast:message-name ast)]))

  (verify-no-type-conflicts
   (ast:message-nested-msgs ast) ast:message-name
   (ast:message-nested-enums ast) ast:enum-name)

  (send msgd set-nested-types
        (map ast:message->descriptor%
             (ast:message-nested-msgs ast)))
  (send msgd set-nested-enums
        (map ast:enum->enum-descriptor%
             (ast:message-nested-enums ast)))
  ;; TODO: compile message options
  msgd)


;; convert ast to enum-descriptor%
;; does not compile values
(define (ast:enum->enum-descriptor% enum)
  (new enum-descriptor%
       [name (ast:enum-name enum)])
  ;; TODO: compile enum options
  )


;; raises an error if any of the messages or enums have conflicting names
(define (verify-no-type-conflicts . args)
  (define names (mutable-set))
  (let trav ([args args])
    (match args
      ['() (void)]
      [(list* asts get-name rest)
       (for ([ast (in-list asts)])
         (when (set-member? names (get-name ast))
           (raise-compile-error (ast-loc ast)
                                "encountered multiple types named ~v"
                                (get-name ast)))
         (set-add! names (get-name ast)))
       (trav rest)])))
