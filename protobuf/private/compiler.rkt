#lang racket
(require "ast.rkt"
         "dependencies.rkt"
         "descriptors.rkt")

(provide current-file-desc-pool
         current-names-in-use
         compile-root
         (struct-out exn:fail:compile))


(define-struct (exn:fail:compile exn:fail:read) ())

(define (raise-compile-error loc msg . fmts)
  (raise (make-exn:fail:compile
          (apply format (cons msg fmts))
          (current-continuation-marks)
          (list loc loc))))


;; maps resolved paths to generated file descriptors
;; (hash complete-path? => (is-a?/c file-descriptor%))
(define current-file-desc-pool
  (make-parameter (make-hash)))

(define (get-file-desc resolved-path)
  (hash-ref (current-file-desc-pool)
            resolved-path))


;; maps full names (e.g. "google.protobuf.Any") to the file
;; they were declared in. includes package paths (e.g. "google" and "google.protobuf")
;; which map to the file they were first declared in
;; (hash string? => complete-path?)
(define current-names-in-use
  (make-parameter (make-hash)))

(define (name-append prefix post)
  (if (equal? prefix "")
      post
      (string-append prefix "." post)))

;; errors if the name is already in use, otherwise
;; registers it and returns (void)
(define (check-in-use name loc)
  (cond
    [(hash-ref (current-names-in-use) name #f)
     => (λ (use-path)
          (raise-compile-error loc
                               "name ~v already bound in ~a"
                               name
                               (path->string (normalize-path use-path))))]

    [else
     (hash-set! (current-names-in-use)
                name
                (srcloc-source loc))]))


;; fully compile root AST into file-descriptor% object
;; additionally adds it to the file desc pool
(define (compile-root root)
  (define fd (new file-descriptor%
                  [path (ast-source root)]
                  [package (ast:root-package root)]
                  [origin-ast root]))

  ; register package path
  (let ([pkg-path (send fd get-package-path)])
    (for ([i (in-range (length pkg-path))])
      (hash-ref! (current-names-in-use)
                 (take pkg-path (add1 i))
                 (ast-source root))))

  (define deps
    (for/list ([imp (in-list (ast:root-imports root))])
      (get-file-desc (unbox (ast:import-resolved-path-box imp)))))

  (send fd set-dependencies deps)

  (send fd set-public-dependencies
        (for/list ([imp (in-list (ast:root-imports root))]
                   [dep (in-list deps)]
                   #:when (ast:import-public? imp))
          dep))

  (send fd set-message-types
        (for/list ([x (in-list (ast:root-messages root))])
          (ast:message->descriptor% x (ast:root-package root))))

  (send fd set-enum-types
        (for/list ([x (in-list (ast:root-enums root))])
          (ast:enum->enum-descriptor% x (ast:root-package root))))

  ;; TODO: compile file options
  ;; TODO: compile message fields
  ;; TODO: compile enum values

  (hash-set! (current-file-desc-pool)
             (ast-source root)
             root)
  fd)


;; convert ast to descriptor%
;; does not compile any fields, just nested types.
(define (ast:message->descriptor% ast name-prefix)
  (define msgd (new descriptor%
                  [name (ast:message-name ast)]))

  (let ([nested-prefix (name-append name-prefix (ast:message-name ast))])
    (check-in-use nested-prefix (ast-loc ast))

    (send msgd set-nested-types
          (for/list ([x (in-list (ast:message-nested-msgs ast))])
            (ast:message->descriptor% x nested-prefix)))

    (send msgd set-nested-enums
          (for/list ([x (in-list (ast:message-nested-enums ast))])
            (ast:enum->enum-descriptor% x nested-prefix))))

  ;; TODO: compile message options
  msgd)


;; convert ast to enum-descriptor%
;; does not compile values
(define (ast:enum->enum-descriptor% ast name-prefix)
  (new enum-descriptor%
       [name (ast:enum-name ast)])

  (check-in-use (name-append name-prefix (ast:enum-name ast))
                (ast-loc ast))

  ;; TODO: compile enum options
  )
