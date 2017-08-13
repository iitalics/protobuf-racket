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

  ;; TODO: populate dependencies
  ;; TODO: create type descriptors
  ;; TODO: compile file options
  ;; TODO: compile message fields
  ;; TODO: compile enum values

  (hash-set! (current-file-desc-pool)
             (ast-source root)
             root)
  fd)
