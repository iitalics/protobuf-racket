#lang racket
(require "ast.rkt"
         "dependencies.rkt"
         "descriptors.rkt")


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

