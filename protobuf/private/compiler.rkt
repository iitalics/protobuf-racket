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


;; append a '.'-delimeted name onto another, e.g.
;;   (name-append "a.b" "c") = "a.b.c"
;;   (name-append "" "d.e") = "d.e"
(define (name-append pre post)
  (if (equal? pre "")
      post
      (string-append pre "." post)))

;; break scope into subscopes, e.g.
;;   (in-subscopes "a.b.c") => "a", "a.b", "a.b.c"
;;
;; in-subscopes : string? -> (sequenceof string?)
(define (in-subscopes scope)
  (in-sequences (for/stream ([c (in-string scope)]
                             [i (in-naturals)]
                             #:when (char=? #\. c))
                  (substring scope 0 i))
                (in-value scope)))
