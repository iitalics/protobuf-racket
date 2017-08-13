#lang racket
(require "descriptors.rkt"
         racket/generic)

(provide scope?
         type-in-scope
         subscope
         resolve-type)

(define-generics scope
  [type-in-scope scope name]
  [subscope scope name])


;; resolve a type by the given name by searching the list of scopes.
;; the name is the entire string e.g. "Any" or "protobuf.Any"
;; or ".google.protobuf.Any"
(define (resolve-type type-name scopes)
  (cond
    [(string-prefix? type-name ".")
     (match (string-split (substring type-name 1) ".")
       [(list path ... name)
        ;; assuming that the last entry in scopes is the base scope
        ;; which contains all packages as subscopes
        (resolve-type/scope path name (last scopes))])]

    [else
     (match (string-split type-name ".")
       [(list path ... name)
        (for/or ([scope (in-list scopes)])
          (resolve-type/scope path name scope))])]))


;; resolve a type in a single scope. 'path' is the list of namespace
;; prefixes, and 'name' is the final element.
;; e.g. for "google.protobuf.Any": path = '("google" "protobuf"), name = "Any"
;; returns #f if type not found
(define (resolve-type/scope path name scope)
  (cond
    [(null? path) (type-in-scope scope name)]
    [(subscope scope (car path))
     => (λ (ss)
          (resolve-type/scope (cdr path) name ss))]
    [else #f]))
