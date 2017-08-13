#lang racket
(require "descriptors.rkt"
         racket/generic)

(provide gen:scope
         scope?
         scope-leaf
         subscope
         subscope*
         resolve-type)

(define-generics scope
  ;; return a subscope of this scope by the given name, or #f if doesn't exist.
  ;; for instance if we have packages
  ;;   com.google
  ;;   com.oracle
  ;; then (subscope <package com> "google") = <package com.google>
  ;; then (subscope <package com> "oracle") = <package com.oracle>
  ;; then (subscope <package com> "mozilla") = #f
  [subscope scope name]

  ;; return something directly in this scope that isn't just a subscope
  ;; in particular, this is usually a type.
  ;; (scope-leaf <package google> "protobuf") = #f
  ;; (scope-leaf <package google.protobuf> "Any") = <descriptor%>
  ;; (scope-leaf <message google.protobuf.FieldDescriptor> "Type") = <enum-descriptor%>
  [scope-leaf scope name])


;; repeated application of subscope
;; e.g. (subscope* <package a> '("b" "c")) = <package a.b.c>
;; (if that package exists)
;; returns #f it doesn't exist.
(define (subscope* scope path)
  (cond
    [(null? path) scope]
    [(subscope scope (car path)) => (λ (ss) (subscope* ss (cdr path)))]
    [else #f]))


;; resolve a type by the given name by searching the list of scopes.
;; the name is the entire string e.g. "Any" or "protobuf.Any"
;; or ".google.protobuf.Any"
;; returns #f if type not found
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
;; returns #f if type can't be found
(define (resolve-type/scope path name scope)
  (let ([ss (subscope* scope path)])
    (and ss (scope-leaf ss name))))
