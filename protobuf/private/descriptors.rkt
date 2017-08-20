#lang racket/base
(require racket/class
         racket/set
         racket/string
         "oop-utils.rkt")

(provide file-descriptor%
         descriptor%
         field-descriptor%
         oneof-descriptor%
         enum-descriptor%
         enum-value%

         options%
         file-options%
         message-options%
         field-options%
         oneof-options%
         enum-options%
         enum-value-options%

         current-file-descriptor
         file-descriptor-pool)

(define-syntax define-adder
  (syntax-rules (=>)
    [(_ (method arg ...) => variable (class%))
     (define/public (method arg ...)
       (let ([obj (new class% [arg arg] ...)])
         (set! variable (append variable (list obj)))
         obj))]))

;; maps resolved paths to generated file descriptors
;; (hash complete-path? => (is-a?/c file-descriptor%))
(define file-descriptor-pool (make-parameter (make-hash)))

;; this parameter determines the automatic 'file-descriptor'
;; field in most of the following objects
(define current-file-descriptor (make-parameter #f))


;; these classes are derived almost exactly from google/protobuf/descriptor.proto
;;   that .proto file is licensed under Copyright 2008 Google Inc.  All rights reserved.
;;   and written by Kenton Varda

(define-simple-class file-descriptor% object%
  ([file-path (error "file path must be set")]
   [package ""]
   [dependencies (make-hash)] ; hash complete-path? => bool? (is public?)
   [message-types '() #:list]
   [enum-types '() #:list]
   [options (new file-options%)])

  (define/public (has-dependency? complete-path)
    (hash-has-key? dependencies complete-path))
  (define/public (has-public-dependency? complete-path)
    (hash-ref dependencies complete-path #f))
  (define/public (get-public-dependencies)
    (for/list ([(path public?) (in-hash dependencies)]
               #:when public?)
      path))

  (define-adder (add-message) => message-types (descriptor%))
  (define-adder (add-enum) => enum-types (enum-descriptor%)))


(define-simple-class descriptor% object%
  ([name ""]
   [full-name #f]
   [file-descriptor (current-file-descriptor)]
   [fields '() #:list]
   [oneofs '() #:list]
   [nested-types '() #:list]
   [nested-enums '() #:list]
   [reserved-names (mutable-set)]
   [options (new message-options%)])

  (define-adder (add-field) => fields (field-descriptor%))
  (define-adder (add-oneof) => oneofs (oneof-descriptor%))
  (define-adder (add-nested-type) => nested-types (descriptor%))
  (define-adder (add-nested-enum) => nested-enums (enum-descriptor%))

   ;; list of predicates (integer? -> boolean?)
   ;; that return #t if the index is reserved
  (init-field [reserved-preds '()])
  (define/public (add-reserved-index-predicate p)
    (set! reserved-preds (cons p reserved-preds)))
  (define/public (index-reserved? i)
    (ormap (λ (f) (f i)) reserved-preds))

  ;; reserved-names is just a mutable set of strings (name of reserved field)
  (define/public (add-reserved-name x)
    (set-add! reserved-names x))
  (define/public (name-reserved? x)
    (set-member? reserved-names x)))


(define-simple-class field-descriptor% object%
  ([name ""]
   [file-descriptor (current-file-descriptor)]
   [number 0]
   [label 'optional]
   [type #f]
   [parent-oneof #f]
   [options (new field-options%)]))


(define-simple-class oneof-descriptor% object%
  ([name ""]
   [file-descriptor (current-file-descriptor)]
   [options (new oneof-options%)]))


(define-simple-class enum-descriptor% object%
  ([name ""]
   [full-name ""]
   [file-descriptor (current-file-descriptor)]
   [values '() #:list]
   [options (new enum-options%)])

  (define-adder (add-value) => values (enum-value%)))


(define-simple-class enum-value% object%
  ([name ""]
   [file-descriptor (current-file-descriptor)]
   [number 0]
   [options (new enum-value-options%)]))




(define-simple-class options% object%
  ([deprecated? #f]
   [uninterpreted '() #:list]))

(define-simple-class file-options% options%
  ([java-package #f]
   [java-outer-classname #f]
   [java-generate-equals-and-hash #f] ; does nothing lol?
   [java-string-utf8-checked? #f]
   [optimize-for 'speed]
   [go-package #f]
   [objc-class-prefix #f]
   [c#-namespace #f]
   [swift-prefix #f]
   [php-class-prefix #f]))

(define-simple-class message-options% options%
  ([message-set-wire-format? #f]
   [no-standard-accessor? #f]
   [map-entry? #f]))

(define-simple-class field-options% options%
  ([c-type 'string]
   [js-type 'normal]
   [packed? #f]
   [lazy? #f]
   [lispy #f]))

(define-simple-class enum-options% options%
  ([alias-allowed? #f]))

; these classes don't provide any additional options, so just use options%
(define oneof-options% options%)
(define enum-value-options% options%)
