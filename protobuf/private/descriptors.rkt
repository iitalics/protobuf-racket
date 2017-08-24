#lang racket/base
(require (for-syntax racket/base
                     racket/syntax))

(provide (struct-out dsctor)
         dsctor-source-file-path
         dsctor-option)


(struct dsctor (loc name options) #:transparent)


(define-syntax define-descriptor-struct
  (syntax-rules ()
    [(_ (name) (fld ...))
     (begin
       (provide (struct-out name))
       (struct name dsctor
         (fld ...)
         #:transparent))]))


(define-descriptor-struct (dsctor:file)
  (package
   deps
   public-deps
   messages
   enums
   all-types))

(define-descriptor-struct (dsctor:message)
  (fields
   oneofs
   nested-types
   nested-enums
   reserved-names
   reserved-index?))

(define-descriptor-struct (dsctor:field)
  (type
   number
   repeated?
   oneof))

(define-descriptor-struct (dsctor:oneof)
  ())

(define-descriptor-struct (dsctor:enum)
  (values))

(define-descriptor-struct (dsctor:enum-value)
  (number))


(define (dsctor-source-file-path d)
  (srcloc-source (dsctor-loc d)))

(define (dsctor-option d key default)
  (if (hash? (dsctor-options d))
      (hash-ref (dsctor-options d) key (λ () default))
      default))

#|
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
   [lazy? #f]))

(define-simple-class enum-options% options%
  ([alias-allowed? #f]))

; these classes don't provide any additional options, so just use options%
(define oneof-options% options%)
(define enum-value-options% options%)
|#
