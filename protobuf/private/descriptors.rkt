#lang racket/base
(require racket/class
         (for-syntax racket/base syntax/parse racket/syntax))


;; remove 's' from symbol name e.g. 'names => 'name
(define-for-syntax (remove-s sym)
  (let ([s (symbol->string sym)])
    (string->symbol (substring s 0 (sub1 (string-length s))))))

; generate init-field, get-X, set-Y, and also add-X if #:list is supplied
(define-syntax gen-pub+get/set
  (syntax-parser
    [(_ field:id init-val (~optional (~and #:list (~bind [is-list? #t]))))
     #:with init-field (datum->syntax this-syntax 'init-field)
     #:with getter (format-id this-syntax "get-~a" #'field)
     #:with setter (format-id this-syntax "set-~a" #'field)
     #:with adder (format-id this-syntax "add-~a" (remove-s (syntax-e #'field)))
     #:with (adder-impl ...)
     (if (attribute is-list?)
         #'[ (define/public (adder . vs) (set! field (append field vs))) ]
         #'[])
     #'(begin
         (init-field [field init-val])
         (define/public (getter) field)
         (define/public (setter v) (set! field v))
         adder-impl ...)]))

; generate a class with the above automatically
(define-syntax-rule (define-simple-class name% parent% ([field ...] ...) rest ...)
  (define name%
    (class* parent% ()
      (super-new)
      (gen-pub+get/set field ...) ...
      rest ...)))



(define-simple-class file-descriptor% object%
  ([path (error "file path must be set")]
   [package ""]
   [imports '() #:list]
   [public-imports '() #:list]
   [message-types '() #:list]
   [enum-types '() #:list]
   [file-options (new file-options%)]))

(define-simple-class descriptor% object%
  ([name (error "descriptor name must be set")]
   [fields '() #:list]
   [oneofs '() #:list]
   [nested-types '() #:list]
   [nested-enums '() #:list]
   [reserved-indices '()]
   [options (new message-options%)]))

(define-simple-class field-descriptor% object%
  ([name (error "field name must be set")]
   [number (error "field number must be set")]
   [label 'optional]
   [type (error "field type must be set")]
   [parent-oneof #f]
   [options (new field-options%)]))

(define-simple-class oneof-descriptor% object%
  ([name (error "oneof name must be set")]
   [options (new oneof-options%)]))

(define-simple-class enum-descriptor% object%
  ([name (error "enum name must be set")]
   [values '() #:list]
   [options (new enum-options%)]))

(define-simple-class enum-value% object%
  ([name (error "enum value name must be set")]
   [number 0]
   [options (new enum-value-options%)]))




(define-simple-class options% object%
  ([is-deprecated? #f]
   [uninterpreted '() #:list]))

(define-simple-class file-options% options%
  ([java-package #f]
   [java-outer-classname #f]
   [java-generate-equals-and-hash #f] ; does nothing lol?
   [java-string-check-utf8? #f]
   [optimize-for 'speed]
   [go-package #f]
   [objc-class-prefix #f]
   [c#-namespace #f]
   [switch-prefix #f]
   [php-class-prefix #f]))

(define-simple-class message-options% options%
  ([is-map-entry? #f]))

(define-simple-class field-options% options%
  ([c-type 'string]
   [js-type 'normal]
   [is-packed? #f]
   [is-lazy? #f]))

(define-simple-class enum-options% options%
  ([allow-alias? #f]))

; these classes don't provide any additional options, so just use options%
(define oneof-options% options%)
(define enum-value-options% options%)
