#lang racket/base



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
