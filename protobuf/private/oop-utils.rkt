#lang racket/base
(require racket/class (for-syntax racket/base syntax/parse racket/syntax))
(provide gen-pub+get/set
         define-simple-class)

;; remove last k (default: 1) characters from string/symbol
(define-for-syntax (remove-last sym [k 1])
  (let ([s (if (string? sym) sym (symbol->string sym))])
    (string->symbol (substring s 0 (- (string-length s) k)))))

; foo? => is-foo?
; bar  => get-bar
(define-for-syntax (mk-getter x)
  (define s (symbol->string (syntax->datum x)))
  (if (regexp-match? #px".+\\?" s)
      (format-id x "is-~a" s)
      (format-id x "get-~a" s)))

; foo? => set-foo
; bar  => set-bar
(define-for-syntax (mk-setter x)
  (define s (symbol->string (syntax->datum x)))
  (if (regexp-match? #px".+\\?" s)
      (format-id x "set-~a" (remove-last s))
      (format-id x "set-~a" s)))

; babies => add-baby
; adults => add-adult
(define-for-syntax (mk-adder x)
  (define s (symbol->string (syntax->datum x)))
  (if (regexp-match? #rx"ies$" s)
      (format-id x "add-~ay" (remove-last s 3))
      (format-id x "add-~a" (remove-last s 1))))



; generate init-field, get-X, set-Y, and also add-X if #:list is supplied
(define-syntax gen-pub+get/set
  (syntax-parser
    [(_ field:id init-val (~optional (~and #:list (~bind [is-list? #t]))))
     #:with init-field (datum->syntax this-syntax 'init-field)
     #:with getter (mk-getter #'field)
     #:with setter (mk-setter #'field)
     #:with adder (mk-adder #'field)
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
