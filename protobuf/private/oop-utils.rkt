#lang racket/base
(require racket/class (for-syntax racket/base syntax/parse racket/syntax))
(provide gen-pub+get/set
         define-simple-class)

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
