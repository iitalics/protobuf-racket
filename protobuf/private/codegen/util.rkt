#lang racket/base
(require "../descriptors.rkt"
         "../compiler/compiler.rkt"
         racket/port)
(provide (all-defined-out))


(define (map-field? dsc)
  (and (dsctor:field-repeated? dsc)
       (map-entry-type? (dsctor:field-type dsc))))

(define (map-entry-type? ty)
  (and (string? ty)
       (dsctor-option (hash-ref (all-descriptors) ty)
                      "map_entry"
                      #f)))


;; get the name of the given identifier and convert it to
;; a lispy format
;; dsctor-lisp-name : dsctor? -> string?
(define (dsctor-lisp-name dsc)
  ;; TODO: allow descriptor option to customize this
  (lispify (dsctor-name dsc)))

;; "lispify" a string, converting underscore and
;; camelcase into dashes.
(define (lispify s)
  (with-output-to-string
    (λ ()
      (for/fold ([prev-chr-locase? #f])
                ([c (in-string s)])

        (when (or (eqv? c #\_)
                  (and prev-chr-locase? (char-upper-case? c)))
          (write-char #\-))

        (when (not (eqv? c #\_))
          (write-char (char-downcase c)))

        (or (char-lower-case? c)
            (char-numeric? c))))))

(module+ test
  (require rackunit)
  (check-equal? (lispify "UpperCamelCase") "upper-camel-case")
  (check-equal? (lispify "lowerCamelCase") "lower-camel-case")
  (check-equal? (lispify "snake_case") "snake-case")
  (check-equal? (lispify "with_number123") "with-number123")
  (check-equal? (lispify "Act1Scene2") "act1-scene2")
  (check-equal? (lispify "Mixed_Ugly_Case") "mixed-ugly-case")
  (check-equal? (lispify "JUST_REMEMBER_ALL_CAPS") "just-remember-all-caps"))
