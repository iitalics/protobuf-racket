#lang racket
(provide (all-defined-out))
(require "../private/compiler/compiler.rkt"
         "../private/compiler/dependencies.rkt"
         "../private/codegen/codegen.rkt")


;; parse the ast:root for a file with the given string lines as contents
(define (parse-root/tmp . strs)
  (let ([tmp-path (make-temporary-file "protobuftest~a.proto")])

    (with-output-to-file tmp-path
      #:exists 'replace
      (λ ()
        (for-each displayln strs)))

    ;; dynamic wind so that we always delete the temp file
    ;; if anything goes wrong
    (dynamic-wind
      void
      (λ ()
        (let-values ([(init-asts sorted-asts)
                      (parse+dependencies (list tmp-path))])
          (first init-asts)))
      (λ ()
        (delete-file tmp-path)))))

;; compile the dsctor:file for a file with the given string lines as contents
(define (compile-root/tmp . strs)
  (compile-root (apply parse-root/tmp strs)))


;; return codegen implementation objects, for the given fully-qualified types,
;; in the context of a file using the string lines as contents
(define (implement/tmp fqs-to-gen . strs)
  (let ([file-dsc (apply compile-root/tmp strs)])
    (parameterize ([current-impl-queue (make-hash)])
      (map get-or-queue-impl
           fqs-to-gen))))
