#lang racket
(provide (all-defined-out))
(require "../private/compiler.rkt"
         "../private/dependencies.rkt"
         "../private/codegen.rkt")


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

;; generate syntax object for a file with the given string lines as contents
(define (codegen-root/tmp #:export exports . strs)
  (codegen-file (apply compile-root/tmp strs)))
