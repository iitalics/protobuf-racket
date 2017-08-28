#lang racket
(provide (all-defined-out))
(require "../private/compiler.rkt"
         "../private/dependencies.rkt")


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

(define (compile-root/tmp . strs)
  (compile-root (apply parse-root/tmp strs)))
