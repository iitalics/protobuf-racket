#lang racket/base
(require "../encdec/wire.rkt"
         "../encdec/varint.rkt"
         (for-syntax racket/base
                     racket/list
                     syntax/parse
                     syntax/stx))

(provide check-encoding
         message-decoder)

(define (check-encoding actual expected fld-name)
  (unless (eq? actual expected)
    (printf "actual: ~a, expected: ~a\n" actual expected)
    (raise (exn:fail:read (format "field ~v encoded incorrectly"
                                  fld-name)
                          (current-continuation-marks)
                          '()))))


(define-syntax message-decoder
  (λ (stx)

    ;;;; functions to use for the .replace attribute:
    ;; don't change decoded input at all (e.g. int32, bytes)
    (define (noop decoded previous) decoded)

    ;; transform decoded input by calling a function on it
    (define (transf fn-id . arg-exprs)
      (with-syntax ([fn-id fn-id]
                    [(arg ...) arg-exprs])
        (λ (decoded previous)
          #`(fn-id #,decoded arg ...))))


    (define-syntax-class type-encoding
      #:literals (quote)
      #:datum-literals (bytes string
                              uint32 int32 sint32
                              uint64 int64 sint64)
      #:attributes (wire-type decoder replace)
      (pattern '(~or uint32 uint64)
               #:with wire-type #'varint #:with decoder #'decode-varint
               #:attr replace noop)
      (pattern '(~or sint32 sint64)
               #:with wire-type #'varint #:with decoder #'decode-varint
               #:attr replace (transf #'uint->sint/zz))

      (pattern 'int32
               #:with wire-type #'varint #:with decoder #'decode-varint
               #:attr replace (transf #'uint->sint32/2c))
      (pattern 'int64
               #:with wire-type #'varint #:with decoder #'decode-varint
               #:attr replace (transf #'uint->sint64/2c))

      (pattern 'bytes
               #:with wire-type #'length-delim #:with decoder #'decode-length-delim
               #:attr replace noop)
      (pattern 'string
               #:with wire-type #'length-delim #:with decoder #'decode-length-delim
               #:attr replace (transf #'bytes->string/utf-8)))


    (syntax-parse stx
      [(_ join-fn
          [oneof ...]
          (arg-num arg-name:str arg-type:type-encoding arg-init-expr) ...)

       #:with (arg-id ...) (generate-temporaries #'[arg-name ...])
       #:with ((new-arg-list ...) ...)
       (let ([arg-ids (stx->list #'[arg-id ...])])
         (for/list ([i (in-naturals)]
                    [arg-id (in-list arg-ids)]
                    [replace (in-list (attribute arg-type.replace))])
           (with-syntax ([(pre-arg ...) (take arg-ids i)]
                         [(post-arg ...) (drop arg-ids (add1 i))]
                         [replacement (replace #'x- arg-id)])
             #'(pre-arg ... replacement post-arg ...))))

       #'(λ (bs i0)
           (let loop ([i i0] [arg-id arg-init-expr] ...)
             (cond
               [(>= i (bytes-length bs))
                (join-fn arg-id ...)]

               [else
                (define-values (i1 num+wire-type) (decode-field-number+type bs i))
                (case (car num+wire-type)
                  [(arg-num)
                   ; check wire type encoding
                   (check-encoding (cdr num+wire-type) 'arg-type.wire-type arg-name)
                   ; apply the first decoder
                   (define-values (i2 x-) (arg-type.decoder bs i1))
                   ; new-arg-list applies replacement and only fills in the
                   ; affected arg id
                   (loop i2 new-arg-list ...)]
                  ...

                  [else
                   (loop i1 arg-id ...)])])))])))
