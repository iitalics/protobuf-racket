#lang racket/base
(require "util.rkt" "varint.rkt"
         racket/contract/base
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(provide uint->sint/2c
         uint->sint/zz
         read-fixed64 read-sfixed64 read-double
         read-fixed32 read-sfixed32 read-float
         read-field-number+type)

;; convert unsigned integer to signed integer
;; using two's complement, e.g.
;;   (uint->sint/2c #x0005 16) = 5
;;   (uint->sint/2c #xffff 16) = -1
;;   (uint->sint/2c #xff02 16) = -254
;;
;; uint-sint/2c : nat? nat? -> exact-integer?
(define (uint->sint/2c x n-bits)
  (if (zero? (bitwise-and (arithmetic-shift 1 (sub1 n-bits)) x))
      x
      (- x (arithmetic-shift 1 n-bits))))

;; convert unsigned integer to signed integer
;; using "ZigZag", e.g.
;;   (uint->sint/zz 0) = 0
;;   (uint->sint/zz 1) = -1
;;   (uint->sint/zz 4294967294) = 2147483647
;;   (uint->sint/zz 4294967295) = -2147483648
(define (uint->sint/zz x)
  (if (even? x)
      (arithmetic-shift x -1)
      (- 0 (arithmetic-shift x -1) 1)))


;; (define-fixed-bytes-r/d T bits (func args ...))
;; defines functions 'read-T' and 'decode-T' which read
;; the given number of bits, and send them to the given
;; function.
;; the first _ in the args list is replaced by the input
;; bytes, and the second _ in the args list is replaced
;; by the start index and length
;;
;; e.g.
;;    (define-fixed-bytes-d/r fixed64 64 (integer-bytes->integer _ #f #f _))
;;  will call
;;    (integer-bytes->integer bs #f #f start-idx 8)
(define-syntax define-fixed-bytes-d/r
  (syntax-parser
    [(_ T bits:nat (func pre-arg ... (~datum _) mid-arg ... (~datum _) post-arg ...))
     #:with byts (datum->syntax this-syntax (/ (syntax-e #'bits) 8))
     #:with decode-T (format-id #'T "decode-~a" #'T)
     #:with read-T (format-id #'T "read-~a" #'T)
     #'(begin
         (define (decode-T bs i)
           (values (check-len bs i byts)
                   (func pre-arg ... bs
                         mid-arg ... 0 byts
                         post-arg ...)))
         (define-reader-from-decoder read-T decode-T))]))


;; decode-T : bytes? pos? -> pos? X
;; read-T : [input-port?] -> exact-integer?
(define-fixed-bytes-d/r fixed64 64
  (integer-bytes->integer _ #f #f _))
(define-fixed-bytes-d/r sfixed64 64
  (integer-bytes->integer _ #t #f _))
(define-fixed-bytes-d/r double 64
  (floating-point-bytes->real _ #f _))
(define-fixed-bytes-d/r fixed32 32
  (integer-bytes->integer _ #f #f _))
(define-fixed-bytes-d/r sfixed32 32
  (integer-bytes->integer _ #t #f _))
(define-fixed-bytes-d/r float 32
  (floating-point-bytes->real _ #f _))


;; wire types as described by the protocol buffers guide. the deprecated
;; wire types ("start group" and "end group") are not included
(define wire-type?
  (or/c 'varint '64-bit '32-bit 'length-delim))


;; reads and returns a cons of two values: the field number, and the wire
;; type of the field. encoded using just a varint and a bit-shift.
;;
;; decode-field-number+type : bytes? pos? -> pos? (cons/c exact-integer? wire-type?)
(define (decode-field-number+type bs i)
  (define-values (j type+num) (decode-varint bs i))
  (define wire-type (bitwise-and type+num 7))
  (define field-num (arithmetic-shift type+num -3))
  (case wire-type
    [(0) (values j (cons field-num 'varint))]
    [(1) (values j (cons field-num '64-bit))]
    [(2) (values j (cons field-num 'length-delim))]
    [(5) (values j (cons field-num '32-bit))]
    [else
     (raise (exn:fail:read "encountered invalid or deprecated wire type"
                           (current-continuation-marks)
                           '()))]))

;; read-field-number+type : [input-port?] -> (or/c eof-object? (cons/c exact-integer? wire-type?))
(define-reader-from-decoder read-field-number+type
  decode-field-number+type)
