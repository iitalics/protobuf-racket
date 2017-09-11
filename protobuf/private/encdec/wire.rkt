#lang racket/base
(require "util.rkt" "varint.rkt"
         racket/contract/base
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(provide uint->sint/2c
         uint->sint/zz
         decode-fixed64 decode-sfixed64 decode-double
         decode-fixed32 decode-sfixed32 decode-float
         decode-length-delim
         decode-field-number+type)

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


;; (define-fixed-bytes T bits (func args ...))
;; defines function 'decode-T' which reads a fixed number
;; of bits, and passes them to the given function.
;; the first _ in the args list is replaced by the read
;; bytes, and the second _ in the args list is replaced
;; by the start index and length
;;
;; e.g.
;;    (define-fixed-bytes fixed64 64 (integer-bytes->integer _ #f #f _))
;;  will call
;;    (integer-bytes->integer bs #f #f start-idx 8)
(define-syntax define-fixed-bytes
  (syntax-parser
    [(_ T bits:nat (func pre-arg ... (~datum _) mid-arg ... (~datum _) post-arg ...))
     #:with byts (datum->syntax this-syntax (/ (syntax-e #'bits) 8))
     #:with decode-T (format-id #'T "decode-~a" #'T)
     ;#:with read-T (format-id #'T "read-~a" #'T)
     #'(define (decode-T bs i)
         (values (check-len bs i byts)
                 (func pre-arg ... bs
                       mid-arg ... 0 byts
                       post-arg ...)))]))


;; decode a number that has a fixed number
;; of bytes representation. all are encoded in
;; little endian, and vary in signedness / floating point.
;;
;; decode-T : bytes? pos? -> pos? X
(define-fixed-bytes fixed64 64
  (integer-bytes->integer _ #f #f _))
(define-fixed-bytes sfixed64 64
  (integer-bytes->integer _ #t #f _))
(define-fixed-bytes double 64
  (floating-point-bytes->real _ #f _))
(define-fixed-bytes fixed32 32
  (integer-bytes->integer _ #f #f _))
(define-fixed-bytes sfixed32 32
  (integer-bytes->integer _ #t #f _))
(define-fixed-bytes float 32
  (floating-point-bytes->real _ #f _))

;; decode a length delimeted sequence of bytes.
;; decode-length-delim : bytes? pos? -> pos? bytes?
(define (decode-length-delim bs i)
  (define-values (j len) (decode-varint bs i))
  (values (check-len bs j len)
          (subbytes bs j (+ j len))))


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
