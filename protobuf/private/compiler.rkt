#lang racket
(require "ast.rkt"
         "dependencies.rkt"
         "descriptors.rkt")

(provide with-clean-compile
         all-descriptors
         current-scope
         (struct-out exn:fail:compile)
         ast->descriptor)


(define-struct (exn:fail:compile exn:fail:read) ())

(define (raise-compile-error loc msg . fmts)
  (raise (make-exn:fail:compile
          (apply format (cons msg fmts))
          (current-continuation-marks)
          (list loc loc))))


(define (name-append prefix post)
  (if (equal? prefix "")
      post
      (string-append prefix "." post)))



;; maps resolved paths to generated file descriptors
;; (hash complete-path? => (is-a?/c file-descriptor%))
(define file-descriptor-pool (make-parameter (make-hash)))

;; maps qualified names to descriptors
;; e.g. [file 'descriptor.proto']
;;     syntax = 'proto3';
;;     package google.protobuf;
;;     message Field {
;;       enum Type { UINT32 = 0; ... }
;;       Type type = 1;
;;     }
;; would be generated into
;;   google                        =>  <file-descriptor% 'descriptor.proto'>
;;   google.protobuf               =>  <file-descriptor% 'descriptor.proto'>
;;   google.protobuf.Field         =>  <descriptor% 'Field'>
;;   google.protobuf.Field.Type    =>  <enum-descriptor% 'Type'>
;;   google.protobuf.Field.UINT32  =>  <enum-value% 'UINT32'>
;;   google.protobuf.Field.type    =>  <field-descriptor% 'Type'>
(define all-descriptors (make-parameter (make-hash)))

;; the current scope string (e.g. "google.protobuf" or "")
(define current-scope (make-parameter ""))


;; reset all of the above parameters
(define-syntax-rule (with-clean-compile body ...)
  (parameterize ([file-descriptor-pool (make-hash)]
                 [all-descriptors (make-hash)]
                 [current-scope ""]
                 [current-file-descriptor #f])
    body ...))



;; convert any arbitrary ast into a descriptor
;; this is recursive, checks for name clashes,
;; populates all-descriptors and compiles options.
;;
;; but it does NOT resolve type names! that must be
;; done in a second pass.
;;
;; ast->descriptor : ast? -> object%
(define (ast->descriptor ast)
  (match ast
    [(struct ast:root (loc pkg imports messages enums opts)) (void)])

  (error "unimplemented"))
