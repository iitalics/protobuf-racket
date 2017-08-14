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

;; append a name onto another, e.g.
;;   (name-append "a.b" "c") = "a.b.c"
;;   (name-append "" "d.e") = "d.e"
(define (name-append pre post)
  (if (equal? pre "")
      post
      (string-append pre "." post)))

;; break scope into subscopes, e.g.
;;   (subscopes "a.b.c") = '("a.b.c" "a.b" "a")
(define (subscopes scope)
  (define-values (_ scopes)
    (for/fold ([fullname ""] [scopes '()])
              ([part (in-list (string-split scope "."))])
      (let ([new-fullname (name-append fullname part)])
        (values new-fullname
                (cons new-fullname scopes)))))
  scopes)


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



;; check that the descriptor (any kind) is not already used.
;; if it's used, raises a compile error
;; if it isn't, then adds it to all-descriptors
;; add-descriptor : object% string? srcloc? -> void
(define (add-descriptor des full-name loc)
  (cond
    [(hash-ref (all-descriptors) full-name #f)
     =>
     (λ (used-des)
       (let ([fd (if (is-a? used-des file-descriptor%)
                     used-des
                     (send used-des get-file-descriptor))])
         (raise-compile-error loc
                              "name ~v already bound in file ~v"
                              full-name
                              (path->string
                               (find-relative-path (current-directory)
                                                   (send fd get-file-path))))))]

    [else
     (hash-set! (all-descriptors) full-name des)]))



;; convert any arbitrary ast into a descriptor
;; this is recursive, checks for name clashes,
;; populates all-descriptors and compiles options.
;;
;; if given an ast:oneof, it will return a LIST of
;; descriptors instead, because oneofs contain nested fields.
;; this is the only exception, otherwise just returns a single descriptor.
;;
;; but it does NOT resolve type names! that must be
;; done in a second pass.
;;
;; ast->descriptor : ast? -> object%
;; ast->descriptor : ast:oneof? -> (listof object%)
(define (ast->descriptor ast)
  (match ast
    [(struct ast:root (loc pkg imports messages enums opts))
     (let ([fd (new file-descriptor%
                    [file-path (srcloc-source loc)]
                    [package pkg])])

       (hash-set! (file-descriptor-pool)
                  (srcloc-source loc)
                  fd)

       (for ([ss (in-list (subscopes pkg))])
         ;; hash-set! will overwrite entries; hash-ref! won't.
         ;; not sure which behavior we want here, not sure if it matters either way
         (hash-ref! (all-descriptors) ss fd))

       (parameterize ([current-file-descriptor fd])

         (send fd set-message-types (map ast->descriptor messages))
         (send fd set-enum-types (map ast->descriptor enums))

         ;; TODO: dependencies??
         ;; TODO: compile file options

         fd))]


    [(struct ast:message (loc name fields oneofs maps
                          messages enums reserved opts))

     (let* ([full-name (name-append (current-scope) name)]
            [des (new descriptor%
                      [name name]
                      [full-name full-name])])

       (add-descriptor des full-name loc)

       (parameterize ([current-scope full-name])
         (send des set-fields
               ;; NOTE: append*  (not append)
               (append* (map ast->descriptor fields)
                        (map ast->descriptor maps)
                        (map ast->descriptor oneofs)))

         (send des set-nested-types (map ast->descriptor messages))
         (send des set-nested-enums (map ast->descriptor enums))

         ;; TODO: compile message options

         des))]


    [(struct ast:field (loc name number label type opts))

     (let ([field-des (new field-descriptor%
                          [name name]
                          [number number]
                          [label label])])

       (add-descriptor field-des
                       (name-append (current-scope) name)
                       loc)

       ;; TODO: compile field options

       field-des)]


    [(struct ast:oneof (loc name sub-fields)) '()]

    [_ (format "unimplemented AST ~a" ast)]))
