#lang racket
(require "ast.rkt"
         "dependencies.rkt"
         "descriptors.rkt"
         racket/stxparam
         (for-syntax syntax/transformer))

(provide all-descriptors
         current-scope
         (struct-out exn:fail:compile)
         compile-ast
         compile-root
         with-clean-compile)


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
;;   google.protobuf.Field.type    =>  <field-descriptor% 'type'>
(define all-descriptors (make-parameter (make-hash)))

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




;; below is a macro for generating compile-ast function, so that it is
;; less tedious
;; for instance (%-sub-asts a => m)
;; automates a loop that iterates over 'a' and uses the method 'm' on
;; the current descriptor, to get a target for the ast
;; erm, as in:
;;   (%-sub-asts fields => add-field)
;; becomes
;;   (for ([field-ast (in-list fields)])
;;     (compile-ast field-ast (send this-descriptor add-field)))

;; the current ast node being parsed
(define-syntax-parameter this-ast
  (λ (stx) (error "use of this-ast outside of ast compiler")))
;; the loc of that node
(define-syntax this-loc
  (make-variable-like-transformer #'(ast-loc this-ast)))
;; the *-descriptor% being parsed into
(define-syntax-parameter this-desc
  (λ (stx) (error "use of this-desc outside of ast compiler")))


(define-syntax %-ast-compiler
  (syntax-rules ()
    [(_ [(struct-id (field-pat ...))
         expr ...]
        ...)
     (λ (the-ast the-desc)
       (syntax-parameterize ([this-ast (make-rename-transformer #'the-ast)]
                             [this-desc (make-rename-transformer #'the-desc)])
         (match the-ast
           [(struct struct-id (_ field-pat ...))
            expr ...]
           ...

           [_
            (displayln this-ast) (error "unimplement AST")])))]))

(define-syntax %-scoped-name
  (syntax-rules ()
    [(_ e)
     (let* ([name e]
            [full-name (name-append (current-scope) name)])
       (send this-desc set-name name)
       (add-descriptor this-desc
                       full-name
                       this-loc)
       full-name)]))

(define-syntax %-sub-asts
  (syntax-rules (=>)
    [(_ e => method)
     (for ([the-sub-ast (in-list e)])
       (compile-ast the-sub-ast (send this-desc method)))]))

(define-syntax %-options
  (syntax-rules (<=)
    [(_ kind e <= method)
     (let ([opts (send this-desc method)])
       (for ([the-sub-ast (in-list e)])
         (compile-option 'kind the-sub-ast opts)))]))


;; the current scope string (e.g. "google.protobuf" or "")
(define current-scope (make-parameter ""))

;; the current outer message, for when compiling a field or sub-message
;; of a message
(define current-message (make-parameter #f))

;; info about types that need to be resolved
(define current-unresolved
  (make-parameter '()))

(struct unresolved-field-type (loc desc scope))
(define (add-unresolved-field loc desc)
  (let ([ur (unresolved-field-type loc
                                   desc
                                   (current-scope))])
    (current-unresolved
     (cons ur (current-unresolved)))))



;; compile an ast into the given descriptor.
;; this adds the necessary properties to the
;; *-descriptor% object.
;;
;; compile-ast : ast? object% -> void
(define compile-ast
  (%-ast-compiler

   [(ast:message (name fields oneofs map-fields messages enums rsvs opts))
    (send this-desc set-full-name (%-scoped-name name))
    (%-options <message> opts <= get-options)
    (parameterize ([current-message this-desc]
                   [current-scope (name-append (current-scope) name)])
      (%-sub-asts fields     => add-field)
      (%-sub-asts oneofs     => add-oneof)
      (%-sub-asts map-fields => add-field)
      (%-sub-asts messages   => add-nested-type)
      (%-sub-asts enums      => add-nested-enum)
      (compile-reserved rsvs this-desc))]


   [(ast:field (name number label type opts))
    (%-scoped-name name)
    (%-options <field> opts <= get-options)
    (send this-desc set-number number)
    (send this-desc set-label label)
    (send this-desc set-type type)
    (unless (symbol? type)
      (add-unresolved-field this-loc this-desc))]


   [(ast:oneof (name fields))
    (%-scoped-name name)
    (for ([sub-field (in-list fields)])
      (let ([sub-field-desc (send (current-message) add-field)])
        (compile-ast sub-field sub-field-desc)
        (send sub-field-desc set-parent-oneof this-desc)))]


   [(ast:map-field (name number key-type val-type opts))
    (%-scoped-name name)
    (%-options <map-field> opts <= get-options)
    (send this-desc set-number number)
    (send this-desc set-label 'repeated)
    (send this-desc set-type (make-map-entry-type this-loc key-type val-type))]


   [(ast:enum (name vals opts))
    (%-scoped-name name)
    (%-options <enum> opts <= get-options)
    (%-sub-asts vals => add-value)
    (match vals
      [(cons (ast:enum-val _ _ 0 _) _) 'ok]
      ['() (raise-compile-error this-loc "enum must contain at least one field")]
      [_   (raise-compile-error this-loc "first enum field must be number 0")])
    ]

   [(ast:enum-val (name number opts))
    (%-scoped-name name)
    (%-options <enum-val> opts <= get-options)
    (send this-desc set-number number)
    ]))


;; compile the given list of reserved THINGS (indices, ranges, field names)
;; into the given descriptor%.
(define (compile-reserved reserved-things msg-desc)
  ;; reserved ranges, names
  (for ([rsv (in-list reserved-things)])
    (cond
      [(string? rsv) (send msg-desc add-reserved-name rsv)]
      [(ast:range? rsv)
       (send msg-desc add-reserved-index-predicate
             (if (eq? (ast:range-max rsv) 'max)
                 (>=/c (ast:range-min rsv))
                 (between/c (ast:range-min rsv)
                            (ast:range-max rsv))))]))

  ;; reserved indices as a bit set
  (define (idx->bit i)
    (arithmetic-shift 1 i))

  (define bitset
    (for/sum ([i (in-list reserved-things)]
              #:when (exact-integer? i))
      (idx->bit i)))

  (unless (zero? bitset)
    (send msg-desc add-reserved-index-predicate
          (λ (j)
            (positive? (bitwise-and bitset (idx->bit j)))))))


;; create a descriptor% generated by a map entry, which holds
;; two fields, one for the key type and one for the value type.
;; e.g.
;;   map<uint32, string> m = 4;
;; =>
;;   message (hidden).MapEntryType {
;;     uint32 key = 1;
;;     string value = 2;
;;   }
;;   repeated (hidden).MapEntryType m = 4;
(define (make-map-entry-type loc key-type value-type)

  (define entry-key-field
    (new field-descriptor%
         [name "key"]
         [number 1]
         [type key-type]))

  (define entry-value-field
    (new field-descriptor%
         [name "value"]
         [number 2]
         [type value-type]))

  (define entry-type
    (new descriptor%
         [name "MapFieldEntry"]
         [full-name (name-append (current-scope) "(hidden).MapFieldEntry")]
         [fields (list entry-key-field
                       entry-value-field)]))
  (send (send entry-type get-options) set-map-entry #t)

  ;; key is always a known type, but value could be unresolved!
  (unless (symbol? value-type)
    (add-unresolved-field loc entry-value-field))

  entry-type)


;; find any descriptor from the given scope using the given short name
;; may return anything with a full name, including fields and other non-types!
;; e.g.
;;   (resolve-something "google.protobuf" "protobuf.Any") => {google.protobuf.Any}
(define (resolve-something scope short-name)
  (or (for/or ([ss (in-list (subscopes scope))])
        (hash-ref (all-descriptors)
                  (name-append ss short-name)
                  #f))
      (hash-ref (all-descriptors)
                short-name
                #f)))


;; resolve the unresolved field described by the given unresolved-field-type
;; instance
(define (resolve ur)
  (match ur
    [(struct unresolved-field-type (loc desc scope))
     (let* ([short-name (send desc get-type)]
            [type (resolve-something scope short-name)])
       (cond
         [(or (is-a? type descriptor%)
              (is-a? type enum-descriptor%))
          (send desc set-type type)]

         [type ; (is not #f)
          (raise-compile-error (ast-loc ast)
                               "~v is not a type"
                               short-name)]

         [else
          (raise-compile-error loc
                               "cannot resolve type ~a in scope ~a"
                               short-name
                               scope)]))]))


;; compile the root of an ast tree into a file-descriptor%
;; this is recursive, checks for name clashes,
;; populates all-descriptors and compiles options (TODO).
;;
;; this also resolves all types, so it assumes that all
;; required types will be introduced after a first pass
;; through the tree. the best way to assure this is to
;; use the ordering produced by dependencies.rkt
;;
;; compile-root : ast:root? -> file-descriptor%
(define (compile-root ast)
  (match ast
    [(struct ast:root (loc pkg imports messages enums opts))

     ;; TODO: dependencies set

     (define file-desc (new file-descriptor%
                            [file-path (ast-source-file-path ast)]
                            [package pkg]))

     (hash-set! (file-descriptor-pool)
                (ast-source-file-path ast)
                file-desc)

     ;; register subscope variants of the package name
     ;; e.g. "foo.bar.baz" => "foo.bar.baz", "foo.bar", "foo"
     (for ([ss (in-list (subscopes pkg))])
       (hash-ref! (all-descriptors) ss file-desc))

     (define unresolved
       (parameterize ([current-file-descriptor file-desc]
                      [current-scope pkg]
                      [current-message #f]
                      [current-unresolved '()])

         (for ([opt (in-list opts)])
           (compile-option '<file> opt (send file-desc get-options)))
         (for ([msg-ast (in-list messages)])
           (compile-ast msg-ast (send file-desc add-message)))
         (for ([enum-ast (in-list enums)])
           (compile-ast enum-ast (send file-desc add-enum)))

         (current-unresolved)))

     (for-each resolve unresolved)
     file-desc]))



(define-syntax %-option-compiler
  (syntax-rules (=>)
    [(_ [kind (name => type method) ...] ...)
     (λ (the-kind the-ast the-opt)

       (when (or (ast:option-extension the-ast)
                 (not (= 1 (length (ast:option-names the-ast)))))
         (invalid-option the-ast))

       (case the-kind
         [(kind)
          (case (first (ast:option-names the-ast))
            [(name)
             (send the-opt method
                   (type (ast-loc the-ast)
                         (ast:option-value the-ast)))] ...
            [("deprecated")
             (send the-opt set-deprecated
                   (*bool* (ast-loc the-ast)
                           (ast:option-value the-ast)))]
            [else (invalid-option the-ast)])] ...
         [else (invalid-option the-ast)]))]))

(define (invalid-option ast)
  (raise-compile-error (ast-loc ast)
                       "invalid option \"~a~a\""
                       (if (ast:option-extension ast)
                           (format "(~a)." (ast:option-extension ast))
                           "")
                       (string-join (ast:option-names ast) ".")))

(define-syntax %-option-enum-type
  (syntax-rules (=>)
    [(_ errmsg-name [before => after-expr] ...)
     (λ (loc x)
       (case x
         [(before) after-expr] ...
         [else (raise-compile-error loc
                                    "expected ~a enum value"
                                    'errmsg-name)]))]))


(define (*bool* loc x)
  (if (boolean? x) x
      (raise-compile-error loc "expected boolean value")))

(define (*string* loc x)
  (if (string? x) x
      (raise-compile-error loc "expected string value")))

(define *optimize-mode*
  (%-option-enum-type OptimizeMode
    [SPEED => 'speed]
    [CODE_SIZE => 'code-size]
    [LITE_RUNTIME => 'lite-runtime]))

(define *c-type*
  (%-option-enum-type CType
    [STRING => 'string]
    [CORD => 'cord]
    [STRING_PIECE => 'string-piece]))

(define *js-type*
  (%-option-enum-type JSType
    [JS_NORMAL => 'normal]
    [JS_STRING => 'string]
    [JS_NUMBER => 'number]))


(define compile-option
  (%-option-compiler
   [<file> ("optimize_for"           => *optimize-mode* set-java-optimize-for)
           ("java_package"           => *string* set-java-package)
           ("java_outer_classname"   => *string* set-java-outer-classname)
           ("java_generate_equals_and_hash" => *bool* set-java-generate-equals-and-hash)
           ("java_string_check_utf8" => *bool*   set-java-string-utf8-checked)
           ("go_package"             => *string* set-go-package)
           ("objc_class_prefix"      => *string* set-objc-class-prefix)
           ("csharp_namespace"       => *string* set-c#-namespace)
           ("swift_prefix"           => *string* set-swift-prefix)
           ("php_class_prefix"       => *string* set-php-class-prefix)]

   [<message> ("message_set_wire_format"         => *bool* set-message-set-wire-format)
              ("no_standard_descriptor_accessor" => *bool* set-no-standard-accessor)]

   [<field> ("packed" => *bool* set-packed)
            ("lazy"   => *bool* set-lazy)
            ("ctype"  => *c-type* set-c-type)
            ("jstype" => *js-type* set-js-type)]

   [<enum> ("allow_alias" => *bool* set-alias-allowed)]

   [<map-field>]
   [<enum-val>]
   ))



;; reset all parameters to ensure fresh compilation
(define-syntax-rule (with-clean-compile body ...)
  (parameterize ([file-descriptor-pool (make-hash)]
                 [all-descriptors (make-hash)]
                 [current-scope ""]
                 [current-message #f]
                 [current-unresolved '()]
                 [current-file-descriptor #f])
    body ...))
