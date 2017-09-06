#lang racket
(require "ast.rkt"
         "dependencies.rkt"
         "../descriptors.rkt"
         racket/hash
         racket/generator
         racket/stxparam
         (for-syntax racket/base
                     (only-in racket/sequence in-syntax)
                     syntax/stx
                     syntax/parse))

(provide (struct-out exn:fail:compile)
         fq-name?
         ur-name?
         all-descriptors
         recursive-descent
         compile-root)



;; compiler-specific exception type
(define-struct (exn:fail:compile exn:fail:read) ())

;; raise compiler error with string formatting
(define (raise-compile-error loc msg . fmts)
  (raise (make-exn:fail:compile
          (apply format (cons msg fmts))
          (current-continuation-marks)
          (list loc loc))))

(define (shorten-file-path p)
  (let ([paths (list p (find-relative-path (current-directory) p))])
    (argmin string-length
            (map path->string paths))))



;; fq-name = fully qualified name, e.g. ".google.protobuf.Any"
(define (fq-name? s)
  (and (string? s)
       (string-prefix? s ".")))

;; ur-name = unresolved name, e.g. "protobuf.Any"
(define (ur-name? s)
  (string? s))


;; string? string? -> string?
(define (qualify-in-scope name [scope (current-scope)])
  (string-append scope "." name))

;; break scope into subscopes, e.g.
;;   (in-subscopes ".a.b.c") => "", ".a", ".a.b", ".a.b.c"
;;
;; in-subscopes : string? -> (sequenceof string?)
(define (in-subscopes scope)
  (in-generator (for ([c (in-string scope)]
                      [i (in-naturals)]
                      #:when (char=? #\. c))
                  (yield (substring scope 0 i)))
                (yield scope)))


;; descriptors that are finished being compiled
;;   (e.g. from a previous pass)
;;
;; fq-name? => dsctor?
(define all-descriptors
  (make-parameter (make-hash)))

(define (get-dsc fq)
  (hash-ref (all-descriptors) fq))

;; descriptors being compiled that need additional passes
;; (listof fq-name? dsctor?)
(define current-unresolved-descriptors
  (make-parameter '()))

;; current scope string, e.g. "" or ".google.protobuf"
;; (or/c "" fq-name?)
(define current-scope
  (make-parameter ""))

;; fully-qualified name of current oneof
;; for use in compiling fields nested in a oneof
;; (or/c #f fq-name?)
(define current-oneof-fq-name
  (make-parameter #f))



(define (add-unresolved! dsc
                         [fq-name (qualify-in-scope (dsctor-name dsc))]
                         [loc (dsctor-loc dsc)])
  (cond
    [(hash-ref (all-descriptors) fq-name #f)
     =>
     (λ (prev-dsc)
       (raise-compile-error
        loc
        "name ~a already used by file ~a"
        fq-name
        (shorten-file-path (dsctor-source-file-path prev-dsc))))]

    [else
     (hash-set! (all-descriptors) fq-name dsc)
     (current-unresolved-descriptors
      (cons fq-name (current-unresolved-descriptors)))
     fq-name]))


;; first pass: (recursive decent)
;;
;; instantiate skeleton descriptors (as described above)
;; check for name conflicts
;; insert (cons AST dsc) into current-unresolved-descriptors
;;
;;   dsctor-options = (listof <ast:option>)
;;   dsctor:message-fields/oneofs = (listof <fq-name>)
;;   dsctor:message-nested-* = (listof <fq-name>)
;;   dsctor:message-reserved-names = (listof <string>)
;;   dsctor:message-reserved-index? = (listof <int/range>)
;;   dsctor:field-type = (cons <uqname> <scope>)
;;
;; recursive-descent : ast? -> fq-name?
(define (recursive-descent ast)
  (match ast

    ;; -[ message ]-
    [(struct ast:message (loc name field-asts oneof-asts map-field-asts
                          nested-msg-asts nested-enum-asts reserved opts))

     (define fq-name (qualify-in-scope name))

     ;; parameterize current-scope, because fields etc. need to be in
     ;; a sub scope of the message
     ;;  e.g. message A { message B {}  uint32 x = 1; }
     ;;  descriptors:
     ;;     A, A.B, A.x
     (add-unresolved!
      (parameterize ([current-scope fq-name])
        (let* ([fields (map recursive-descent field-asts)]
               [map-fields (map recursive-descent map-field-asts)]
               [oneofs (map recursive-descent oneof-asts)]
               [oneof-fields
                ;; traverse fields nested in oneofs
                (for/list ([oo-fq (in-list oneofs)]
                           [oo-ast (in-list oneof-asts)])
                  (parameterize ([current-oneof-fq-name oo-fq])
                    (map recursive-descent
                         (ast:oneof-fields oo-ast))))]
               [all-fields (append* fields map-fields oneof-fields)]
               [nested-msgs (map recursive-descent nested-msg-asts)]
               [nested-enums (map recursive-descent nested-enum-asts)])

          (dsctor:message loc
                          name
                          opts
                          all-fields
                          oneofs
                          nested-msgs
                          nested-enums
                          (filter string? reserved)
                          (filter (negate string?) reserved)))))]

    ;; -[ field ]-
    [(struct ast:field (loc name number label type opts))
     (add-unresolved!
      (dsctor:field loc
                    name
                    opts
                    (cons type (current-scope))
                    number
                    (eq? label 'repeated)
                    (current-oneof-fq-name)))]


    ;; -[ oneof ]-
    [(struct ast:oneof (loc name field-asts))
     (add-unresolved!
      (dsctor:oneof loc name '()))]


    ;; -[ map-field ]-
    [(struct ast:map-field (loc name number key-type val-type opts))

     (define outer-scope (current-scope))
     (define hidden-scope (format "(~a)~a" (gensym 'hidden) (current-scope)))

     ;; create "key" and "value" fields.
     ;; note that they use outer-scope to resolve types, not the hidden
     ;; scope that we're putting the MapFieldEntry into
     (parameterize ([current-scope (qualify-in-scope "MapFieldEntry" hidden-scope)])
       (add-unresolved!
        (dsctor:field loc "key" '() (cons key-type outer-scope) 1 #f #f))
       (add-unresolved!
        (dsctor:field loc "value" '() (cons val-type outer-scope) 2 #f #f)))

     ;; create the entry type
     (define entry
       (parameterize ([current-scope hidden-scope])
         (add-unresolved!
          (dsctor:message loc
                          "MapFieldEntry"
                          (list (ast:option loc #f '("map_entry") #t))
                          (list (string-append hidden-scope ".MapFieldEntry.key")
                                (string-append hidden-scope ".MapFieldEntry.value"))
                          '() '() '() '() '()))))

     (add-unresolved!
      (dsctor:field loc name opts entry number #t #f))]


    ;; -[ enum ]-
    [(struct ast:enum (loc name val-asts opts))
     (let ([vals (map recursive-descent val-asts)])
       (add-unresolved!
        (dsctor:enum loc name opts vals)))]


    ;; -[ enum-val ]-
    [(struct ast:enum-val (loc name number opts))
     (add-unresolved!
      (dsctor:enum-value loc name opts number))]))


;; given the types for valid options, and a list of ast:options,
;; return an options hash, or error for any invalid options. invalid
;; options includes unknown option names, and known options with incorrect
;; values.
;;
;; extensions are currently unsupported.
;;
;; compile-options : (listof ast:option) options-spec? -> (hash string? => any)
;; options-spec? =
;;   (listof (list string? type-spec?))
;; type-spec? =
;;   (or/c 'bool 'string (listof symbol?))
(define (compile-options asts
                         options-spec
                         [default-options-spec
                           '(["deprecated" bool])])

  ;; check-type : string? any/c -> (or/c 'bad-option 'bad-value 'ok)
  (define (check-type opt-name opt-val)
    (cond
      [(or (assoc opt-name options-spec)
           (assoc opt-name default-options-spec))
       =>
       (λ (ts)
         (define bad?
           (or (and (eq? (cadr ts) 'bool)
                    (not (boolean? opt-val)))

               (and (eq? (cadr ts) 'string)
                    (not (string? opt-val)))

               (and (list? (cadr ts))
                    (not (member opt-val (cadr ts))))))

         (if bad? 'bad-value 'ok))]

      [else 'bad-option]))


  (for/hash ([ast (in-list asts)])

    (when (or (ast:option-extension ast)
              (not (= 1 (length (ast:option-names ast)))))
      (invalid-option ast))

    (case (check-type (car (ast:option-names ast))
                      (ast:option-value ast))

      [(bad-option) (invalid-option ast)]
      [(bad-value) (invalid-option-value ast)]
      [(ok)
       (values (car (ast:option-names ast))
               (ast:option-value ast))])))


;; invalid-option : ast:option? -> !
(define (invalid-option ast [msg "invalid option"])
  (define opt-name-pretty
    (format "\"~a~a\""
            (cond
              [(ast:option-extension ast)
               => (λ (ext) (format "(~a)" ext))]
              [else ""])
            (string-join (ast:option-names ast) ".")))

  (raise-compile-error (ast-loc ast)
                       "~a ~a"
                       msg
                       opt-name-pretty))

;; invalid-option-value : ast:option? -> !
(define (invalid-option-value ast)
  (invalid-option ast "invalid value for option"))



;; (define-nano-pass pass-name
;;   [(dsctor:kind (_ _ pattern _ ...))
;;    body ...
;;    => (_ _ arg _ ...)]    <- optional syntax
;;   ...)
;;
;; the expressions appearing after => are used as arguments to
;; creating a struct with the same type as matched in the beginning
;; of the clause. _'s are substituted for the original arguments of
;; the matched struct.
(define-syntax define-nano-pass
  (syntax-parser
    [(_ fn-name:id
        [(strct:id (pat ...))
         body ...] ...)

     #:with (clause ...)
     (for/list ([stx (in-syntax #'[(strct (pat ...) (body ...)) ...])])
       (syntax-parse stx
         #:datum-literals (=>)
         ; with the =>
         [(strct (pat ...) (body ... => (arg ...)))
          #:with (tmp ...) (generate-temporaries #'[arg ...])
          #:with ((pat+ arg+) ...)
                 (stx-map (syntax-parser
                            [(tmp pat (~datum _)) #'((and pat tmp) tmp)]
                            [(tmp pat arg)        #'(pat arg)])
                          #'((tmp pat arg) ...))
          #'[(struct strct (pat+ ...))
             body ...
             (strct arg+ ...)]]

         ; without
         [(strct (pat ...) (body ...))
          #'[(struct strct (pat ...))
             body ...]]))

     #'(define (fn-name dsc)
         (syntax-parameterize ([this-dsc (make-rename-transformer #'dsc)])
           (match dsc
             clause ...
             [_ dsc])))]))

(define-syntax-parameter this-dsc
  #f)



;; changes dsctor-options from a list of ast:options, into
;; a hash table of valid options to their values
(define-nano-pass pass/options
  [(dsctor:message (_ _ opts _ _ _ _ _ _))
   (define opts+
     (compile-options opts
                      '(["message_set_wire_format" bool]
                        ["no_standard_descriptor_accessor" bool]
                        ["map_entry" bool])))
   => (_ _ opts+ _ _ _ _ _ _)]

  [(dsctor:field (_ _ opts _ _ _ _))
   (define opts+
     (compile-options opts
                      '(["ctype" (STRING CORD STRING_PIECE)]
                        ["jstype" (JS_NORMAL JS_STRING JS_NUMBER)]
                        ["packed" bool]
                        ["lazy" bool])))
   => (_ _ opts+ _ _ _ _)]

  [(dsctor:enum (_ _ opts _))
   (define opts+
     (compile-options opts '(["allow_alias" bool])))
   => (_ _ opts+ _)]

  [(dsctor:oneof (_ _ opts)) => (_ _ (compile-options opts '()))]
  [(dsctor:enum-value (_ _ opts _)) => (_ _ (compile-options opts '()) _)])



;; compiles list of reserved names and ranges, updating
;; dsctor:message-reserved-index? to be a single predicate
;; function (exact-integer? -> bool?) that returns #t for
;; indices that are reserved
(define-nano-pass pass/reserved-fields
  [(dsctor:message (loc _ _ _ _ _ _ rsv-names rsv-idxs))

   (cond
     [(check-duplicates rsv-names)
      => (λ (duplicate)
           (raise-compile-error loc
                                "field name ~v reserved more than once"
                                duplicate))]

     [(check-duplicates rsv-idxs eq?)
      => (λ (duplicate)
           (raise-compile-error loc
                                "index ~a reserved more than once"
                                duplicate))])

   (define (rsv-idx? i)
     (for/or ([rng (in-list rsv-idxs)])
       (cond
         [(exact-integer? rng) (= rng i)]
         [(eq? (ast:range-max rng) 'max)
          (<= (ast:range-min rng) i)]
         [else
          (<= (ast:range-min rng) i (ast:range-max rng))])))

   => (_ _ _ _ _ _ _ rsv-names rsv-idx?)])



;; resolve field type names into fully qualified names
(define-nano-pass pass/resolve-types
  [(dsctor:field (loc _ _ (cons type scope) _ _ _))
   (define type+
     (cond
       [(symbol? type) type]
       [else
        ;; create list of subscopes to check in order
        ;; note: in-subscopes returns deep-scope-first
        ;;         (e.g. ".a.b" => "", ".a", ".a.b")
        ;;   but since we're accumulating onto a list, it
        ;;   reverses the order, making the result shallow-scope-first
        (define type-candidates
          (if (string-prefix? type ".")
              (list type)
              (for/fold ([tcs '()])
                        ([ss (in-subscopes scope)])
                (cons (string-append ss "." type) tcs))))

        ;; find the first matching type
        (or (for*/first ([type-name (in-list type-candidates)]
                         [type (in-value (hash-ref (all-descriptors) type-name #f))]
                         #:when type)
              (cond
                [(or (dsctor:message? type)
                     (dsctor:enum? type))
                 type-name]

                ;; it's not actually a type
                [else
                 (raise-compile-error loc
                                      ;; TODO: better description?
                                      "invalid use of descriptor ~v as a type"
                                      type-name)]))

            (raise-compile-error loc
                                 "cannot find type ~v in scope ~v"
                                 type scope))]))

   => (_ _ _ type+ _ _ _)])



;; check for attempted aliasing (multiple fields/values
;; use the same number), and disallow it unless "allow_alias"
;; is true.
(define-nano-pass pass/check-aliasing
  [(dsctor:message (loc _ _ fields _ _ _ _ _))
   ;; iterate and check fields
   (for ([fq (in-list fields)])
     (let ([field-dsc (get-dsc fq)])
       (unless (positive? (dsctor:field-number field-dsc))
         (raise-compile-error (dsctor-loc field-dsc)
                              "field numbers must be positive"))

       (when (dsctor:message-tag-reserved? this-dsc (dsctor-name field-dsc))
         (raise-compile-error (dsctor-loc field-dsc)
                              "field name ~v is reserved"
                              (dsctor-name field-dsc)))

       (when (dsctor:message-tag-reserved? this-dsc (dsctor:field-number field-dsc))
         (raise-compile-error (dsctor-loc field-dsc)
                              "field number ~a is reserved"
                              (dsctor:field-number field-dsc)))))

   ;; check duplicate numbers (aliasing)
   (cond
     [(check-duplicates #:key dsctor:field-number
                        (map get-dsc (reverse fields)))
      =>
      (λ (field-dsc)
        (raise-compile-error (dsctor-loc field-dsc)
                             "field number ~a used by multiple fields"
                             (dsctor:field-number field-dsc)))]

     [else
      this-dsc])]


  [(dsctor:enum (loc name _ vals))
   ;; no values?
   (when (null? vals)
      (raise-compile-error loc
                           "enum ~v must have at least one value"
                           name))

   ;; first value must be number 0 (lazy protobuf spec lol)
   (let ([first-ev-dsc (get-dsc (first vals))])
     (unless (zero? (dsctor:enum-value-number first-ev-dsc))
       (raise-compile-error (dsctor-loc first-ev-dsc)
                            "first enum value must be number 0")))

   ;; check duplicates, unless "allow_alias" is true
   (cond
     [(dsctor-option this-dsc "allow_alias" #f)
      this-dsc]

     [(check-duplicates #:key dsctor:enum-value-number
                        (map get-dsc (reverse vals)))
      =>
      (λ (ev-dsc)
        (raise-compile-error (dsctor-loc ev-dsc)
                             "cannot alias enum value number ~a without \"allow_alias\" enabled"
                             (dsctor:enum-value-number ev-dsc)))]

     [else
      this-dsc])])


;; substitute fq-names for the descriptors themselves.
;; NOTE: does mutable transformations, since we need
;; to be able to handle cycles.
(define-nano-pass pass/substitute-names
  [(dsctor:message (_ _ _ fields oneofs msgs enums _ _))
   (set-dsctor:message-fields! this-dsc (map get-dsc fields))
   (set-dsctor:message-oneofs! this-dsc (map get-dsc oneofs))
   (set-dsctor:message-nested-types! this-dsc (map get-dsc msgs))
   (set-dsctor:message-nested-enums! this-dsc (map get-dsc enums))
   this-dsc]

  [(dsctor:field (_ _ _ type _ _ oneof))
   (when oneof
     (set-dsctor:field-oneof! this-dsc (get-dsc oneof)))
   this-dsc]

  [(dsctor:enum (_ _ _ vals))
   (set-dsctor:enum-values! this-dsc (map get-dsc vals))
   this-dsc])



(define descriptor-nano-passes
  (list pass/options
        (compose pass/reserved-fields
                 pass/resolve-types)
        pass/check-aliasing
        pass/substitute-names))





;; compile-root : ast:root? -> dsctor:file?
(define (compile-root root-ast)
  (define pkg (ast:root-package root-ast))

  ;; TODO: dependency set

  (parameterize ([current-scope (if (equal? pkg "")
                                    ""
                                    (string-append "." pkg))]
                 [current-oneof-fq-name #f])

    (define-values (root-msg-fqs root-enum-fqs all-unresolved-fqs)
      (parameterize ([current-unresolved-descriptors '()])
        (values (map recursive-descent (ast:root-messages root-ast))
                (map recursive-descent (ast:root-enums root-ast))
                (current-unresolved-descriptors))))

    (for ([pass-fn (in-list descriptor-nano-passes)])
      (hash-union!
       (all-descriptors) #:combine (λ (a b) b)
       (for*/hash ([fq (in-list all-unresolved-fqs)]
                   [dsc (in-value (get-dsc fq))]
                   [dsc+ (in-value (pass-fn dsc))]
                   #:when (not (eq? dsc dsc+)))
         (values fq dsc+))))

    (dsctor:file (ast-loc root-ast)
                 #f
                 (ast:root-options root-ast)
                 (ast:root-package root-ast)
                 '() '() ; deps / public-deps
                 (map get-dsc root-msg-fqs)
                 (map get-dsc root-enum-fqs)
                 (map get-dsc all-unresolved-fqs))))





(module+ tests
  (require rackunit)


  (check-equal? (sequence->list (in-subscopes ".a.b.c"))
                '("" ".a" ".a.b" ".a.b.c"))

  (check-equal? (sequence->list (in-subscopes ""))
                '(""))



  (define L (srcloc #f #f #f #f #f))

  (define-syntax opts-test
    (syntax-rules ()
      [(_ opt-expr ... #:spec (spec ...) #:error re-expr)
       (check-exn
        (λ (e) (and (exn:fail:compile? e)
                    (regexp-match? re-expr (exn-message e))))
        (λ ()
          (compile-options (list opt-expr ...) '(spec ...))))]

      [(_ opt-expr ... #:spec (spec ...) #:ok hash-expr)
       (check-not-exn
        (λ ()
          (check-equal? (compile-options (list opt-expr ...) '(spec ...))
                        hash-expr)))]))


  (opts-test (ast:option L #f '("map_entry") #t)
             (ast:option L #f '("java_package") "opt_tests")
             (ast:option L #f '("optimize_for") 'SPEED)
             (ast:option L #f '("deprecated") #t)
             #:spec (["java_package" string]
                     ["map_entry" bool]
                     ["optimize_for" (SPEED CODE_SIZE)])
             #:ok
             #hash(("map_entry" . #t)
                   ("deprecated" . #t)
                   ("java_package" . "opt_tests")
                   ("optimize_for" . SPEED)))

  (opts-test (ast:option L "some.extension" '() #t)
             #:spec ()
             #:error #px"invalid option \"\\(some\\.extension\\)\"")

  (opts-test (ast:option L #f '("cpp_package") "c++ doesn't have packages lol")
             #:spec ()
             #:error #px"invalid option \"cpp_package\"")

  (opts-test (ast:option L #f '("go_package") #t)
             #:spec (["go_package" string])
             #:error #px"invalid value for option \"go_package\"")

  (opts-test (ast:option L #f '("deprecated") 'maybe?)
             #:spec (["go_package" string])
             #:error #px"invalid value for option \"deprecated\"")

  (opts-test (ast:option L #f '("optimize_for") 'SLOWNESS)
             #:spec (["optimize_for" (SPEED CODE_SIZE)])
             #:error #px"invalid value for option \"optimize_for\"")

  )
