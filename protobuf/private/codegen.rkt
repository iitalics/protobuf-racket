#lang racket/base
(require (for-meta 1
                   racket
                   racket/syntax
                   syntax/parse
                   syntax/stx
                   "descriptors.rkt"
                   "dependencies.rkt"
                   "compiler.rkt")
         racket/base
         racket/contract/base)


(provide load-protobuf)


#|
for fields of messages:
  get-{field}                  ;; returns value, or default value if unset
  set-{field}                  ;; sets to given value
  clear-{field}                ;; sets to default value
for fields with enum type:
  get-{field}-number           ;; returns integer value of enum, rather than symbol

for repeated fields:
  get-{field}-count        ;; return length
  get-{field}              ;; return list of values
  add-{field}              ;; add a new element to list. if no argument suplied, creates default
  add-{field}*             ;; append a list of elements
  clear-{field}            ;; set to empty list

for map fields:
  get-{field}              ;; returns assoc list of key/values
  get-{field}-hash         ;; returns immutable hash
  put-{field}              ;; set a key of the map
  {field}-ref              ;; get a value based on the key (optional 'default' argument)
  clear-{field}            ;; make empty


GENERATED:
protobuf:{package}.{message/enum-name}
descriptor:{package}.{message/enum-name}

messages:
  bindings: {message}%
  predicate: (is-a? {message}%)
  make-default: (new {message}%)

enums:
  bindings: {enum}?
            number->{enum}
            {enum}->number
  predicate: {enum}?
  make-default: '<first-enum>
|#


(begin-for-syntax

  ;; a protobuf type generated into racket syntax
  (struct generated-type
    (;; TODO: some way to determine if the bindings come
     ;;  from a stale .proto file (e.g. need to be regenerated)
     bindings
     predicate-expr
     make-default-expr)
    #:prefab)

  ;; a binding that can be renamed,
  ;; e.g. {enum}? would be
  ;;      (rename-binding "~a?" #'<some-internal-binding>)
  (struct rename-binding
    (format-str internal-id)
    #:prefab)


  (define new-definitions
    (make-parameter '()))

  (define current-todo-types
    (make-parameter (make-hash)))

  (define-syntax %-generate-new-type
    (syntax-rules (=>)
      [(_ desc-expr
          (fmt-str => int-id) ...
          #:predicate pred-expr
          #:default mk-def-expr
          defn-stx-expr)
       (let* ([desc desc-expr]
              [gen-ty (generated-type (list (rename-binding fmt-str #`int-id) ...)
                                      pred-expr
                                      mk-def-expr)])
         (hash-set! (current-todo-types) desc gen-ty)
         (new-definitions (cons defn-stx-expr (new-definitions)))
         gen-ty)]))


  ;; either generate the definition for the descriptor,
  ;; or find an already-generated definition
  (define (descriptor->generated-type desc
                                      [full-name (send desc get-full-name)])
    (cond
      [(hash-ref (current-todo-types) desc #f) => values]

      ;; TODO: use syntax-local-value to allow modules
      ;;       to (provide ..) protobuf type implementations
      ;;       using some sort of protobuf:{package}.{type} symbol
      ;[(syntax-local-value

      [(is-a? desc enum-descriptor%)
       (generate-enum! desc)]

      [else
       (error "cannot generate type " full-name)]))


  ;; generate 'generated-type' for the given enum descriptor
  ;; additionally, adds it to 'current-todo-types', and adds
  ;; definitions to 'new-definitions'
  ;;
  ;; generate-enum! : enum-descriptor% -> generated-type?
  (define (generate-enum! desc)
    (syntax-parse (generate-temporaries (range 3))
      [( E? num->E E->num )

       #:with ([num sym] ...)
         (map (λ (ev)
                (list (send ev get-number)
                      ;; TODO: lispify?
                      (string->symbol (send ev get-name))))
              (send desc get-values))
       #:with (fst-sym . _) #'(sym ...)

       #:with full-E?   (format-id #f "~a?" (send desc get-full-name))
       #:with full-n->E (format-id #f "number->~a" (send desc get-full-name))
       #:with full-E->n (format-id #f "~a->number" (send desc get-full-name))

       (%-generate-new-type desc
        ("~a?"        => E?)
        ("number->~a" => num->E)
        ("~a->number" => E->num)
        #:predicate #'E?
        #:default #''fst-sym

        #'(define-values (E? num->E E->num)
            ;; the sole purpose of this let-binding is to
            ;; install names to the procedures, so that
            ;; they appear as #<procedure:package.path.TypeName>
            ;; when pretty printed, not as the random internal name
            (let ([full-E? (procedure-rename (or/c 'sym ...) 'full-E?)]
                  [full-n->E (λ (i)
                               (case i
                                 [(num) 'sym] ...
                                 [else #f]))]

                  [full-E->n (λ (e)
                               (case e
                                 [(sym) 'num] ...
                                 [else #f]))])

              (values full-E? full-n->E full-E->n))))]))


  ;; return a list of old/new bindings for all
  ;; generated bindings associated with the type.
  ;;
  ;; get-binds : stx? string? symbol? -> (listof (list id?<old> id?<new>))
  (define (get-binds stx full-name new-id-sym)
    (define desc (hash-ref (all-descriptors)
                           full-name
                           (λ ()
                             (raise-syntax-error #f "undefined protobuf type" stx))))

    (unless (or (is-a? desc descriptor%)
                (is-a? desc enum-descriptor%))
      (raise-syntax-error #f (format "~a is not a type" full-name) stx))

    (define gen-ty
      (descriptor->generated-type desc full-name))

    (for/list ([b (in-list (generated-type-bindings gen-ty))])
      (list (rename-binding-internal-id b)
            (format-id stx (rename-binding-format-str b) new-id-sym))))



  ;; for use in #:export (...)
  (define-syntax-class protobuf-export
    #:datum-literals ()
    #:attributes (full-path new-id)
    ;; #:export ([name rename-out])
    (pattern [full-path-id:id new-id:id]
             #:with full-path (symbol->string (syntax-e #'full-path-id)))

    ;; #:export (name)
    (pattern both-id:id
             #:with full-path (symbol->string (syntax-e #'both-id))
             #:with new-id #'both-id))

  )


(define-syntax load-protobuf
  (syntax-parser
    [(_ path:str ...
        (~or (~seq #:extra-proto-path x-proto-path)
             (~seq #:export (export:protobuf-export ...))) ...)

     ;; compile the paths (and any dependencies)
     #:do [(parameterize ([extra-proto-paths
                           (append (extra-proto-paths)
                                   (syntax->datum #'[x-proto-path ...]))])
             (for ([root (in-list (parse+dependencies
                                   (syntax->datum #'[path ...])))])
               (compile-root root)))]

     ;; generate all bindings requested by exports, and
     ;; rename them properly
     #:with ((([old-id new-id] ...) ...)
             [defn ...])
     (parameterize ([new-definitions '()])
       (list (stx-map (λ (args)
                        (with-syntax ([(src full-name new-id) args])
                          (get-binds #'src
                                     (syntax-e #'full-name)
                                     (syntax-e #'new-id))))
                      #'([export export.full-path export.new-id] ... ...))
             (new-definitions)
             ;; TODO: export generated-types from current-todo-types
             ;;       as identifier e.g. protobuf:tests.files.Color
             ))

     #'(begin
         defn ...
         (define new-id old-id) ... ...)]))
