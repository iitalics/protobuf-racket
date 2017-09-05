#lang racket/base
(module+ test
  (require rackunit
           "../private/codegen-macros.rkt")


  ;;; test basic enum usage

  (generate-protobuf #:extra-proto-path "files/codegen"
                     #:source "enum1.proto"
                     [codegen.ProgLang prog-lang])

  (check-pred prog-lang? 'C)
  (check-pred prog-lang? 'Smalltalk)
  (check-pred prog-lang? 'Lisp)
  (check-pred prog-lang? 'LISP)
  (check-false (prog-lang? 'English))

  (for ([sym (in-list '(C Smalltalk Lisp))]
        [num (in-list '(0 1 2))])
    (check-equal? (prog-lang->number sym) num)
    (check-equal? (number->prog-lang num) sym))

  (check-false (prog-lang->number 'English))
  (check-false (number->prog-lang 4))
  (check-equal? (prog-lang->number 'LISP) 2)

  (check-equal? default-prog-lang 'C)



  ;;; test basic message usage

  (generate-protobuf #:extra-proto-path "files/codegen"
                     #:source "msg1.proto"
                     [codegen.Person person])

  (define me (make-person #:name "milo" #:age 19))

  (check-pred person? me)
  (check-equal? (person-name me) "milo")
  (check-equal? (person-age me) 19)
  (check-equal? (person-gf me) #f)

  (check-pred person? default-person)
  (check-equal? (person-name default-person) "")
  (check-equal? (person-age default-person) 0)
  (check-equal? (person-gf default-person) #f)



  ;;; test message oneofs

  (generate-protobuf #:extra-proto-path "files/codegen"
                     #:source "msg2.proto"
                     [codegen.Move move])

  (check-exn
   exn? (λ ()
          (make-move #:duration-case 'sec
                     ;; #:duration not supplied!
                     )))

  (define fast
    (make-move #:duration-case 'sec
               #:duration 1.0
               #:speed-case 'fwd
               #:speed 256.0))

  (check-pred move? fast)
  (check-equal? (move-duration-case fast) 'sec)
  (check-equal? (move-speed-case fast) 'fwd)
  (check-equal? (move-precise fast) #f)

  (check-equal? (move-sec fast) 1.0)
  (check-equal? (move-fwd fast) 256.0)

  (check-equal? (move-ms fast) 0)
  (check-equal? (move-bwd fast) 0.0)
  (check-equal? (move-ang fast) 0.0)

  (check-true (move-has-fwd? fast))
  (check-false (move-has-bwd? fast))
  (check-false (move-has-ang? fast))
  (check-true (move-has-sec? fast))
  (check-false (move-has-ms? fast))



  ;;; test map fields

  (generate-protobuf #:extra-proto-path "files/codegen"
                     #:source "msg3.proto"
                     [codegen.Graph graph]
                     [codegen.Graph.Vert vert])

  (define g
    (make-graph #:vertices
                (hash "A" (make-vert #:edges '("B" "C"))
                      "B" (make-vert #:edges '("C"))
                      "C" (make-vert #:edges '()))))

  (check-equal? (vert-edges (graph-vertices-ref g "A")) '("B" "C"))
  (check-equal? (vert-edges (graph-vertices-ref g "B")) '("C"))
  (check-equal? (vert-edges (graph-vertices-ref g "C")) '())

  )
