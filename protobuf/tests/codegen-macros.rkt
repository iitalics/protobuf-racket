#lang racket/base
(module+ test
  (require rackunit
           "../private/codegen/macros.rkt")


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

  (define mv
    (make-move #:duration-case 'sec
               #:duration 1.0
               #:speed-case 'fwd-speed
               #:speed 256.0))

  (check-pred move? mv)
  (check-equal? (move-duration-case mv) 'sec)
  (check-equal? (move-speed-case mv) 'fwd-speed)
  (check-equal? (move-precise mv) #f)

  (check-equal? (move-sec mv) 1.0)
  (check-equal? (move-fwd-speed mv) 256.0)

  (check-equal? (move-ms mv) 0)
  (check-equal? (move-bwd-speed mv) 0.0)
  (check-equal? (move-ang-speed mv) 0.0)

  (check-true (move-has-fwd-speed? mv))
  (check-false (move-has-bwd-speed? mv))
  (check-false (move-has-ang-speed? mv))
  (check-true (move-has-sec? mv))
  (check-false (move-has-ms? mv))



  ;;; test map fields

  (generate-protobuf #:extra-proto-path "files/codegen"
                     #:source "msg3.proto"
                     [codegen.Graph graph]
                     [codegen.Graph.Vert vert])

  (define g
    (make-graph #:vertices
                (hash "A" (make-vert #:edges '("B" "C"))
                      "B" (make-vert #:edges '("C"))
                      "C" (make-vert))))

  (check-equal? (vert-edges (graph-vertices-ref g "A")) '("B" "C"))
  (check-equal? (vert-edges (graph-vertices-ref g "B")) '("C"))
  (check-equal? (vert-edges (graph-vertices-ref g "C")) '())

  )
