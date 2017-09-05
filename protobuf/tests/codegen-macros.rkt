#lang racket/base
(module+ test
  (require rackunit
           "../private/codegen-macros.rkt")

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


  (generate-protobuf #:extra-proto-path "files/codegen"
                     #:source "msg2.proto"
                     [codegen.Move move])

  (define moves
    (list (make-move #:duration-case 'sec
                     #:duration 30
                     #:speed-case 'fwd
                     #:speed 128)
          (make-move #:duration-case 'ms
                     #:duration 4500
                     #:speed-case 'ang
                     #:speed -64)))

  (check-equal? (map move-speed-case moves) '(fwd ang))
  (check-equal? (map move-duration-case moves) '(sec ms))

  (check-exn
   exn? (λ ()
          (make-move #:duration-case 'sec)))

  )
