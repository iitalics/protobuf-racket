#lang racket/base
(require "ast.rkt"
         "parser.rkt"
         racket/path
         racket/list)

(provide default-proto-paths
         extra-proto-paths
         current-proto-paths
         resolve-file
         parse+dependencies
         (struct-out exn:fail:user:dependency-cycle))

; google's source does this is in a strange way, and doesn't document it very well.
; what we'll do instead is have "default-proto-paths" like /usr/include, and
; "extra-proto-paths" where you can specify your own; then, "current-proto-paths" will give
; us a combination of these plus the current directory

;; todo: take windows into account?
(define default-proto-paths
  (make-parameter
   (list (bytes->path #"/usr/include/" 'unix)
         (bytes->path #"/usr/local/include/" 'unix))))

(define extra-proto-paths
  (make-parameter '()))

(define (current-proto-paths)
  (map simple-form-path
       (append (list (current-directory))
               (extra-proto-paths)
               (default-proto-paths))))

;; resolve-file : path-string? -> (or/c #f complete-path?)
;; finds the file that the given path-string refers to, by looking in the proto paths
;; returns #f if no file exists
(define (resolve-file p)
  (if (absolute-path? p)
      (and (file-exists? p)
           (simple-form-path p))
      (for/or ([base (in-list (current-proto-paths))])
        (let ([full (build-path base p)])
          (and (file-exists? full)
               (simple-form-path full))))))


;; parse+dependencies : (listof path-string?) -> (listof ast:file?)
;;   parses .proto files from the given paths, and all files referenced
;;   by those paths. returns the parsed ASTs in the correct dependency order
;;   such that no AST depends on one that appears before it.
;;   the exact order is unspecified for paths which do not depend on eachother.
(define (parse+dependencies initial-paths)

  ;; preorders : path? => (listof path?)
  ;;  specifies a paths that the key depends on
  ;;  e.g. if (hash-ref preoders p0) = (list p1 p2)
  ;;    then p0 requires p1, p2
  ;;       ; p1 ≤ p0
  ;;       ; p2 ≤ p0
  (define preorders (make-hash))

  ;; asts : (listof ast:root?)
  (define asts '())

  ;; traverse-deps : path-string? (listof path?) -> complete-path?
  ;;  resolve & parse a file. takes a list of 'pending' paths such that
  ;;  if the given file depends on one of those paths, we've created
  ;;  a dependency cycle and should abort.
  ;;  returns the resolved version of the given path, and adds an
  ;;  entry to the assoc-list 'asts'
  (define (traverse-deps path [pending '()])
    (let ([resolved-path
           (or (resolve-file path)
               (raise-not-found path))])
      (cond
        [(member resolved-path pending)
         => (λ _
              (raise-cycle-error resolved-path
                                 (car pending)))]

        [(not (hash-has-key? preorders resolved-path))
         ; parse the AST and extract the dependencies
         (let* ([root (parse-ast resolved-path)]
                [dep-paths (parameterize ([current-directory (path-only resolved-path)])
                             (for/list ([imp (in-list (ast:root-imports root))])
                               (traverse-deps (ast:import-path imp)
                                              (cons resolved-path pending))))])
           ; update preorders and AST list
           (set! asts (cons root asts))
           (hash-set! preorders resolved-path dep-paths)
           ; update resolved paths within each import
           (for ([dep-path (in-list dep-paths)]
                 [imp (in-list (ast:root-imports root))])
             (set-box! (ast:import-resolved-path-box imp) dep-path)))])

      resolved-path))

  (for-each traverse-deps initial-paths)

  ;; evaluate a preorder using reflexivity or transitivity
  (define (dep<=? path1 path2) (dep>=? path2 path1))
  (define (dep>=? path1 path2)
    (or (equal? path1 path2)
        (ormap (λ (p) (dep>=? p path2))
               (hash-ref preorders path1 '()))))

  ;; sort paths by preorder and return corresponding asts
  (sort asts dep<=? #:key ast-source-file-path))



(define (raise-not-found path)
  (raise (exn:fail:filesystem (format "cannot find protobuf file ~v"
                                      (if (string? path) path
                                          (path->string path)))
                              (current-continuation-marks))))

(define-struct (exn:fail:user:dependency-cycle exn:fail:user) (paths))

(define (raise-cycle-error path1 path2)
  (raise (make-exn:fail:user:dependency-cycle
          (format "cyclic dependencies found between ~v and ~v"
                  (path->string (file-name-from-path path1))
                  (path->string (file-name-from-path path2)))
          (current-continuation-marks)
          (list path1 path2))))
