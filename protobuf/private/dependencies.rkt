#lang racket/base
(require "ast.rkt"
         racket/path)

(provide default-proto-paths
         extra-proto-paths
         current-proto-paths
         resolve-file
         parse+dependencies)

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


;; parse+dependencies : (listof string/path?) -> (listof ast:file?)
;;   parses .proto files from the given paths, and all files referenced
;;   by those paths. returns the parsed ASTs in the correct dependency order
;;   such that no AST depends on one that appears before it
(define (parse+dependencies initial-paths)

  ;; TODO: create a dependency tree, then collect them
  ;;  with post-order traversal

  (error "unimplemented"))
