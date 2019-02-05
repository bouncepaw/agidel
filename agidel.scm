#|
This is the main file in the whole Agidel ecosystem.
|#
(import (chicken process-context)
        (srfi 69)
        (srfi 13)
        (prefix (agidel core) agidel/)
        format)

;; Syntax faciliation for `traverse-args`.
(define (string=?2 str o1 o2)
  (or (string=? str o1) (string=? str o2)))

;; Syntax faciliation for `traverse-args`.
(define-syntax set-args-hash
  (syntax-rules ()
    [(_ key val)
     (hash-table-set! args-hash key val)]
    [(_ key fun val)
     (hash-table-set! args-hash
                      key
                      (fun val (hash-table-ref args-hash key)))]))

;; Print help message to stdout. TODO: add actual help here.
(define (show-help-message)
  (format #t "Agidel transpiler. You are welcome!\n"))

;; Parse CLI `args` and return hash map:
;;   key
;;   files:      list of filenames (as strings) to transpile
;;   syntranses: list of syntranses (as symbols) to use
;;   plugins:    list of plugins (as symbols) to use
;; `args` is a list of string CLI arguments. They are returned by function
;;  (command-line-arguments).
(define (traverse-args args)
  (define (loop args-hash args)
    (cond
     ;; When hit end.
     [(null? args) args-hash]
     ;; When asked for help.
     [(string=?2 (car args) "-h" "--help")
      (show-help-message)
      (exit)]
     ;; When setting full syntrans list.
     [(string=?2 (car args) "-s" "--syntranses")
      (set-args-hash 'syntranses (car (agidel/parse-string (cadr args))))
      (loop args-hash (cddr args))]
     ;; When just prepending syntrans list.
     [(string=?2 (car args) "-r" "--prepend-syntranses")
      (set-args-hash 'syntranses append (car (agidel/parse-string (cadr args))))
      (loop args-hash (cddr args))]
     ;; When setting plugin list
     [(string=?2 (car args) "-p" "--plugins")
      (set-args-hash 'plugins (car (agidel/parse-string (cadr args))))
      (loop args-hash (cddr args))]
     ;; Otherwise consider argument as filename.
     [else
      (set-args-hash 'files cons (car args))
      (loop args-hash (cdr args))]))
  (loop (alist->hash-table '((files) (syntranses) (plugins))) args))

