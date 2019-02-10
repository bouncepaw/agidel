#|
This is the main file in the whole Agidel ecosystem.
|#
(import (chicken process-context)
        (chicken io)
        (chicken file)
        (srfi 69)
        (srfi 13)
        (srfi 1)
        (prefix (agidel core) agidel/)
        format
        (clojurian syntax))

(define enable-agilog? #t)
(define (agilog . os)
  (when enable-agilog? (apply format #t os)))


;; Return list of files with syntranses. `lst` is list of syntranses as
;; symbols. The order of files is the same as order in `lst`.
(define (syntrans-files lst)
  (agidel/extension-files lst "syntrans" "es"))



;; Return list of files with plugins. `lst` is list of plugins as symbols. The
;; order of files is the same as order in `lst`. 
(define (plugin-files lst)
  (agidel/extension-files lst "plugin" "s"))


;; When an arg specifying extension to load is not loaded, defaults are applied.
;; This function does exactly that.
(define (apply-defaults args-hash)
  (let* ((hardcoded-hash
          (alist->hash-table '((plugins c)
                               (syntranses discomment
                                           disbrace
                                           disbracket
                                           quotify)
                               (files)))))
    (hash-table-merge args-hash hardcoded-hash)))

;; Syntax faciliation for `traverse-args`.
(define (string=?2 str o1 o2)
  (or (string=? str o1) (string=? str o2)))

;; Syntax faciliation for `traverse-args`.
(define-syntax set-args-hash
  (syntax-rules ()
    [(_ args-hash key val)
     (hash-table-set! args-hash key val)]
    [(_ args-hash key fun val)
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
      (set-args-hash args-hash 'syntranses (car (agidel/parse-string (cadr args))))
      (loop args-hash (cddr args))]
     ;; When just prepending syntrans list.
     [(string=?2 (car args) "-r" "--prepend-syntranses")
      (set-args-hash args-hash 'syntranses append (car (agidel/parse-string (cadr args))))
      (loop args-hash (cddr args))]
     ;; When setting plugin list
     [(string=?2 (car args) "-p" "--plugins")
      (set-args-hash args-hash 'plugins (car (agidel/parse-string (cadr args))))
      (loop args-hash (cddr args))]
     ;; Otherwise consider argument as filename.
     [else
      (set-args-hash args-hash 'files cons (car args))
      (loop args-hash (cdr args))]))
  (loop (alist->hash-table '((files))) args))



(define (compose-syntrans-f syntranses paths)
  (map load paths)
  (let* ((/main-ed-syntranses
          (map (lambda (st) (symbol-append st '/main))
               syntranses))
         (prefixed-syntranses
          (map (lambda (st) (list 'prefix
                                  (symbol-append 'agidel-syntrans. st)
                                  (symbol-append st '/)))
               syntranses)))
    (eval (cons 'import prefixed-syntranses))
    (eval (list 'lambda
                '(source)
                (foldl (lambda (acc next)
                         (list next acc)) 'source /main-ed-syntranses)))))


;; Main
(let* ((args           (apply-defaults (traverse-args (command-line-arguments))))
       (files          (hash-table-ref args 'files))
       (syntrans-paths (agidel/syntrans-files (hash-table-ref args 'syntranses)))
       (plugin-paths   (agidel/plugin-files (hash-table-ref args 'plugins)))
       (syntrans-f     (compose-syntrans-f (hash-table-ref args 'syntranses)
                                           syntrans-paths))
       (parsed-files   (-> (lambda (f)
                             (-> f
                                 open-input-file
                                 (as-> x (read-string #f x))
                                 syntrans-f))
                           (map files)
                           (string-join "\n" 'suffix))))
  (format #t "~A" parsed-files)
  )
