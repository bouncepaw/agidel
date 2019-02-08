#|
This is the main file in the whole Agidel ecosystem.
|#
(import (chicken process-context)
        (srfi 69)
        (srfi 13)
        (srfi 1)
        (prefix (agidel core) agidel/)
        format)

;; This function returns list of files which corresponds to list of syntranses
;; or plugins to load.
;;
;; `lst` is list of syntranses or plugins as symbols.
;; `path` is path where to search for files.
;; `name` is "plugins" or "syntranses".
(define (extension-files lst path name)
  (let* ((needed-exts (map (lambda (f) (string-append f ".scm"))
                           (map symbol->string lst)))
         (local-exts (directory path))
         (matched-exts (lset-intersection string=? needed-exts local-exts))))
  (if (not (eq? (length matched-exts) (length needed-exts)))
      (begin
        (fprintf (current-error-port)
                 "Agidel: could not load ~A: ~S"
                 name
                 (lset-difference string=? needed-exts matched-exts))
        (exit 1))
      (map (lambda (f) (string-append path f)) matched-exts)))

;; Return list of files with plugins. `lst` is list of plugins as symbols.
(define (plugin-files lst)
  (let* ((agidel-dir (get-environment-variable "AGIDEL_DIR"))
         (path (if (not agidel-dir)
                   (string-append (get-environment-variable "HOME")
                                  "/.agidel/syntrans")
                   (string-append agidel-dir
                                  "/syntrans"))))
    (extension-files lst path "syntranses")))

;; Return list of files with syntranses. `lst` is list of syntranses as symbols.
(define (syntrans-files lst)
  (let* ((agidel-dir (get-environment-variable "AGIDEL_DIR"))
         (path (if (not agidel-dir)
                   (string-append (get-environment-variable "HOME")
                                  "/.agidel/plugin")
                   (string-append agidel-dir
                                  "/plugin"))))
    (extension-files lst path "plugins")))


;; When an arg specifying extension to load is not loaded, defaults are applied.
;; This function does exactly that.
(define (apply-defaults args-hash)
  (let* ((hardcoded-hash
          (alist->hash-table '((plugins c)
                               (syntranses discomment
                                           disbrace
                                           disbracket
                                           quotify
                                           aeval)
                               (files)))))
    (hash-table-merge args-hash hardcoded-hash)))

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



(let* ((args-traversed (traverse-args (command-line-arguments)))
       (args-defaulted (apply-defaults args-traversed))
       (files          (hash-table-ref args-defaulted 'files))
       (syntranses     (hash-table-ref args-defaulted 'syntranses))
       (plugins        (hash-table-ref args-defaulted 'plugins))
       (syntrans-paths (syntrans-files syntranses))
       (plugin-paths   (plugin-files plugins)))
  #| do later
  (load-syntranses syntranses)
  (load-plugins plugins)
  (for-each
  (lambda (file)
  (execute (syntrans file)))
  files)
  |#
  )
