(module
 agidel.args
 (traverse)
 (import scheme
         (chicken base)
         (srfi 69)
         format
         (prefix (agidel core) agidel/))

 
 ;; When an arg specifying extension to load is not loaded, defaults are
 ;; applied. This function does exactly that.
 (define (apply-defaults args-hash)
   (define default-hash
     (alist->hash-table '((plugins c)
                          (syntranses discomment
                                      disbrace
                                      disbracket
                                      quotify)
                          (files))))
   (hash-table-merge args-hash default-hash))

 ;; Syntax faciliation for `traverse`.
 (define (string=?2 str o1 o2)
   (or (string=? str o1) (string=? str o2)))

 ;; Syntax faciliation for `traverse`.
 (define-syntax set-args-hash
   (syntax-rules (args-hash)
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
 (define (traverse args)
   (apply-defaults
    (let loop ((args-hash (alist->hash-table '((files))))
               (args args))
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
        (loop args-hash (cdr args))])))))
