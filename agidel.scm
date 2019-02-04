#|
This is the main file in the whole Agidel ecosystem.
|#
(import (chicken process-context)
        (srfi 69)
        (srfi 13)
        (prefix (agidel core) agidel/)
        format)


(define ((show-help-message))
  (format #t "Agidel transpiler. You are welcome!\n"))

(define (traverse-args args)
  (let loop [[args-hash (alist->hash-table '((files)
                                             (syntranses)
                                             (plugins)))]
             [args args]]
    (cond
     ;; When parsed all arguments.
     [(null? args) args-hash]
     ;; When asked for help, show it and exit.
     [(or (string=? (car args) "-h")
          (string=? (car args) "--help"))
      (show-help-message)
      (exit)]
     ;; When arg is a filename. As you can see, no files starting with a dash
     ;; are supported. That's the design.
     [(not (string-prefix? (car args) "-"))
      (hash-table-set! args-hash
                        'files
                        (cons (car args) (hash-table-ref args-hash 'files)))
      (loop args-hash (cdr args))]
     ;; Otherwise, parse as option.
     [else
      (apply hash-table-set!
             args-hash
             (case (car args)
               [(-s --syntranses)
                (list 'syntranses (agidel/parse-string (cadr args)))]
               [(-r --prepend-syntranses)
                (list 'syntranses
                      (append (cadr args)
                              (hash-table-ref args-hash 'syntranses)))]
               [(-p --plugins)
                (list 'plugins (cadr args))]))
      (loop args-hash (cddr args))])
    ))

(format #t "~A" (hash-table->alist (traverse-args (command-line-arguments))))
