(module
 agidel.aeval
 (<aeval>)
 (import scheme
         (chicken base)
         (prefix (agidel core) core/)
         (prefix (agidel plugin) plugin/)
         (srfi 1)
         (srfi 13)
         (srfi 69)
         (clojurian syntax)
         format
         matchable)

 ;; Return aeval object based on plugins list.
 ;; 
 (define (<aeval> plugins)
   (define signatures (plugin/signatures plugins))
   (->> plugins
        (map (lambda (p)
               `(prefix ,(symbol-append 'agidel-plugin. p) /agidel)))
        (cons 'import)
        eval)
   (define (make-parser name-of-λ)
     (define signature (hash-table-ref signatures (symbol-append '/ name-of-λ)))

     (define (aquote expr) `(quote ,expr))

     (define (q/e->λ q/e)
       (cond
        ((eq? q/e 'q) aquote)
        ((eq? q/e 'e) aeval)))

     (define (rest-parser . args)
       (define λ (q/e->λ signature))
       (map λ args))
     
     (define (normal-parser . args)
       (define λs (map q/e->λ signature))
       (map (lambda (λ+arg)
              (apply (car λ+arg) (cdr λ+arg)))
            (zip λs args)))

     (define (normal+rest-parser . args)
       (define normal-length (length+ signature))
       (define normal-λs (map q/e->λ (take signature normal-length)))
       (define rest-λ (q/e->λ (drop signature normal-length)))

       (append (map (lambda (λ+arg) ((car λ+arg) (cdr λ+arg)))
                    (zip normal-λs args))
               (map rest-λ (drop args normal-length))))

     (cond
      ((symbol? signature)      rest-parser)
      ((proper-list? signature) normal-parser)
      ((dotted-list? signature) normal+rest-parser)))

   (match-lambda*
    (('prepare)
     (lambda (expr)
          (cond ((list? expr)
                 (let* ((name-of-λ   (string->symbol (car expr)))
                        (name-of-λ*  (symbol-append '/agidel/ name-of-λ))
                        (args        (cdr expr))
                        (parser      (make-parser name-of-λ))
                        (parsed-args (apply parser args)))
                   (cons name-of-λ* parsed-args)))
                (else expr))))
    (('run)
     (lambda (expr)
       (->> expr
            (map eval)
            string-join))))))
