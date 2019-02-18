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

 ;; Return arg-handler.
 (define (<arg-handler> λ-name signatures)
   (define signature (hash-table-ref signatures (symbol-append '/ λ-name)))

   (define (aquote expr) `(quote ,expr))

   (define (q/e->λ q/e)
     (cond
      ((eq? q/e 'q) aquote)
      ((eq? q/e 'e) aeval)))

   (define (rest-arg-handler . args)
     (define λ (q/e->λ signature))
     (map λ args))
   
   (define (normal-arg-handler . args)
     (define λs (map q/e->λ signature))
     (map (lambda (λ+arg)
            (apply (car λ+arg) (cdr λ+arg)))
          (zip λs args)))

   (define (normal+rest-arg-handler . args)
     (define normal-length (length+ signature))
     (define normal-λs (map q/e->λ (take signature normal-length)))
     (define rest-λ (q/e->λ (drop signature normal-length)))

     (append (map (lambda (λ+arg) ((car λ+arg) (cdr λ+arg)))
                  (zip normal-λs args))
             (map rest-λ (drop args normal-length))))

   (cond
    ((symbol? signature)      rest-arg-handler)
    ((proper-list? signature) normal-arg-handler)
    ((dotted-list? signature) normal+rest-arg-handler)))

 ;; Return aeval object based on plugins list.
 ;; 
 (define (<aeval> plugins)
   (define signatures (plugin/signatures plugins))
   (->> plugins
        (map (lambda (p)
               `(prefix ,(symbol-append 'agidel-plugin. p) /agidel)))
        (cons 'import)
        eval) 

   (match-lambda*
    (('prepare)
     (lambda (expr)
       (cond ((list? expr)
              (let* ((λ-name      (string->symbol (car expr)))
                     (λ-name*     (symbol-append '/agidel/ λ-name))
                     (args        (cdr expr))
                     (arg-handler (<arg-handler> λ-name signatures))
                     (args*       (apply arg-handler args)))
                (cons λ-name* args*)))
             (else expr))))
    (('run)
     (lambda (expr)
       (->> expr
            (map eval)
            string-join))))))
