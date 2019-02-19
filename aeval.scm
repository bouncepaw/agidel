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
;;;;;;;;;;;;;REDO THIS ALL FROM SCRATCH !!!!!!!!!!!!!
 (define (aquote expr) `(quote ,expr))

 ;; Return object that can apply a quote or eval function to args of an Agidel
 ;; macro.
 ;; `signatures` is the hash-table.
 ;; `quote-λ` is function to call when there is 'q in signature.
 ;; `eval-λ` is function to call when there is 'a in signature.
 ;;
 ;; (define AP sigs aquote aeval)
 ;; (AP 'normal expr)
 ;; (AP 'normal+rest expr)
 ;; (AP 'rest expr)
 (define (<arg-applier-factory> signatures quote-λ eval-λ)
   (define (get-signature λ-name)
     (hash-table-ref signatures (symbol-append '/ λ-name)))

   (define (q/e->λ q/e)
     (cond
      ((eq? q/e 'q) quote-λ)
      ((eq? q/e 'e) eval-λ)))

   (define (rest λ-name . args)
     (define signature (get-signature λ-name))
     (define λ (q/e->λ signature))
     (map λ args))

   (define (normal λ-name . args)
     (define signature (get-signature λ-name))
     (define λs (map q/e->λ signature))
     (map (lambda (λ+arg)
            (apply (car λ+arg) (cdr λ+arg)))
          (zip λs args)))

   (define (normal+rest λ-name . args)
     (define signature (get-signature λ-name))
     (define normal-length (length+ signature))
     (define normal-λs (map q/e->λ (take signature normal-length)))
     (define rest-λ (q/e->λ (drop signature normal-length)))
     (append (map (lambda (λ+arg) ((car λ+arg) (cdr λ+arg)))
                  (zip normal-λs args))
             (map rest-λ (drop args normal-length))))

   (match-lambda*
    (('normal      expr) (apply normal      expr))
    (('normal+rest expr) (apply normal+rest expr))
    (('rest        expr) (apply rest        expr))))


 (define (<expr-preparer> signatures arg-applier)
   (define (get-signature λ-name)
     (hash-table-ref signatures (symbol-append '/ λ-name)))

   (lambda (expr)
     (if (not (list? expr))
         expr
         (let* ((λ-name      (string->symbol (car expr)))
                (signature   (get-signature λ-name))
                (λ-name'     (symbol-append '/agidel/ λ-name))
                (args        (cdr expr)))
           (cons λ-name'
                 (cond
                  ((symbol? signature) (arg-applier 'rest expr))
                  ((dotted-list? signature) (arg-applier 'normal+rest expr))
                  ((proper-list? signature (arg-applier 'normal expr)))))))))


 ;; Return arg-handler.
 ;; `λ-name` is a symbol representing Agidel macro name.
 ;; `signatures` is a hash-table with all the signatures.
 ;;
 ;; arg-handler is a function that makes arguments of an Agidel macro
 ;; transformed to a correct form.
 (define (<arg-handler> λ-name signatures)

   )

 (define (<aeval> plugins)
   (define signatures (plugin/signatures plugins))
   ;; Load all Agidel `plugins`
   (->> plugins
        (map (lambda (p)
               `(prefix ,(symbol-append 'agidel-plugin. p) /agidel)))
        (cons 'import)
        eval)
   (define arg-applier (<arg-applier-factory> signatures))
   (define expr-preparer (<expr-preparer> signatures arg-applier))

   (match-lambda*
    (('prepare expr) (expr-preparer expr))
    (('run expr)
     (lambda (expr)
       (->> expr
            (map eval)
            string-join))))))
