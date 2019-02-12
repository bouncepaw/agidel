(module
 agidel.plugin
 (suffix-/_agidel-arities
  arities
  save-needed-plugins
  needed-plugins
  free-needed-plugins)
 (import scheme
         (chicken base)
         (chicken process-context)
         (chicken string)
         (prefix (agidel core) agidel/)
         (srfi 69))

 ;; Append '/_agidel-arities to each element of `lst`.
 (define (suffix-/_agidel-arities lst)
   (map (lambda (p) (symbol-append p '/_agidel-arities)) lst))

 ;; I put this thing into env variable, because I am not aware of global
 ;; variables in Chicken Scheme. I need a global variable to pass list of
 ;; plugins to `prepare` syntrans. I can't pass it directly to the syntrans
 ;; function, because syntranses are designed to work with strings only.
 (define (save-needed-plugins plugins)
   (set-environment-variable! "AGIDEL_TMP_PLUGINS" (->string plugins)))

 ;; Get list of all needed plugins from env var AGIDEL_TMP_PLUGINS
 (define (needed-plugins)
   (car (agidel/parse-string (get-environment-variable "AGIDEL_TMP_PLUGINS"))))

 (define (free-needed-plugins)
   (set-environment-variable! "AGIDEL_TMP_PLUGINS" ""))
 
 ;; Return hash-table, where keys are function names and values are
 ;; lists like that: (q e q q . e). You know that well amigo.
 ;;
 ;; `plugins` is list of plugins to use.
 (define (arities plugins)
   (eval (cons 'import (agidel/importify plugins 'plugin))) ; Import them
   (foldl hash-table-merge
          (make-hash-table)
          (reverse (map eval (suffix-/_agidel-arities plugins)))))

 )
