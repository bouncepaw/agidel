(module
 agidel.plugin
 (suffix-/_agidel-arities
  signatures)
 (import scheme
         (chicken base)
         (chicken process-context)
         (chicken string)
         (prefix (agidel core) agidel/)
         (srfi 69))

 ;; Append '/_agidel-arities to each element of `lst`.
 (define (suffix-/_agidel-arities lst)
   (map (lambda (p) (symbol-append p '/_agidel-arities)) lst))
 
 ;; Return hash-table, where keys are function names and values are
 ;; lists like that: (q e q q . e). You know that well amigo.
 ;;
 ;; `plugins` is list of plugins to use.
 (define (signatures plugins)
   (eval (cons 'import (agidel/importify plugins 'plugin))) ; Import them
   (foldl hash-table-merge
          (make-hash-table)
          (reverse (map eval (suffix-/_agidel-arities plugins)))))

 )
