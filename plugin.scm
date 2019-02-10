(module
 agidel.plugin
 (files arities)
 (import scheme
         (chicken base)
         (prefix (agidel core) agidel/)
         (srfi 69))

 ;; Return list of files with plugins. `lst` is list of plugins as symbols. The
 ;; order of files is the same as order in `lst`. 
 (define (files lst)
   (agidel/extension-files lst "plugin" "s"))

 ;; Append '/_agidel-arities to each element of `lst`.
 (define (suffix-/_agidel-arities lst)
   (map (lambda (p) (symbol-append p '/_agidel-arities)) lst))

 ;; Return hash-table, where keys are function names and values are
 ;; lists like that: (q e q q . e). You know that well amigo.
 ;;
 ;; `plugins` is list of plugins to use.
 (define (arities plugins)
   (map load (files plugins)) ; Load all plugin files
   (eval (cons 'import (agidel/importify plugins 'plugin))) ; Import them
   (define raw-arities
     (foldl append '() (map eval (suffix-/_agidel-arities plugins))))
   raw-arities
   )
 )
