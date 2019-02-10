(module
 agidel.syntrans
 (compose-λ)
 (import scheme
         (chicken base)
         (prefix (agidel core) agidel/))

 ;; Return list of files with syntranses. `lst` is list of syntranses as
 ;; symbols. The order of files is the same as order in `lst`.
 (define (files lst)
   (agidel/extension-files lst "syntrans" "es"))

 ;; Append '/main to each element of `lst`.
 (define (suffix-/main lst)
   (map (lambda (st) (symbol-append st '/main)) lst))

 ;; Return unary function which accepts a string and returns a string after
 ;; syntax transformation.
 ;;
 ;; `syntranses` is list of syntranses to use.
 (define (compose-λ syntranses)
   (map load (files syntranses)) ; Load all syntrans files
   (eval (cons 'import (agidel/importify syntranses 'syntrans))) ; Import them
   (eval (list 'lambda '(source) ; And create function from them
               (foldl (lambda (acc next) (list next acc))
                      'source
                      (suffix-/main syntranses))))))
