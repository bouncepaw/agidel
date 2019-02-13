(module
 agidel.syntrans
 (compose-λ)
 (import scheme
         (chicken base)
         (prefix (agidel core) agidel/))

 ;; Append '/main to each element of `lst`.
 (define (suffix-/main lst)
   (map (lambda (st) (symbol-append st '/main)) lst))


 ;; Return unary function which accepts a string and returns a string after
 ;; syntax transformation.
 ;;
 ;; `syntranses` is list of syntranses to use.
 (define (compose-λ syntranses)
   (eval (cons 'import (agidel/importify syntranses 'syntrans))) ; Import them
   (eval (list 'lambda '(source plugins) ; And create function from them
               (foldl (lambda (acc next) (list next acc 'plugins))
                      'source
                      (suffix-/main syntranses))))))
