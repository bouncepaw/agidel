#|
This is the main file in the whole Agidel ecosystem.
|#
(import (chicken process-context)
        (chicken io)
        (chicken file)
        (chicken string)
        (srfi 69)
        (srfi 13)
        (srfi 1)
        (prefix (agidel core) agidel/)
        (prefix (agidel plugin) plugin/)
        (prefix (agidel syntrans) syntrans/)
        (prefix (agidel args) args/)
        format
        (clojurian syntax))

(define enable-agilog? #t)
(define (agilog . os)
  (when enable-agilog? (apply format #t os)))


;; Main
(let* ((args           (args/traverse (command-line-arguments)))
       (files          (hash-table-ref args 'files))
       (syntrans-λ     (syntrans/compose-λ (hash-table-ref args 'syntranses))))
  ;; I put this thing into env variable, because I am not aware of global
  ;; variables in Chicken Scheme. I need a global variable to pass list of
  ;; plugins to `prepare` syntrans. I can't pass it directly to the syntrans
  ;; function, because syntranses are designed to work with strings only.
  (set-environment-variable! "AGIDEL_TMP_PLUGINS"
                             (->string (map string->symbol
                                            (hash-table-ref args 'plugins))))
  (format #t "~A" (-> (lambda (f)
                        (-> f
                            open-input-file
                            (as-> x (read-string #f x))
                            syntrans-λ))
                      (map files)
                      (string-join "\n" 'suffix)))
  )
