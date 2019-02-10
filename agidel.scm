#|
This is the main file in the whole Agidel ecosystem.
|#
(import (chicken process-context)
        (chicken io)
        (chicken file)
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
       (plugin-paths   (plugin/files (hash-table-ref args 'plugins)))
       (syntrans-λ     (syntrans/compose-λ (hash-table-ref args 'syntranses)))
       (parsed-files   (-> (lambda (f)
                             (-> f
                                 open-input-file
                                 (as-> x (read-string #f x))
                                 syntrans-λ))
                           (map files)
                           (string-join "\n" 'suffix))))
  (format #t "~A" parsed-files)
  )
