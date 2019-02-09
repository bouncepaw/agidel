(module
 agidel.core
 (disdot dotify parse-string mirror-set!)
 (import scheme
         (chicken base)
         (chicken string)
         (chicken port)
         (chicken process-context)
         (chicken file)
         srfi-13
         srfi-1
         (clojurian syntax)
         )

 #|
 ;;; Global constants
 ;; Main Agidel directory.
 (define agidel-dir (get-environment-variable "AGIDEL_DIR"))
 ;; Directory where Agidel plugin modules are stored.
 (define plugins-dir (get-environment-variable "AGIDEL_PLUGINS_DIR"))
 ;; Plugin names that are installed in `plugins-dir`.
 (define installed-plugins (map string->symbol
                                (filter
                                 (lambda (file) (car (string-split file ".")))
                                 (directory plugins-dir))))
 ;; Plugins that are asked to be loaded by the transpiler.
 (define plugins-to-load (get-environment-variable "AGIDEL_LOAD_PLUGINS"))
 |#

 ;; (disdot '(1 2 3 .4)) → (1 2 3 4)
 (define (disdot dotted-list)
   (append (take dotted-list (length+ dotted-list))
           (list (cdr (take-right dotted-list 1)))))

 ;; (dotify '(1 2 3 4)) → (1 2 3 . 4)
 (define (dotify proper-list)
   (append (take proper-list (- (length proper-list) 1))
           (car (take-right proper-list 1))))


 ;; `source-string`: string with `prepare`d Agidel source code. This
 ;; function parses it and returns it as list of lists, where each
 ;; list if result of reading a list in `source-string`. Like that:
 ;;
 ;; (parse-string "(foo bar) (baz)")
 ;; ⇒ ((foo bar) (baz))
 (define (parse-string source-string)
   (with-input-from-string source-string
     (lambda () (port-map (lambda (x) x) read))))
 
 ;; As it can happen so that no `set!` is available when creating a new Agidel
 ;; plugin, this module provides its 'mirror'.
 (define-syntax mirror-set!
   (syntax-rules ()
     ((_ var expr) (set! var expr))))

 )
