(module
 agidel.core
 (disdot dotify parse-string mirror-set! add-to-list add-to-list!
         extension-files)
 (import scheme
         (chicken base)
         (chicken string)
         (chicken port)
         (chicken process-context)
         (chicken file)
         srfi-13
         srfi-1
         (clojurian syntax)
         format)

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

 ;; (add-to-list '(a b) 'c) → (a b c)
 (define (add-to-list lst elt)
   (append lst (list elt)))

 (define-syntax add-to-list!
   (syntax-rules ()
     ((_ lst elt) (set! lst (add-to-list lst elt)))))
 
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


 ;; This function returns list of files which corresponds to list of syntranses
 ;; or plugins to load.
 ;;
 ;; `lst`           : list of syntranses or plugins as symbols.
 ;; `name`          : "plugin" or "syntrans".
 ;; `plural-suffix` : "s"      or "es".
 (define (extension-files lst name plural-suffix)
   (let* ((agidel-dir (get-environment-variable "AGIDEL_DIR"))
          (path (if agidel-dir
                    (string-append agidel-dir "/" name "/")
                    (string-append (get-environment-variable "HOME")
                                   "/.agidel/" name "/")))
          (needed-exts (map (lambda (f) (string-append f ".scm"))
                            (map symbol->string lst)))
          (local-exts (directory path))
          (matched-exts (lset-intersection string=? needed-exts local-exts)))
     (if (eq? (length matched-exts) (length needed-exts))
         (map (lambda (f) (string-append path f)) matched-exts)
         (begin
           (format (current-error-port)
                   "Agidel: could not load ~A~A: ~S\n"
                   name
                   plural-suffix
                   (lset-difference string=? needed-exts matched-exts))
           (exit 1)))))

 )


