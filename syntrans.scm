(module
 agidel.syntrans
 (files)
 (import scheme
         (chicken base)
         (prefix (agidel core) agidel/))

 ;; Return list of files with syntranses. `lst` is list of syntranses as
 ;; symbols. The order of files is the same as order in `lst`.
 (define (files lst)
   (agidel/extension-files lst "syntrans" "es")))
