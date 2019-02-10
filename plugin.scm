(module
 agidel.plugin
 (files)
 (import scheme
         (chicken base)
         (prefix (agidel core) agidel/))

 ;; Return list of files with plugins. `lst` is list of plugins as symbols. The
 ;; order of files is the same as order in `lst`. 
 (define (files lst)
   (agidel/extension-files lst "plugin" "s")))
