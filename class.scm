(module
 agidel.class
 *
 (import scheme
         (chicken base)
         coops)

 (define-class <a-meta> ()
   ((val initform: "")))

 (define-class <a-number> (<a-meta>)
   ((val initform: "0")))

 (define-class <a-string> (<a-meta>)
   ((val initform: "\"\"")))

 (define-class <a-symbol> (<a-meta>)
   ((val initform: "?")))

 (define-class <a-bool> (<a-meta>)
   ((val initform: "false")))

 (define-method (as-string (obj <a-meta>))
   (slot-value obj 'val))
 (define-method (as-inside-string (obj <a-string>))
   (let* [[val (slot-value obj 'val)]
          [len (string-length val)]]
     (substring val 1 (- len 1))))

 (define-method (as-number (obj <a-number>))
   (string->number (slot-value obj 'val)))
 (define-method (as-number (obj <a-bool>))
   (if (string=? (slot-value obj 'val) "true") 1 0))

 (define-method (as-symbol (obj <a-symbol>))
   (string->symbol (slot-value obj 'val)))
 (define-method (as-symbol (obj <a-bool>))
   (string->symbol (slot-value obj 'val)))

 (define-method (as-bool (obj <a-bool>))
   (if (string=? (slot-value obj 'val) "true") #t #f))

 )
