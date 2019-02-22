(shebang!)
(set 'import_statement
     "(import (only scheme define quote string-append display)")
(for-each-cli-arg
 'plugin
 (set 'import_statement '+ "(agidel-plugin $plugin)"))
(set 'import_statement '+ ")")
['csi '$csi_args
      '-batch '-quiet '-eval
      "(begin
        (module agidel_temp (main)
                $import_statement
                (define (main)
                   (display (string-append $(cat /dev/stdin)))))
        (import agidel_temp)
        (main))"]
['echo]
