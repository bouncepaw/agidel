(shebang!)
(set 'import_statement
     "(import (prefix (only scheme define string-append display)
                      AGIDEL/)
              (only scheme quote)")
(for-each-cli-arg
 'plugin
 (set 'import_statement '+ "(agidel-plugin $plugin)"))
(set 'import_statement '+ ")")
['csi '$csi_args
      '-batch '-quiet '-eval
      "(begin
        (module agidel_temp (main)
                $import_statement
                (AGIDEL/define (main)
                   (AGIDEL/display (AGIDEL/string-append $(cat /dev/stdin)))))
        (import agidel_temp)
        (main))"]
['echo]
