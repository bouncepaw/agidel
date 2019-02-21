(shebang!)
(set 'csi_args "-batch -quiet ")
(for-each-cli-arg
 'plugin
 (set 'csi_args + "-require-extension agidel-plugin.$plugin "))
['csi '$csi_args '-e "(display (string-append $(cat /dev/stdin)))"]
['echo]
