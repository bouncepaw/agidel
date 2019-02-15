# What is Agidel?

Agidel is an extensible macro processor. You can create your own
macros in any way you want. As of version 1.0 (yet to be released),
only Lisp-like S-expression-based syntax is available, however the
architecture of Agidel allows anyone to extend syntax however they
like.

## Extensiblity

Agidel is built from three things:

- The Agidel transpiler. It's a program that arranges things below so
  everything works.
- Syntax transformers (syntranses for short). These are special
  facilities that get source code on input and output modified code to
  the next syntrans. Each syntrans is meant to do one thing. For
  example:
  - `discomment` strips all comments from source code and outputs
    comment-less code.
  - `disbrace` extends S-expression syntax with support for braces.
  - etc.

  After syntax transformation, transpiler ends up with valid Scheme
  code that uses functions from…
- Plugins. These are Scheme modules that provide functions for final
  step of transpilation — evaluation. For example (NB all those
  plugins are not made yet):
  - `c` adds support for generating C programs.
  - `json` adds support for generating JSON documents.

  These functions are meant to return strings that are put into final
  code. Since they behave just like Lisp macros (they get source code
  and return modified source code), they are called Agidel macros.
  
Everything above is written in Scheme and is distributed through
Chicken Eggs repositories. To make your own syntrans or plugin you'll
need to create your own egg, but it's not required to publish it.

## Examples

Author has originally built Agidel to be able to write C source code
in S-expressions like that:

```lisp
(import stdio.h)

(defun (main int) ()
   [printf "Hello world!"]
   (return 0))
```

It then transforms to this:

```c
#include <stdio.h>

int main() {
    printf("Hello world!");
    return 0;
}
```

<hr>

Bash example:
```lisp
(define location ~)
(defun move (filename)
   [mv %filename $location]
   [echo Move %filename to $location.])
(each-string-in-file "files.txt" as file
  [move $file])
```

```bash
location=~
function move {
    mv $1 $location
    echo Move $1 to $location
}
while read file
do
    move "$file"
done < files.txt
```
