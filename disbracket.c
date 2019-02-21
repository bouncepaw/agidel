// disbracket.c
//  Part of Agidel project.
//
//  This simple program reads Agidel source code, finds any
//  bracket-enclosed expression, converts them to their true form and
//  prints converted source code to stdout.
// 
//  [a b c] → (_bracket a b c)

#include <stdbool.h>
#include <unistd.h>
#include <stdio.h>

bool in_string, escaping_char;
// Counter of opened [] pairs which are not closed yet.
int pairs_in_search;

int main() {
  char ch;
  while(read(STDIN_FILENO, &ch, 1) > 0) {
    if (in_string) {
      if (escaping_char) {
        printf("\%c", ch);
        escaping_char = false;
      } else {
        if (ch == '\\') {
          escaping_char = true;
        } else if (ch == '"') {
          printf("%c", ch);
          in_string = false;
        } else {
          printf("%c", ch);
        }
      }
    } else {
      if (ch == '[') {
        printf("(_bracket ");
        pairs_in_search++;
      } else if (ch == ']') {
        printf(")");
        pairs_in_search--;
      } else if (ch == '"') {
        in_string = true;
        printf("%c", ch);
      } else {
        printf("%c", ch);
      }
    }
  }

  if (pairs_in_search != 0) {
    fprintf(stderr, "Agidel: %d unmatched bracket pairs.", pairs_in_search);
    return 1;
  }

  return 0;
}
