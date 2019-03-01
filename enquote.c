#include <stdbool.h>
#include <unistd.h>
#include <stdio.h>
#include <ctype.h>

enum State { LIST, STRING, ESCAPING, SYMBOL} state = LIST;
int nestlvl = 0;
char ch;

int main() {
  while(read(STDIN_FILENO, &ch, 1) > 0) {
    switch (state) {
    case ESCAPING:
      putchar(ch);
      state = STRING;
      break;
    case STRING:
      switch (ch) {
      case '\\':
        state = ESCAPING;
        putchar(ch);
        break;
      case '\"':
        state = LIST;
        putchar(ch);
        break;
      default:
        putchar(ch);
        break;
      }
      break;

    case SYMBOL:
      if (isspace(ch)) state = LIST;
      putchar(ch);
      break;

    case LIST:
      switch (ch) {
      case '(':
        nestlvl++;
        putchar(ch);
        break;
      case ')':
        nestlvl--;
        putchar(ch);
        break;
      case '\"':
        state = STRING;
        putchar(ch);
        break;
      default:
        if (isspace(ch)) {
          putchar(ch);
        } else {
          state = SYMBOL;
          printf("'%c", ch);
        }
      }
    }
  }

  return 0;
}
