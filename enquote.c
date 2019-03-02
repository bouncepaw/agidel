#include <stdbool.h>
#include <unistd.h>
#include <stdio.h>
#include <ctype.h>

enum State { LIST, STRING, ESCAPING, SYMBOL} state = LIST;
bool in_car = false;
int nestlvl = 0,
  spacecnt = 0;
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
      spacecnt++;
      break;

    case LIST:
      switch (ch) {
      case '(':
        nestlvl++;
        spacecnt = 0;
        putchar(ch);
        break;
      case ')':
        nestlvl--;
        spacecnt = 1;
        putchar(ch);
        break;
      case '\"':
        state = STRING;
        putchar(ch);
        break;
      default:
        if (isspace(ch)) {
          putchar(ch);
          spacecnt++;
        } else {
          state = SYMBOL;
          if (spacecnt > 0) printf("'%c", ch);
          else putchar(ch);
        }
      }
    }
  }

  return 0;
}
