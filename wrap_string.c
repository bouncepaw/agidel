#include <unistd.h>
#include <stdio.h>
#include <ctype.h>

enum State { SOMEWHERE, STRING, ESCAPING } state = SOMEWHERE;
char ch;

int main() {
  while(read(STDIN_FILENO, &ch, 1) > 0) {
    switch (state) {
    case STRING:
      switch (ch) {
      case '\\':
        state = ESCAPING;
        putchar(ch);
        break;
      case '\"':
        state = SOMEWHERE;
        printf("\\\"\""); // \""
        break;
      default:
        putchar(ch);
      }
      break;

    case ESCAPING:
      putchar(ch);
      state = STRING;
      break;

    case SOMEWHERE:
    default:
      switch (ch) {
      case '\"':
        state = STRING;
        printf("\"\\\""); // "\"
        break;
      default:
        putchar(ch);
      }
    }
  }

  if (state != SOMEWHERE) {
    fprintf(stderr, "Agidel: unfinished string.\n");
    return 1;
  }

  return 0;
}
