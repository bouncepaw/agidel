#include <stdio.h>
#include <unistd.h>

int main(int argc, char **argv) {
  printf("(import");
  for (int i = 1; i < argc; i++)
    printf("\n (agidel-plugin %s)", argv[i]);
  printf(")\n(display\n");
  char ch;
  while(read(STDIN_FILENO, &ch, 1) > 0) {
    printf("%c", ch);
  }
  printf(")");
}
