#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
    if (argc < 2)
        return 1;

    int a = 0, b = 0, c = 0, d = 0;
    a = atoi(argv[1]);

    d = a;
    c = 11;
    while (c > 0) {
      b = 231;
      while (b > 0) {
        d++;
        b--;
      }
      c--;
    }

    while (1) {
      a = d;
      while (a > 0) {
        b = a;
        a = 0;
        while (1) {
          c = 2;
          while (c > 0) {
            if (b == 0) { goto BREAK_OUTER; }
            b--;
            c--;
          }
          a++;
        }
BREAK_OUTER:

        b = 2;
        while (1) {
          if (c == 0) { break; }
          b--;
          c--;
        }

        printf("%d,", b);
      }
    }

    return 0;
}
