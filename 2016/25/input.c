#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
    if (argc < 2)
        return 1;

    int a = 0, b = 0, d = 0;
    d = atoi(argv[1]) + 2541;

    // need d s.t. each division goes odd, even, odd, even

    while (1) {
      a = d;
      // X + 2541 times
      while (a > 0) {
        b = a;
        a = 0;

        a = b / 2;
        b = b % 2 != 0;

        printf("%d,", b);
      }
    }

    return 0;
}
