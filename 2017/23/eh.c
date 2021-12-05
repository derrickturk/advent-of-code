#include <stdio.h>

int problem2(void)
{
    int composite = 0;

    int b = 106500;
    int c = 123500;
    int g = 0;

    for (; b <= c; b += 17) {
        int prime = 1;
        int d = 2;

        for (d = 2; d < b; ++d) {
            if (b % d == 0) {
                prime = 0;
            }
        }

        if (prime == 0) {
            composite += 1;
            // printf("composite goes up!\n");
        }

        if (b == c) {
            break;
        }
    }

    return composite;
}

int main(void)
{
    printf("%d\n", problem2());
}
