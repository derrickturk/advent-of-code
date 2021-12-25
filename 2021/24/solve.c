#include <stdio.h>

int valid(int input[14])
{
    int prevZ[14] = { 0 };
    prevZ[0] = (input[0] + 7) * (12 != input[0]);
    prevZ[1] = prevZ[0] * (25 * (prevZ[0] % 26 + 11 != input[1]) + 1) + (input[1] + 15) * (prevZ[0] % 26 + 11 != input[1]);
    prevZ[2] = prevZ[1] * (25 * (prevZ[1] % 26 + 12 != input[2]) + 1) + (input[2] + 2) * (prevZ[1] % 26 + 12 != input[2]);
    prevZ[3] = prevZ[2] / 26 * (25 * (prevZ[2] % 26 + -3 != input[3]) + 1) + (input[3] + 15) * (prevZ[2] % 26 + -3 != input[3]);
    prevZ[4] = prevZ[3] * (25 * (prevZ[3] % 26 + 10 != input[4]) + 1) + (input[4] + 14) * (prevZ[3] % 26 + 10 != input[4]);
    prevZ[5] = prevZ[4] / 26 * (25 * (prevZ[4] % 26 + -9 != input[5]) + 1) + (input[5] + 2) * (prevZ[4] % 26 + -9 != input[5]);
    prevZ[6] = prevZ[5] * (25 * (prevZ[5] % 26 + 10 != input[6]) + 1) + (input[6] + 15) * (prevZ[5] % 26 + 10 != input[6]);
    prevZ[7] = prevZ[6] / 26 * (25 * (prevZ[6] % 26 + -7 != input[7]) + 1) + (input[7] + 1) * (prevZ[6] % 26 + -7 != input[7]);
    prevZ[8] = prevZ[7] / 26 * (25 * (prevZ[7] % 26 + -11 != input[8]) + 1) + (input[8] + 15) * (prevZ[7] % 26 + -11 != input[8]);
    prevZ[9] = prevZ[8] / 26 * (25 * (prevZ[8] % 26 + -4 != input[9]) + 1) + (input[9] + 15) * (prevZ[8] % 26 + -4 != input[9]);
    prevZ[10] = prevZ[9] * (25 * (prevZ[9] % 26 + 14 != input[10]) + 1) + (input[10] + 12) * (prevZ[9] % 26 + 14 != input[10]);
    prevZ[11] = prevZ[10] * (25 * (prevZ[10] % 26 + 11 != input[11]) + 1) + (input[11] + 2) * (prevZ[10] % 26 + 11 != input[11]);
    prevZ[12] = prevZ[11] / 26 * (25 * (prevZ[11] % 26 + -8 != input[12]) + 1) + (input[12] + 13) * (prevZ[11] % 26 + -8 != input[12]);
    int z = prevZ[12] / 26 * (25 * (prevZ[12] % 26 + -10 != input[13]) + 1) + (input[13] + 13) * (prevZ[12] % 26 + -10 != input[13]);
    return z == 0;
}

void decr(int num[14])
{
    int pos = 13;
    while (pos > 0) {
        if (num[pos] > 0) {
            num[pos] -= 1;
            for (int p = pos + 1; p < 14; ++p) {
                num[p] = 9;
            }
            break;
        }
        pos -= 1;
    }
}

void print(int num[14])
{
    for (int i = 0; i < 14; ++i) {
        printf("%d", num[i]);
    }
    printf("\n");
}

int main()
{
    int num[14] = {9,9,9,9,9,9,9,9,9,9,9,9,9,9};
    int i = 0;
    while (1) {
        if (i % 1000000 == 0) {
            printf("checking ");
            print(num);
        }
        if (valid(num)) {
            print(num);
            break;
        }
        decr(num);
        ++i;
    }
    return 0;
}
