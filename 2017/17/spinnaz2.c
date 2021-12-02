// soooooo lame

#include <stdio.h>
#include <stdlib.h>

#define ASININE_ITER_COUNT 50000000

typedef struct spinptr {
    int right;
} spinptr;

int spinnaz(int input)
{
    spinptr *buf = malloc(ASININE_ITER_COUNT * sizeof(spinptr));
    if (!buf)
        return -1;

    buf[0].right = 0;

    int ptr = 0;
    for (int i = 1; i <= ASININE_ITER_COUNT; ++i) {
        for (int j = 0; j < input; ++j) {
            ptr = buf[ptr].right;
        }
        buf[i].right = buf[ptr].right;
        buf[ptr].right = i;
        ptr = i;
    }

    ptr = buf[0].right;
    free(buf);
    return ptr;
}

int main(int argc, char* argv[])
{
    if (argc < 2) {
        fprintf(stderr, "Usage: %s input\n", argc ? argv[0] : "spinnaz2");
        return EXIT_FAILURE;
    }

    int input = atoi(argv[1]);
    if (input <= 0)
        return EXIT_FAILURE;

    printf("%d\n", spinnaz(input));
}
