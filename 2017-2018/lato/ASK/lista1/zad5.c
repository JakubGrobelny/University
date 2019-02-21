#include <stdio.h>
#include <stdint.h>

int main()
{
    uint32_t a[10] = {0, 1, 1000, 3, 4, 5, 6, 7, 8, 9};
    //uint32_t b[10] = {10, 11, 12, 13, 14, 15, 16, 17, 18, 19};
    uint32_t c[20] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20};

    uint32_t b = 42;

    uint32_t s = 42;

    uint32_t i = 2;
    uint32_t j = 3;

    a[i++] -= b * (c[j*2] + 1);

    printf("a[%d] = %d\na[%d] = %d\n", i, a[i], i-1, a[i-1]);
}
