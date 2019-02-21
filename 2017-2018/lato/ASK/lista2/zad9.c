#include <stdio.h>
#include <limits.h>
#include <stdint.h>

void printb(int x)
{
    for (int i = 31; i >= 0; i--)
    {
        if (x & (1 << i))
            printf("1");
        else
            printf("0");
    }
    printf("\n");
}

int32_t odd_ones(uint32_t x)
{
    int32_t c;

    c = (x & 0x55555555) + ((x >> 1) & 0x55555555);
    c = (c & 0x33333333) + ((c >> 2) & 0x33333333);
    c = (c & 0x0F0F0F0F) + ((c >> 4) & 0x0F0F0F0F);
    c = (c & 0x00FF00FF) + ((c >> 8) & 0x00FF00FF);
    c = (c & 0x0000FFFF) + ((c >> 16)& 0x0000FFFF);

    printf("%d ", c); printb(c);

    return (c & 1);
}

int main()
{
    int x = 127;
    printb(x);
    printf("%d\n", odd_ones(x));

    return 0;
}