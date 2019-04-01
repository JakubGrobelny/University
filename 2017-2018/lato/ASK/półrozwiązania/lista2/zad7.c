#include <stdint.h>
#include <stdio.h>
#include <limits.h>

#define N 32

void printb(int x)
{
    for (int i = 31; i >= 0; i--)
    {
        if (x & (1 << i))
            printf("1");
        else
            printf("0");
    }
    printf(" = %d\n", x);
}

int abss(int x)
{
    // b ? x : y <=> b * x + ~b * y

    int mask = x >> 31;
    return (mask^x) - mask;
}

int main()
{
    printb(0);
    printb(~0 * -100);

    printf("%d\n", abss(-42));

    return 0;
}