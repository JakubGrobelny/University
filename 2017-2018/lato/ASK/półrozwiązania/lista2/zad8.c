#include <stdio.h>
#include <limits.h>
#include <stdint.h>

int sign(int x)
{
    int mask = x & (1 << 31);
    mask >>= 31;
    return (mask == 0 & (x != 0)) + mask;
}

int main()
{
    printf("%d\n", sign(0));
    return 0;
}