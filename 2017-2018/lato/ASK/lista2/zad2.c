#include <stdint.h>
#include <stdio.h>
#include <limits.h>

void printb(int x)
{
    printf("%d = ", x);
    for (int i = 31; i >= 0; i--)
    {
        if (x & (1 << i))
            printf("1");
        else
            printf("0");
    }
    printf("\n");
}

int main()
{   
    int32_t x = INT_MAX;
    int32_t y = INT_MIN + 1;

    printb(x);
    printb(y);

    x ^= y;
    printb(x);

    y ^= x;
    printb(y);

    x ^= y;
    printb(x);

    printf("x = %d\ny = %d\n", x, y);

    return 0;
}