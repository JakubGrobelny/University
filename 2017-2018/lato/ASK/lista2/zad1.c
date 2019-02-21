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

int main()
{
    int32_t x = -100;
    int32_t y = 100;

    printb(x);
    printb(y);

    printb(x * ~y);
    printb((uint32_t)y * (uint32_t)x);

    printb(x * ~y + (uint32_t)y * (uint32_t)x);

    printb(~x);

    if (x * ~y + (uint32_t)y * (uint32_t)x == ~x)
        printf("No siemanko");

    /*
    int32_t x = 0;
    printb(x);
    printb(-x);
    printb(x | -x);
    printb((x | -x) >> 31);
    */

    /*
    int32_t x = 0x80000000;

    for (;x < 0x7FFFFFFF; x++)
    {
        if (x * x < 0)
        {
            printf("%d\n", INT_MIN - x);
            printb(x);
            printf("%d\n", x * x);
            printb(x*x);

            break;
        }        
    }

    int32_t y = INT_MAX;

    for (;y >= INT_MIN; y--)
    {
        if (y * y < 0)
        {
            printf("%d\n", INT_MAX - y);
            printb(y);
            printf("%d\n", y * y);
            printb(y*y);

            break;
        }        
    }

    printf("%d\n", INT_MAX -46342);
    printb(INT_MAX -46342);
    printf("%d\n", (INT_MAX - 46342) * (INT_MAX - 46342));
    printb((INT_MAX - 46342) * (INT_MAX - 46342));
    */

    return 0;
}