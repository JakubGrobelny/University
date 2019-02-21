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

int greater_than(int x, int y)
{
    
}

int main()
{
    printf("%d\n", greater_than(6, 3));

    return 0;
}