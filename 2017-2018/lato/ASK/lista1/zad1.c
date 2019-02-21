#include <stdio.h>
#include <stdint.h>

void printb(uint32_t n)
{
    for (uint32_t i = 0; i < 32; i++)
    {
	if (n & (1 << i))
	    printf("1");
	else
	    printf("0");
    }

    printf("\n%d\n", n);
}

int main()
{
    uint32_t i;
    uint32_t k;

    scanf("%d %d", &i, &k);

    uint32_t n;
     
    scanf("%d", &n);

    uint32_t mask = n & (1 << i);
    mask >>= i;
    mask <<= k;
    
    uint32_t mask2 = 1 << k;
    mask2 = ~mask2;

    n &= mask2;

    n |= mask;

    printb(n);

    return 0;
}
