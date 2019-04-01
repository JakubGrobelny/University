#include <stdint.h>
#include <stdio.h>
#include <limits.h>

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

/* Oblicz x * 3 / 4 zaokrąglając w dół. */
int32_t threefourths(int32_t x)
{
    uint64_t temp = x;
    temp <<= 1; // mnozenie przez 2
    temp += x;
    temp >>= 2; // podzielic przez 4
    return temp;
}

int32_t threefourths_nocasting(int32_t x)
{
    int32_t overflow;
    int32_t temp_x = (x >> 1) + (x >> 2); // 2x + x shifted to the right by two

    overflow = 0xC0000000 & temp_x; // saving the last two bits that could've overflown

    int32_t temp = x;
    temp <<= 1; // multiplying by 2
    temp += x; // adding one more x 
    temp >>= 2; // dividing by 4

    temp ^= overflow;

    return temp;    
}

int main()
{
    int32_t x = INT_MIN;
    printf("threefourths(%d) = %d\n", x, threefourths_nocasting(x));

    x = INT_MAX;
    printf("threefourths(%d) = %d\n", x, threefourths_nocasting(x));

    x = 24;
    printf("threefourths(%d) = %d\n", x, threefourths_nocasting(x));

    x = -36;
    printf("threefourths(%d) = %d\n", x, threefourths_nocasting(x));

    return 0;
}