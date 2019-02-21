#include <stdio.h>
#include <stdint.h>

// returns number * 2^i
float multiply_by_power_of_two(float number, int i)
{
    uint32_t x = *((uint32_t*)(&number));

    uint32_t exp_mask = 0x7F800000;
    uint32_t man_mask = 0x007FFFFF;

    uint32_t exp = x & exp_mask;
    uint32_t man = x & man_mask;
 
    uint32_t exp_one = 0x00800000;

    uint32_t sign = (x >> 31) & 1;

    if (i >= 0)
    {
        for (int e = 0; e < i; e++)
        {
            if (exp + exp_one == exp_mask)
            {
                man <<= 1;
                man &= man_mask;
            }
            else
                exp += exp_one;
        }        
    }
    else
    {
        i *= -1;

        for (int e = 0; e < i; e++)
        {
            if (!exp)
            {
                man >>= 1;
                man &= man_mask;
            }
            else
                exp -= exp_one;
        }
    }

    x = (sign | exp | man);

    return *(float*)&x;
}

int main()
{
    float f = 2.5f;
    int i = -10;
    float f2 = multiply_by_power_of_two(f, i);

    printf("%f * 2^%d = %.100f\n", f, i, f2);

    return 0;
}