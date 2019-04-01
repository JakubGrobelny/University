#include <stdio.h>
#include <stdint.h>

uint32_t ror(uint32_t a)
{
    return (a >> 1) | (a << 31);
}

main()
{
    return 0;
}
