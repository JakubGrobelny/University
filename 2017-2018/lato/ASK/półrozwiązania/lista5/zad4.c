#include <stdint.h>
#include <stdio.h>

int puzzle4(long *a, long v, uint64_t s, uint64_t e)
{
    long result = e;
    result -= s;

    result >>= 1;
    result &= 0x7FFFFFFFFFFFFFFF;

    result += s;

int main()
{
    return 0;
}
