#include <stdio.h>
#include <stdint.h>

int main()
{
    uint64_t a;
    uint64_t b;

    scanf("%ld%ld", &a, &b);
    printf("%ld\n", (long)cmp(a, b));

    return 0;
}

