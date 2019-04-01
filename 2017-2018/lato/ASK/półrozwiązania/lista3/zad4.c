#include <stdio.h>
#include <limits.h>
#include <stdint.h>
#include <float.h>

int main()
{
    int32_t x = INT_MAX;

    double d = 2147483650.0;

    float f = FLT_MAX;
    printf("%f -- %f\n", f, -(-f));

    printf("%f -- %f\n", 1.0 / 2, 1 / 2.0);

    printf("%f -- %f\n", d, (float)d);

    printf("%d -- %d\n", x, (int32_t)(float)x);

    if (x == (int32_t)(float)x)
        printf("Tak\n");

    double d2 = -d;

    if (d2 * d2 >= 0)
        printf("Tak2 %f\n", d2*d2);

    double d3 = DBL_MIN;
    float f2 = FLT_MAX;

    printf("%f\n", f2);

    printf("%f + %f = %f\n", f2, d3, f2 + d3);
    printf("(%.5f + %.1000f) - %.5f = %.100f != %.1000f\n", f2, d3, f2, (f2 + d3) - f2, d3);

    if ((f2 + d3) - f2 != d3)
        printf("\nNIEEE\n");

    return 0;
}