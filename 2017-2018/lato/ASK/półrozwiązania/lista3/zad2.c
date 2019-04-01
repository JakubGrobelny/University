#include <stdio.h>

int main()
{
    int x = 2;
    double sum = 0;

    printf("sum = ");
    for (int i = 0; i < 10; i++)
    {
        sum += (1.0 / x);
        printf("1/%d ", x);
        x <<= 1;
    }
    printf(" = %f\n", sum);

    return 0;
}