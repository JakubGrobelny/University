#include <stdio.h>
#include <math.h>

int funkcja(int m, int n)
{
    for (int i = 2; i < n; i++)
    {
        int pierwsza = 1; // bool nie chce sie kompilowac

        for (int e = 2; e < i; e++)
        {
            if (i % e == 0)
                pierwsza = 0;
        }

        if (pierwsza == 1)
        {
            if (m % i != n % i)
            {
                return i;
            }
        }
    }
}

int main()
{
    // 3, 9

    float max = funkcja(3, 4) / log(4);
    int nmax = 4;
    int mmax = 3;

    for (int m = 3; m < 999; m++)
    {
        for (int n = 4; n < 1000; n++)
        {
            if (max < funkcja(m, n) / log(n) && n > m)
            {
                max = funkcja(m,n) / log(n);
                nmax = n;
                mmax = m;
            }
        }
    }

    printf("Max dla m = %i i n = %i i p = %i Wynik to %f \n", mmax, nmax, funkcja(mmax, nmax), max);

    return 0;
}
