#include <stdio.h>
#include <math.h>

int* pierwsze;
int rozmiar;

short int jestPierwsza(int a)
{
    for (int i = 2; i <= sqrt(a); i++)
    {
        if (a % i == 0)
            return 0;
    }

    return 1;
}

int ileSposobow(int n)
{
    int licznik = 0;

    for (int p = 2; p <= n - p; p++)
    {
        int q = n - p;

        if (jestPierwsza(p) && jestPierwsza(q))
        {
            licznik++;
        }
    }

    return licznik;
}

int main()
{
    int n;

    scanf("%i", &n);

    if (n % 2 != 0 || n <= 2)
    {
        printf("NIEPOPRAWNA LICZBA");
    }
    else
        printf("%i", ileSposobow(n));


    return 0;
}

