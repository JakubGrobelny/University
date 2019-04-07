#include <stdio.h>

#define N 400

double Minus1DoPotegi(int n)
{
    if (n % 2 == 0)
        return 1;
    else
        return -1;
}

double Kwadrat(int liczba)
{
    return liczba * liczba;
}

double Catalan()
{
    double K = 0;


    for (int i=0; i < N; i++)
    {
        K += Minus1DoPotegi(i)/Kwadrat(2 * i + 1);
    }

    return K;
}

int main()
{
    printf("%.9f", Catalan());


    return 0;
}

