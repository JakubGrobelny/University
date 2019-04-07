#include <stdio.h>

void narysujElipse(int a, int b)
{
    for (double y = b - 0.5; y >= -b + 0.5; y--) // sprawdzamy wspolrzedne srodkow kolejnych 'kwadratow' na ktore podzielony jest uklad wspolrzednych
    {
        for (double x = -a + 0.5; x <= a - 0.5; x++)
        {
            double rownanieElipsy = ((x*x)/(double)(a*a)) + ((y*y)/(double)(b*b)); // (x^2 / a^2) + (y^2 / b^2) <= 1

            if (rownanieElipsy <= 1.0)
                putchar('#');
            else
                putchar(' ');
        }
        putchar('\n');
    }
}

int main()
{
    int a;
    int b;

    printf("Podaj dlugosc polosi poziomej (a): ");
    scanf("%d", &a);

    printf("Podaj dlugosc polosi pionowej (b): ");
    scanf("%d", &b);

    printf("\n");
    narysujElipse(a,b);

    return 0;
}
