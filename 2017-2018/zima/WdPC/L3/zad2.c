#include <stdio.h>
#include <ctype.h>

#define MAX_DLUGOSC_PASKA 20 // szerokosc paska

void narysujPasek(double procent)
{
    int dlugoscPaska = procent * MAX_DLUGOSC_PASKA + 0.5;

    for (int i = 0; i < dlugoscPaska; i++)
        putchar('*');
}

int main()
{
    unsigned int znaki[256]; // tablica przechowujaca ilosc znakow

    for (int i = 0; i < 256; i++)
    {
        znaki[i] = 0;
    }

    int znak;
    int iloscWszystkichZnakow = 0;

    while ((znak=getchar()) != '\n')
    {
        if (isgraph((char)(znak))) // jezeli znak jest drukowalny
        {
            znaki[znak]++;
            iloscWszystkichZnakow++;
        }
    }

    for (int i = 0; i < 256; i++)
    {
        if (znaki[i] > 0)
        {
            double procent = (double)(znaki[i]) / (double)(iloscWszystkichZnakow);

            printf("%c %d%c ",i ,(int)(procent*100 + 0.5), '%');
            narysujPasek(procent);
            putchar('\n');
        }
    }

    return 0;
}
