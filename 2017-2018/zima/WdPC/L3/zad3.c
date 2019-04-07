#include <stdio.h>
#include <stdlib.h>

#define DOBA 24*60

int zamienNaMinuty(int h, int m)
{
    return h * 60 + m;
}

int policzCzasOczekiwania(int przybycie, int odjazd)
{
    int czas;

    if (przybycie <= odjazd)
        czas = odjazd - przybycie;
    else
    {
        while (przybycie > DOBA)
            przybycie -= DOBA;

        czas = (DOBA - przybycie) + odjazd;
    }

    return czas;
}

void wypiszFormatGodzinaMinuty(int minuty)
{
    int h;
    int m;

    h = minuty / 60;
    m = minuty % 60;

    printf("%d %d", h, m);
}

int normalizujCzas(int start, int czas)
{
    int znormalizowanyCzas = start + czas;

    if (znormalizowanyCzas > DOBA)
        znormalizowanyCzas -= DOBA;

    return znormalizowanyCzas;
}

int main()
{
    int liczbaPrzystankow;
    scanf("%d", &liczbaPrzystankow);

    int czasPodrozy;
    int iloscOdjazdow;

    scanf("%d %d", &czasPodrozy, &iloscOdjazdow);

    int rozmiar = iloscOdjazdow;

    int* starty = malloc(sizeof(int) * rozmiar);
    int* czasy = malloc(sizeof(int) * rozmiar);
    int* oczekiwanie = malloc(sizeof(int) * rozmiar);

    int m;
    int h;

    for (int i = 0; i < rozmiar; i++)
    {
        scanf("%d %d", &h, &m);

        starty[i] = zamienNaMinuty(h, m);
        czasy[i] = czasPodrozy;
        oczekiwanie[i] = 0;
    }

    for (int i = 1; i < liczbaPrzystankow; i++)
    {
        scanf("%d %d", &czasPodrozy, &iloscOdjazdow);

        for (int e = 0; e < iloscOdjazdow; e++)
        {
            scanf("%d %d", &h, &m);

            for (int j = 0; j < rozmiar; j++)
            {
                int czas = normalizujCzas(starty[j], czasy[j]);

                if (e == 0)
                {
                    oczekiwanie[j] = policzCzasOczekiwania(czas, zamienNaMinuty(h, m));
                }
                else
                {
                    if (oczekiwanie[j] > policzCzasOczekiwania(czas, zamienNaMinuty(h, m)))
                        oczekiwanie[j] = policzCzasOczekiwania(czas, zamienNaMinuty(h, m));
                }
            }
        }

        for (int j = 0; j < rozmiar; j++)
            czasy[j] += oczekiwanie[j] + czasPodrozy;
    }

    int najkrotszyCzas = czasy[0];
    int indeksNajkrotszegoCzasu = 0;

    for (int i = 1; i < rozmiar; i++)
    {
        if (czasy[i] < najkrotszyCzas)
        {
            indeksNajkrotszegoCzasu = i;
            najkrotszyCzas = czasy[i];
        }
    }

    wypiszFormatGodzinaMinuty(starty[indeksNajkrotszegoCzasu]);
    putchar(' ');
    wypiszFormatGodzinaMinuty(czasy[indeksNajkrotszegoCzasu]);

    return 0;
}

