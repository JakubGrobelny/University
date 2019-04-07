#include <stdio.h>

int zakoduj(int miesiac, int dzien, int godzina, int minuty, int sekundy)
{
    int reprezentacja = 0;

    reprezentacja = miesiac;

    reprezentacja <<= 5;
    reprezentacja += dzien;

    reprezentacja <<= 5;
    reprezentacja += godzina;

    reprezentacja <<= 6;
    reprezentacja += minuty;

    reprezentacja <<= 6;
    reprezentacja += sekundy;

    return reprezentacja;
}

void wyznacz_czesc_wspolna(int* poczatek, int* koniec, int p_A, int p_B, int p_C, int p_D, int k_A, int k_B, int k_C, int k_D)
{
    *poczatek = p_A;
    *koniec = k_A;

    if (*poczatek < p_B) *poczatek = p_B;
    if (*poczatek < p_C) *poczatek = p_C;
    if (*poczatek < p_D) *poczatek = p_D;

    if (*koniec > k_B) *koniec = k_B;
    if (*koniec > k_C) *koniec = k_C;
    if (*koniec > k_D) *koniec = k_D;
}

int odkoduj_miesiac(int reprezentacja)
{
    reprezentacja >>= 22;
    return reprezentacja;
}

int odkoduj_dzien(int reprezentacja)
{
    reprezentacja &= (31<<17);
    reprezentacja >>= 17;
    return reprezentacja;
}

int odkoduj_godzine(int reprezentacja)
{
    reprezentacja &= (31<<12);
    reprezentacja >>= 12;
    return reprezentacja;
}

int odkoduj_minuty(int reprezentacja)
{
    reprezentacja &= (63<<6);
    reprezentacja >>= 6;
    return reprezentacja;
}

int odkoduj_sekundy(int reprezentacja)
{
    reprezentacja &= (63);
    return reprezentacja;
}

void wypisz_czas(int czas)
{
    printf("%d: %d %d %d %d %d\n", czas, odkoduj_miesiac(czas), odkoduj_dzien(czas), odkoduj_godzine(czas), odkoduj_minuty(czas), odkoduj_sekundy(czas));
}

int main()
{
    int graczePoczatek[512];
    int graczeKoniec[512];

    int iloscGraczy;

    scanf("%d", &iloscGraczy);
    if (iloscGraczy < 4)
    {
        printf("NOT FOUND!");
        return 0;
    }

    for (int i = 0; i < iloscGraczy; i++)
    {
        int miesiac, dzien, godzina, minuta, sekundy;
        scanf("%d %d %d %d %d", &miesiac, &dzien, &godzina, &minuta, &sekundy);
        graczePoczatek[i] = zakoduj(miesiac, dzien, godzina, minuta, sekundy);

        scanf("%d %d %d %d %d", &miesiac, &dzien, &godzina, &minuta, &sekundy);
        graczeKoniec[i] = zakoduj(miesiac, dzien, godzina, minuta, sekundy);
    }

    int najlepszyPrzedzial[2];
    int dlugoscNajlepszegoPrzedzialu = -1;

    int graczA, graczB, graczC, graczD;

    for (int pierwszy = 0; pierwszy < iloscGraczy - 3; pierwszy++)
    {
        if (graczeKoniec[pierwszy] - graczePoczatek[pierwszy] < dlugoscNajlepszegoPrzedzialu)
            continue;

        for (int drugi = pierwszy + 1; drugi < iloscGraczy - 2; drugi++)
        {
            if (graczeKoniec[drugi] - graczePoczatek[drugi] < dlugoscNajlepszegoPrzedzialu)
                continue;

            for (int trzeci = drugi + 1; trzeci < iloscGraczy - 1; trzeci++)
            {
                if (graczeKoniec[trzeci] - graczePoczatek[trzeci] < dlugoscNajlepszegoPrzedzialu)
                    continue;

                for (int czwarty = trzeci + 1; czwarty < iloscGraczy; czwarty++)
                {
                    if (graczeKoniec[czwarty] - graczePoczatek[czwarty] < dlugoscNajlepszegoPrzedzialu)
                        continue;

                    int poczatekCzesciWspolnej, koniecCzesciWspolnej;

                    wyznacz_czesc_wspolna(&poczatekCzesciWspolnej, &koniecCzesciWspolnej,
                    graczePoczatek[pierwszy], graczePoczatek[drugi], graczePoczatek[trzeci], graczePoczatek[czwarty],
                    graczeKoniec[pierwszy], graczeKoniec[drugi], graczeKoniec[trzeci], graczeKoniec[czwarty]);

                    int dlugosc = koniecCzesciWspolnej - poczatekCzesciWspolnej;

                    if (dlugosc > dlugoscNajlepszegoPrzedzialu)
                    {
                        graczA = czwarty;
                        graczB = trzeci;
                        graczC = drugi;
                        graczD = pierwszy;

                        dlugoscNajlepszegoPrzedzialu = dlugosc;
                        najlepszyPrzedzial[0] = poczatekCzesciWspolnej;
                        najlepszyPrzedzial[1] = koniecCzesciWspolnej;
                    }
                }
            }
        }
    }

    if (dlugoscNajlepszegoPrzedzialu <= 0)
        printf("NOT FOUND!");
    else
    {
        printf("%d %d %d %d %d\n", dlugoscNajlepszegoPrzedzialu, graczA, graczB, graczC, graczD);
        wypisz_czas(najlepszyPrzedzial[0]);
        wypisz_czas(najlepszyPrzedzial[1]);
    }

    return 0;
}

