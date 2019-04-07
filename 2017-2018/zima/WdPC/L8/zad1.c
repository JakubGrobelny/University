#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

bool znaleziono_rozwiazanie = false;

void wypisz_tablice(int** tablica, int wysokosc, int szerokosc)
{
    for (int y = 0; y < wysokosc; y++)
    {
        for (int x = 0; x < szerokosc; x++)
        {
            printf(" %d ", tablica[y][x]);
        }
        putchar('\n');
    }
}

bool sprawdz_wstawienie(int** tablica, int wysokosc, int szerokosc)
{
    // sumuj kolumny
    for (int kolumna = 0; kolumna < szerokosc; kolumna++)
    {
        int suma_kolumny = 0;
        for (int wiersz = 0; wiersz < wysokosc; wiersz++)
        {
            suma_kolumny += tablica[wiersz][kolumna];
        }

        if (suma_kolumny)
        {
            return false;
        }
    }

    // sumuj wiersze
    for (int wiersz = 0; wiersz < wysokosc; wiersz++)
    {
        int suma_wiersza = 0;
        for (int kolumna = 0; kolumna < szerokosc; kolumna++)
        {
            suma_wiersza += tablica[wiersz][kolumna];
        }

        if (suma_wiersza)
        {
            return false;
        }
    }

    return true;
}

bool pelne(int** tablica, int wysokosc, int szerokosc, int wiersz, int kolumna)
{
    for (int i = 0; i < szerokosc; i++)
    {
        if (tablica[wiersz][i] == 0)
            return false;
    }

    for (int i = 0; i < wysokosc; i++)
    {
        if (tablica[i][kolumna] == 0)
            return false;
    }

    return true;
}

bool pelny_wiersz(int** tablica, int wysokosc, int szerokosc, int wiersz)
{
    for (int i = 0; i < szerokosc; i++)
    {
        if (tablica[wiersz][i] == 0)
            return false;
    }
    return true;
}

bool pelna_kolumna(int** tablica, int wysokosc, int szerokosc, int kolumna)
{
    for (int i = 0; i < wysokosc; i++)
    {
        if (tablica[i][kolumna] == 0)
            return false;
    }
    return true;
}

bool zle_wstawienie(int** tablica, int wysokosc, int szerokosc, int wiersz, int kolumna)
{
    int suma_wiersza = 0;
    int suma_kolumny = 0;

    if (pelna_kolumna(tablica, wysokosc, szerokosc, kolumna))
    {
        for (int y = 0; y < wysokosc; y++)
        {
            suma_kolumny += tablica[y][kolumna];
        }

        if (suma_kolumny != 0)
            return true;
    }
    if (pelny_wiersz(tablica, wysokosc, szerokosc, wiersz))
    {
        for (int x = 0; x < szerokosc; x++)
        {
            suma_wiersza += tablica[wiersz][x];
        }

        if (suma_wiersza != 0)
            return true;
    }

    return false;
}

void zamien_zera(int* wspolrzedne_zer[2], int ilosc_zer, int** tablica, int wysokosc, int szerokosc, int iterator)
{
    if(znaleziono_rozwiazanie)
        return;

    if (iterator >= ilosc_zer)
    {
        if (sprawdz_wstawienie(tablica, wysokosc, szerokosc))
        {
            znaleziono_rozwiazanie = true;
            wypisz_tablice(tablica, wysokosc, szerokosc);
            return;
        }
    }
    else
    {
        int wiersz = wspolrzedne_zer[0][iterator];
        int kolumna = wspolrzedne_zer[1][iterator];

        for (int i = -9; i <= 9; i++)
        {
            if (i)
            {
                tablica[wiersz][kolumna] = i;

                if (zle_wstawienie(tablica, wysokosc, szerokosc, wiersz, kolumna))
                {
                    tablica[wiersz][kolumna] = 0;
                    continue;
                }

                zamien_zera(wspolrzedne_zer, ilosc_zer, tablica, wysokosc, szerokosc, iterator+1);
                tablica[wiersz][kolumna] = 0;

            }
        }
    }
}

int main()
{
    int szerokosc;
    int wysokosc;

    int ilosc_zer = 0;

    scanf("%d %d", &szerokosc, &wysokosc);

    int** tablica = malloc(sizeof(int*) * wysokosc);

    for (int i = 0; i < wysokosc; i++)
        tablica[i] = malloc(sizeof(int) * szerokosc);

    for (int y = 0; y < wysokosc; y++)
    {
        for (int x = 0; x < szerokosc; x++)
        {
            scanf("%d", &tablica[y][x]);

            if (!tablica[y][x])
                ilosc_zer++;
        }
    }

    int* wspolrzedne_zer[2];
    wspolrzedne_zer[0] = malloc(sizeof(int) * ilosc_zer);
    wspolrzedne_zer[1] = malloc(sizeof(int) * ilosc_zer);

    int i = 0;
    for (int y = 0; y < wysokosc; y++)
    {
        for (int x = 0; x < szerokosc; x++)
        {
            if (!tablica[y][x])
            {
                wspolrzedne_zer[0][i] = y;
                wspolrzedne_zer[1][i] = x;

                i++;
            }
        }
    }

    zamien_zera(wspolrzedne_zer, ilosc_zer, tablica, wysokosc, szerokosc, 0);

    if (!znaleziono_rozwiazanie)
        printf("\nNie znaleziono zadnego rozwiazania.\n");

    return 0;
}

