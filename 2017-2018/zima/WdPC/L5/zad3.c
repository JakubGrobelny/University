#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

bool** alokuj_figure(int wysokosc, int szerokosc)
{
    bool** figura;
    figura = malloc(sizeof(bool*)*wysokosc);

    for (int i = 0; i < wysokosc; i++)
        figura[i] = malloc(sizeof(bool)*szerokosc);

    return figura;
}

int przytnij(int n, int maks)
{
    if (n < 0)
        return 0;
    else if (n >= maks)
        return maks-1;
    else
        return n;
}

void usun_linie(bool** plansza, int linia, int wysokosc, int szerokosc)
{
    for (int y = linia; y > 0; y--)
    {
        plansza[y] = plansza[y-1];
    }

    for (int x = 0; x < szerokosc; x++)
        plansza[0][x] = false;
}

void symuluj(int wysokosc, int szerokosc, bool** plansza, char typFigury, int pozycjaFigury)
{
    bool **figura;
    int wysokoscFigury;
    int szerokoscFigury;

    switch (typFigury)
    {
        case 'K':
            wysokoscFigury = 2;
            szerokoscFigury = 2;
            figura = alokuj_figure(wysokoscFigury, szerokoscFigury);
            figura[0][0] = true; figura[0][1] = true;
            figura[1][0] = true; figura[1][1] = true;
            break;
        case 'T':
            wysokoscFigury = 2;
            szerokoscFigury = 3;
            figura = alokuj_figure(wysokoscFigury, szerokoscFigury);
            figura[0][0] = true; figura[0][1] = true; figura[0][2] = true;
            figura[1][0] = false; figura[1][1] = true; figura[1][2] = false;
            break;
        case 'I':
            wysokoscFigury = 4;
            szerokoscFigury = 1;
            figura = alokuj_figure(wysokoscFigury, szerokoscFigury);
            figura[0][0] = true;
            figura[1][0] = true;
            figura[2][0] = true;
            figura[3][0] = true;
            break;
        case 'F':
            wysokoscFigury = 3;
            szerokoscFigury = 2;
            figura = alokuj_figure(wysokoscFigury, szerokoscFigury);
            figura[0][0] = true; figura[0][1] = true;
            figura[1][0] = true; figura[1][1] = false;
            figura[2][0] = true; figura[2][1] = false;
            break;
        case 'W':
            wysokoscFigury = 3;
            szerokoscFigury = 5;
            figura = alokuj_figure(wysokoscFigury, szerokoscFigury);
            for (int i = 0; i < wysokoscFigury; i++)
                for (int e = 0; e < szerokoscFigury; e++)
                    figura[i][e] = true;
            figura[0][1] = false;
            figura[0][3] = false;
            figura[1][1] = false;
            figura[1][3] = false;
            break;
        case '*':
            wysokoscFigury = 1;
            szerokoscFigury = 1;
            figura = alokuj_figure(wysokoscFigury, szerokoscFigury);
            figura[0][0] = true;
            break;
        default:
            return;
    }

    int x0 = pozycjaFigury;
    int y0 = 0;
    int x1 = x0 + szerokoscFigury-1;
    int y1 = y0 + wysokoscFigury-1;

    bool kolizja = false;

    while (!kolizja && y0+wysokoscFigury-1 < wysokosc)
    {
        for (int x = 0; x < szerokoscFigury; x++)
        {
            for (int y = 0; y < wysokoscFigury; y++)
            {
                int poz_x = x0 + x;
                int poz_y = y0 + y;

                if (plansza[poz_y][poz_x] && figura[y][x])
                {
                    kolizja = true;
                    break;
                }
            }

            if (kolizja)
                break;
        }

        if (kolizja)
            break;

        y0++;
        y1++;
    }

    y0--;
    y1--;

    if(typFigury != '*')
    {
        for (int x = x0; x <= x1; x++)
        {
            for (int y = y0; y <= y1; y++)
                if (figura[y - y0][x - x0])
                    plansza[y][x]= true;
        }
    }
    else // CZY MA WYBUCHAC JEZELI WYPELNIA LINIE?
    {
        for (int x = przytnij(x0 - 1, szerokosc); x <= przytnij(x0 + 1, szerokosc); x++)
        {
            for (int y = przytnij(y0 - 1, wysokosc); y <= przytnij(y0 + 1, wysokosc); y++)
                plansza[y][x] = false;
        }

        /*
        bool pelna = true;
        for (int x = 0; x < szerokosc; x++)
        {
            if (!plansza[y0][x] && x != x0)
                pelna = false;
        }

        if (!pelna)
        {
            for (int x = przytnij(x0 - 1, szerokosc); x <= przytnij(x0 + 1, szerokosc); x++)
            {
                for (int y = przytnij(y0 - 1, wysokosc); y <= przytnij(y0 + 1, wysokosc); y++)
                    plansza[y][x] = false;
            }
        }

        if (pelna)
            plansza[y0][x0] = true;*/
    }

    // usuwanie pelnych wierszy

    for (int y = wysokosc - 1; y >= 0; y--)
    {
        bool pelna = true;
        for (int x = 0; x < szerokosc; x++)
        {
            if (!plansza[y][x])
                pelna = false;
        }

        if (pelna)
            usun_linie(plansza, y, wysokosc, szerokosc);
    }


    free(figura);
}

void rysuj_plansze(int wysokosc, int szerokosc, bool** plansza)
{
    for (int i = 0; i < wysokosc; i++)
    {
        for (int e = 0; e < szerokosc; e++)
        {
            if (plansza[i][e])
                printf("X");
            else
                printf(".");
        }
        putchar('\n');
    }
}

int main()
{
    int wysokosc;
    int szerokosc;
    int iloscFigur;

    scanf("%d %d %d",&szerokosc, &wysokosc, &iloscFigur);
    bool** plansza;
    plansza = malloc(sizeof(bool*)*wysokosc);

    for (int i = 0; i < wysokosc; i++)
    {
        plansza[i] = malloc(sizeof(bool) * szerokosc);

        for (int e = 0; e < szerokosc; e++)
            plansza[i][e] = false;
    }

    for (int k = 0; k < iloscFigur; k++)
    {
        char typFigury;
        int pozycjaFigury;
        scanf(" %c %d", &typFigury, &pozycjaFigury);
        symuluj(wysokosc, szerokosc, plansza, typFigury, pozycjaFigury);

    }

    rysuj_plansze(wysokosc, szerokosc, plansza);
    free(plansza);

    return 0;
}

