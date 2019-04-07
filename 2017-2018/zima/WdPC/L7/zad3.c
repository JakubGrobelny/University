#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

// # sciana
// - puste
// ! zrodlo
// @ woda (w nastepnej sekundzie zamienia sie na zrodlo)

bool zalana(int wysokosc, int szerokosc, char** plansza)
{
    for (int x = 0; x < szerokosc; x++)
    {
        for (int y = 0; y < wysokosc; y++)
        {
            if (plansza[y][x] == '-')
                return false;
        }
    }

    return true;
}

bool takie_same(int wysokosc, int szerokosc, char** p1, char** p2)
{
    for (int x = 0; x < szerokosc; x++)
    {
        for (int y = 0; y < wysokosc; y++)
        {
            if (p1[y][x] != p2[y][x])
                return false;
        }
    }

    return true;
}

void przepisz(int wysokosc, int szerokosc, char** p1, char** p2)
{
    for (int x = 0; x < szerokosc; x++)
    {
        for (int y = 0; y < wysokosc; y++)
        {
            p2[y][x] = p1[y][x];
        }
    }
}

int obetnij(int liczba, int minimum, int maksimum)
{
    if (liczba < minimum)
        return minimum;
    if (liczba > maksimum)
        return maksimum;
    return liczba;
}

bool sasiad(int x, int y, int x1, int y1)
{
    if (x1 == x && y1 == y-1)
        return true;
    if (x1 == x-1 && y1 == y)
        return true;
    if (x1 == x+1 && y1 == y)
        return true;
    if (x1 == x && y1 == y+1)
        return true;
    return false;
}

void zalej(int wysokosc, int szerokosc, char** plansza, int x, int y)
{
    for (int i = obetnij(x-1, 0, szerokosc-1); i <= obetnij(x+1, 0, szerokosc-1); i++)
    {
        for (int e = obetnij(y-1, 0, wysokosc-1); e <= obetnij(y+1, 0, wysokosc-1); e++)
        {
            if(sasiad(x, y, i, e))
            {
                if(plansza[e][i] == '-')
                    plansza[e][i] = '@';
            }
        }
    }
}

void aktualizuj(int wysokosc, int szerokosc, char** plansza)
{
    for (int x = 0; x < szerokosc; x++)
    {
        for (int y = 0; y < wysokosc; y++)
        {
            if (plansza[y][x] == '!')
            {
                zalej(wysokosc, szerokosc, plansza, x, y);
            }
        }
    }

    for (int x = 0; x < szerokosc; x++)
    {
        for (int y = 0; y < wysokosc; y++)
        {
            if (plansza[y][x] == '@')
                plansza[y][x] = '!';
        }
    }
}

// debug
void narysuj(int wysokosc, int szerokosc, char** plansza)
{
    for (int y = 0; y < wysokosc; y++)
    {
        for (int x = 0; x < szerokosc; x++)
        {
            putchar(plansza[y][x]);
        }
        putchar('\n');
    }

}
// debug

int main()
{
    int wysokosc, szerokosc;
    scanf("%d %d", &wysokosc, &szerokosc);

    char** plansza = malloc(sizeof(char*) * wysokosc);
    char** poprzednia = malloc(sizeof(char*) * wysokosc);

    for (int i = 0; i < szerokosc; i++)
    {
        plansza[i] = malloc(sizeof(char) * szerokosc);
        poprzednia[i] = malloc(sizeof(char) * szerokosc);
    }

    for (int i = 0; i < wysokosc; i++)
    {
        scanf("%s", plansza[i]);
    }

    int czas = 0;

    bool nie_da_sie = false;
    bool zalano = false;
    while (!zalano && !nie_da_sie)
    {
        czas++;

        przepisz(wysokosc, szerokosc, plansza, poprzednia);
        aktualizuj(wysokosc, szerokosc, plansza);

        if (zalana(wysokosc, szerokosc, plansza))
        {
            zalano = true;
            break;
        }

        if (takie_same(wysokosc, szerokosc, plansza, poprzednia))
        {
            nie_da_sie = true;
            break;
        }
    }

    if(nie_da_sie)
        printf("-1");
    else
        printf("%d", czas);

    return 0;
}

