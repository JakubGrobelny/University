#include <stdio.h>
#include <stdbool.h>

char mapa[100][100];

bool odwiedzone[100][100] = {false};

int k;
int szerokosc;
int wysokosc;

int koniecX;
int koniecY;

bool sprawdz(int x, int y, int kroki, int kierunek)
{
    if (x == koniecX && y == koniecY && kroki <= k)
        return true;

    if (mapa[y][x] == '#')
        return false;

    if (kroki > k)
        return false;

    if (odwiedzone[y][x])
        return false;

    odwiedzone[y][x] = true;

    if (x != 0) // lewo 1
    {
        if (sprawdz(x - 1, y, kierunek == 1 ? kroki : kroki + 1, 1))
        {
            return true;
        }
    }
    if (x < szerokosc - 1) // prawo 2
    {
        if (sprawdz(x + 1, y, kierunek == 2 ? kroki : kroki + 1, 2))
        {
            return true;
        }
    }
    if (y != 0) // gora 3
    {
        if (sprawdz(x, y - 1, kierunek == 3 ? kroki : kroki + 1, 3))
        {
            return true;
        }
    }
    if (y < wysokosc - 1) // dol 4
    {
        if (sprawdz(x, y + 1, kierunek == 4 ? kroki : kroki + 1, 4))
        {
            return true;
        }
    }

    odwiedzone[y][x] = false;

    return false;
}

int main()
{
    scanf("%d%d%d", &szerokosc, &wysokosc, &k);

    int startX;
    int startY;

    for (int i = 0; i < wysokosc; i++)
    {
        for (int e = 0; e < szerokosc; e++)
        {
            scanf(" %c", &mapa[i][e]);

            if (mapa[i][e] == 'B')
            {
                koniecX = e;
                koniecY = i;
            }
            else if (mapa[i][e] == 'A')
            {
                startX = e;
                startY = i;
            }
        }
    }

    if (sprawdz(startX, startY, 0, 0))
        printf("TAK\n");
    else
        printf("NIE\n");

    return 0;
}
