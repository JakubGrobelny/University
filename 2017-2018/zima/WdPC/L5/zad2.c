#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

bool sprawdzWartosc(unsigned int liczba, int i)
{
    return liczba & (1 << i);
}

unsigned int dlugoscPodzbioru(int podzbior, int n)
{
    int dlugosc = 0;

    for (int i = 0; i <= n; i++)
    {
        if (sprawdzWartosc(podzbior, i))
            dlugosc++;
    }

    return dlugosc;
}

void liczSumy(int zbior[], int n)
{
    int najwiekszyPodzbior = 0;
    int dlugoscNajwiekszegoPodzbioru = 0;

    for (int ktore = 1; ktore < (1U << n); ktore++)
    {
        int suma = 0;

        for (int i = 0; i <= n; i++)
        {
            if (sprawdzWartosc(ktore, i))
                suma += zbior[i];
        }

        if (!suma)
        {
            if (dlugoscPodzbioru(ktore, n) > dlugoscNajwiekszegoPodzbioru)
            {
                dlugoscNajwiekszegoPodzbioru = dlugoscPodzbioru(ktore, n);
                najwiekszyPodzbior = ktore;
            }
        }
    }

    for (int i = 0; i < n; i++)
    {
        if (sprawdzWartosc(najwiekszyPodzbior, i))
        {
            printf("%d ", zbior[i]);
        }
    }

    putchar('\n');      
}

int main()
{
    int n;
    scanf("%d", &n);

    int* zbior;
    zbior = malloc(sizeof(int) * n);

    for (int i = 0; i < n; i++)
        scanf("%d", &zbior[i]);

    liczSumy(zbior, n);

    free(zbior);
}
