#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

bool sprawdzWartosc(unsigned int liczba, int i)
{
    return liczba & (1 << i);
}

void liczSumy(int zbior[], int n)
{
    bool dotychczasoweSumy[2002];
    for (int i = 0; i <= 2001; i++)
        dotychczasoweSumy[i] = 0;

    printf("0 ");
    dotychczasoweSumy[1000] = true;

    for (int ktore = 1; ktore < (1U << n); ktore++)
    {
        int suma = 0;

        for (int i = 0; i <= n; i++)
        {
            if (sprawdzWartosc(ktore, i))
                suma += zbior[i];
        }

        if (dotychczasoweSumy[suma + 1000] == false)
        {
            printf("%d ", suma);
            dotychczasoweSumy[suma + 1000] = true;
        }
    }
}

int main()
{
    int n;
    scanf("%d", &n);

    int* zbior;
    zbior = malloc(sizeof(int) * n);

    for (int i = 0; i < n; i++)
    {
        scanf("%d", &zbior[i]);
    }

    liczSumy(zbior, n);

    free(zbior);
}
