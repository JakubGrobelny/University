#include <stdio.h>
#include <stdbool.h>

#define MAX 6002

int czytajWiersz(char wiersz[], int rozmiar)
{
    int znak;
    int i;

    for (i = 0; i < rozmiar-1 && (znak = getchar()) != EOF; ++i)
    {
        if ((wiersz[i] = znak) == '\n')
        {
            ++i; break;
        }

    }
    
    wiersz[i] = '\0';
    
    return i;
}

void kopiuj(char docelowy[],char zrodlo[])
{
    for (int i = 0; (docelowy[i] = zrodlo[i]) != '\0'; ++i);
}

bool porownajWiersze(char wiersz1[], char wiersz2[])
{
    unsigned int dl1 = 0;
    unsigned int dl2 = 0;

    while(wiersz1[dl1] != '\0')
        dl1++;

    while(wiersz2[dl2] != '\0')
        dl2++;

    if (dl1 != dl2)
        return false;

    for (unsigned int i = 0; i < dl1; i++)
        if (wiersz1[i] != wiersz2[i])
            return false;

    return true;
}

int main(void)
{
    char wiersz[MAX], poprzedniWiersz[MAX]; // aktualny wiersz, najdluzszy wiersz

    czytajWiersz(poprzedniWiersz, MAX);
    printf("%s", poprzedniWiersz);

    while (czytajWiersz(wiersz, MAX) > 0)
    {
        if (!porownajWiersze(wiersz, poprzedniWiersz))
            printf("%s", wiersz);

        kopiuj(poprzedniWiersz, wiersz);
    }
    
    return 0;
}

