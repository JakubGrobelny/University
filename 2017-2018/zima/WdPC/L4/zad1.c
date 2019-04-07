#include <stdio.h>

int najdluzszyPodciagZer = 0;
int najdluzszyPodciagJedynek = 0;
int aktualnyPodciagZer = 0;
int aktualnyPodciagJedynek = 0;
int licznikZer = 0;
int licznikJedynek = 0;
int aktualnyPodciag = 2;

void sprawdz(int liczba)
{
    int bity = 0;

    int tablica[8];

    while(bity < 8) // kazdy znak ma miec 8 bitow
    {
        int aktualnaCyfra = liczba % 2;

        if (aktualnaCyfra)
            licznikJedynek++;
        else
            licznikZer++;

        liczba -= aktualnaCyfra;
        liczba /= 2;

        // zapisywanie odwrotnie w tablicy zeby kolejnosc sie zgadzala
        tablica[7 - bity] = aktualnaCyfra;

        bity++;
    }
    aktualnyPodciag = tablica[0];
    printf("%d", tablica[0]);

    // sprawdzanue podciagow
    for (int i = 1; i < 8; i++)
    {
        printf("%d",tablica[i]);

        if (tablica[i] != aktualnyPodciag)
        {
            if (aktualnyPodciag == 1)
            {
                if (aktualnyPodciagJedynek > najdluzszyPodciagJedynek)
                    najdluzszyPodciagJedynek = aktualnyPodciagJedynek;
                aktualnyPodciag = 0;
                aktualnyPodciagZer = 1;
                aktualnyPodciagJedynek = 1;
            }
            else if (aktualnyPodciag == 0)
            {
                if (aktualnyPodciagZer > najdluzszyPodciagZer)
                    najdluzszyPodciagZer = aktualnyPodciagZer;
                aktualnyPodciag = 1;
                aktualnyPodciagJedynek = 1;
                aktualnyPodciagZer = 1;
            }
        }
        else // jezeli podciag jest kontynuowany
        {
            if (aktualnyPodciag == 1)
            {
                aktualnyPodciagJedynek++;
            }
            else if (aktualnyPodciag == 0)
            {
                aktualnyPodciagZer++;
            }
        }
    }

}

int main()
{
    int znak;

    while ((znak = getchar()) != '\n')
        sprawdz(znak);

    printf("\n\nIlosc zer: %d\nIlosc jedynek: %d\nNajdluzszy podciag zer: %d\nNajdluzszy podciag jedynek: %d\n\n", licznikZer, licznikJedynek, najdluzszyPodciagZer, najdluzszyPodciagJedynek);

    return 0;
}

