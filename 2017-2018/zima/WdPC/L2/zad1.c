#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdbool.h>

int main()
{
    int znak;

    znak = getchar();

    znak = toupper(znak);

    putchar(znak);

    bool noweZdanie = false;
    bool licznikSpacji = 0;
    bool interpunkcyjny = 0;;

    while(znak != EOF)
    {
        znak = getchar();

        if (znak == '.' || znak == '!' || znak == '?' || znak == ';' || znak == ',')
        {
            putchar(znak);
            putchar(' ');

            if (znak == ',' || znak == ';')
                noweZdanie = 0;
            else
                noweZdanie = 1;

            licznikSpacji = 0;
            interpunkcyjny = 1;
        }
        else if (znak == ' ' )
            licznikSpacji++;
        else
        {
            if (interpunkcyjny)
            {
                licznikSpacji = 0;
                interpunkcyjny = 0;
            }

            if (licznikSpacji > 0)
            {
                putchar(' ');
                licznikSpacji = 0;
            }
            if (noweZdanie)
            {
                znak = toupper(znak);
                putchar(znak);
                noweZdanie = 0;
            }
            else
            {
                putchar(znak);
            }
        }

    }

    return 0;
}
