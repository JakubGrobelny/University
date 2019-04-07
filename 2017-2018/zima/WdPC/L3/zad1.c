#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define MAX 10001

char tablica[MAX];

int main()
{
    int znak;

    int licznikZamykajacych = 0;

    int i = 0;
    bool poprawne = true;

    while((znak=getchar()) != '\n')
    {

        if (znak == '{' || znak == '(' || znak == '[' )
        {
            tablica[i] = znak;
            i++;
        }

        if (znak == ')')
        {
            i--;
            if (tablica[i] != '(')
            {
                poprawne = false;
                break;
            }
        }
        else if (znak == ']')
        {
            i--;
            if (tablica[i] != '[')
            {
                poprawne = false;
                break;
            }
        }
        else if (znak == '}')
        {
            i--;
            if (tablica[i] != '{')
            {
                poprawne = false;
                break;
            }
        }
    }

    if (poprawne && i == 0)
        printf("Nawiasy sa poprawne");
    else
        printf("Nawiasy nie sa zamkniete!");


    return 0;
}
