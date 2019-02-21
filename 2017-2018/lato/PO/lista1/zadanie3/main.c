// Programowanie Obiektowe
// Jakub Grobelny 300481
// Zadanie 3 lista 1

// Program tworzący tablicę indeksowaną liczbami
// całkowitymi, którą można powiększać dodając
// do niej elementy.

// Język: C
// Kompilator: gcc

#include "array.h"
#include <stdbool.h>

void print_element(type element)
{
    printf("%d", element);
}

int main()
{
    Array test_array = create_array(0);

    bool exit = false;

    while (!exit)
    {
        printf("What do you want to do?\n1) Add an element\n2) Exit\n");
        printf("Current contents of the array:\n");
        print_array(&test_array, print_element);

        char c = getchar();

        int index;
        int value;

        switch (c)
        {
            case '1':
                printf("Enter the index and the value: ");
                scanf("%d %d", &index, &value);
                add_element(&test_array, index, value);
                getchar();
                break;
            case '2':
                exit = true;
                break;
            default:
                printf("There's no such option!\n");
                break;
        }

        system("clear");
    }

    destroy_array(&test_array);
    return 0;
}
