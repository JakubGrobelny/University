#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

struct Array2D
{
    unsigned int width;
    unsigned int height;

    int *t;
};

struct Array2D create_array()
{
    struct Array2D array;

    array.width = 0;
    array.height = 0;
    array.t = NULL;

    return array;
}

int get_cell(const struct Array2D* array, unsigned int x, unsigned int y)
{
    if (x < 0 || x > array->width || y < 0 || y > array->height)
    {
        printf("\nERROR! There is no (%d, %d) cell in the array! <get_cell>", array->width, array->height);
        return 0;
    }
    return array->t[y * array->width + x];
}

void set_cell(struct Array2D* array, unsigned int x, unsigned int y, int val)
{
    if (x < 0 || x > array->width || y < 0 || y > array->height)
        return;

    array->t[y * array->width + x] = val;
}

void move_right(int *t, int size, int start)
{
    int previous = 0;

    for (int i = start; i < size; i++)
    {
        int buffer;
        buffer = t[i];
        t[i] = previous;
        previous = buffer;
    }
}

void move_left(int *t, int size, int start)
{
    for (int i = start; i < size-1; i++)
        t[i] = t[i+1];

    t[size-1] = 0;
}

void fill_with_zeros(int *t, int size, int start)
{
    int i = start + 1;
    for (i; i < size; i++)
        t[i] = 0;
}

void resize(struct Array2D* array, unsigned int width, unsigned int height)
{
    if (width == 0 || height == 0)
    {
        array->t = NULL;
        array->width = 0;
        array->height = 0;

        return;
    }
    else if (array->width == width && array->height == height)
        return;

    unsigned int size = width * height;

    if (width > array->width)
    {
        array->t = realloc(array->t, size*sizeof(int));
        int old_size = array->width * array->height;

        if (old_size < size)
            for (int i = old_size; i < size; i++)
                array->t[i] = 0;

        int x;
        int y;

        for (int i = 0; i < width * height; i++)
        {
            y = i / width;
            x = i % width;

            if (y > array->height)
                set_cell(array, x, y, 0);
            else if (x >= array->width)
                move_right(array->t, width*height, i);
        }
    }
    else
    {
        int width_difference = array->width - width;
        int old_size = array->width * array->height;

        int i;

        for (i = width; i < old_size; i += width)
        {
            for (int e = 0; e < width_difference; e++)
            {
                move_left(array->t, old_size, i);
            }
        }

        array->t = realloc(array->t, size*sizeof(int));
        fill_with_zeros(array->t, width*height, i);
    }

    array->width = width;
    array->height = height;
}

void print_array(struct Array2D* array)
{
    if (array->width == 0 || array->height == 0)
    {
        printf("This array is empty!\n");
        return;
    }

    int longest_length = 0;

    for (int y = 0; y < array->height; y++)
    {
        for (int x = 0; x < array->width; x++)
        {
            printf("%d ", get_cell(array, x, y));
        }
        putchar('\n');
    }
}

int main()
{
    // PRZYKŁADOWA MACIERZ 4x4 I JEJ PRZEKSZTAŁCENIA

    struct Array2D array = create_array(); // nowe macierze tworzone są z rozmiarem 0x0
    resize(&array, 4, 4); // ustawianie rozmiaru na 4x4

    // wstawianie przykładowych danych
    for (unsigned int i = 0; i < array.width; i++)
    {
        for (unsigned int e = 0; e < array.height; e++)
        {
            set_cell(&array, i, e, i);
        }
    }

    // wypisanie wypełnionej macierzy (funkcja print_array() używa funkcji get_cell())
    print_array(&array);
    printf("\n");

    printf("powiększanie do 10x10 i wypisywanie\n");
    resize(&array, 10, 10);
    print_array(&array);
    printf("\n");

    printf("zmniejszanie do 3x9 i wypisywanie\n");
    resize(&array, 3, 9);
    print_array(&array);
    printf("\n");

    printf("zmniejszanie do 2x5\n");
    resize(&array, 2, 5);
    print_array(&array);
    printf("\n");

    printf("zmniejszanie do 3x2 i wypisywanie\n");
    resize(&array, 3, 2);
    print_array(&array);
    printf("\n");

    printf("zmniejszanie do 1x1 i wypisywanie\n");
    resize(&array, 1, 1);
    print_array(&array);
    printf("\n");

    printf("powiekszanie do 5x5 i wypisywanie\n");
    resize(&array, 5, 5);
    print_array(&array);
    printf("\n");

    printf("zmniejszanie do 1x1 i wypisywanie\n");
    resize(&array, 1, 1);
    print_array(&array);
    printf("\n");

    printf("powiekszanie do 3x2 i wypisywanie\n");
    resize(&array, 3, 2);
    print_array(&array);
    printf("\n");

    printf("wstawianie kolejnych przykładowych danych\n");
    for (int x = 0; x < array.width; x++)
    {
        for (int y = 0; y < array.height; y++)
            set_cell(&array, x, y, y*(-x + 2));
    }
    printf("macierz 3x2 wypelniona nowymi danymi\n");
    print_array(&array);
    printf("\n");

    printf("zmiana rozmiaru na 2x3 i wypisywanie\n");
    resize(&array, 2, 3);
    print_array(&array);
    printf("\n");

    printf("zmniejszanie do 0x0 i 'wypisywanie'\n");
    resize(&array, 0, 0);
    print_array(&array);
    printf("\n");

    free(array.t);
    return 0;
}
