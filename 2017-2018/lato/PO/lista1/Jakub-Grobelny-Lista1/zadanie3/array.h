// Programowanie Obiektowe
// Jakub Grobelny 300481
// Zadanie 3 lista 1

#ifndef _ARRAY
#define _ARRAY

#include <stdlib.h>
#include <stdio.h>

typedef int type;

typedef struct Array
{
    int capacity; 
    int beginning;

    type neutral_element;

    type* elements;
} Array;

Array create_array(type neutral_element);

void print_array(Array* array, void print_fun(type));

int abs(int x);

void add_element(Array* array, int index, type element);

type get_element(Array* array, int index);

void destroy_array(Array* array);

#endif
