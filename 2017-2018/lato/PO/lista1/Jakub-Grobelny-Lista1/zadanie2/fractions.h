// Programowanie Obiektowe
// Jakub Grobelny 300481
// Zadanie 2 lista 1

#ifndef _FRACTIONS
#define _FRACTIONS

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct Fraction
{
    int nominator;
    int denominator;

} Fraction;

static int greatest_common_divisor(int a, int b);

static void make_irreducible(Fraction* fraction);

static void fix_minus(Fraction* fraction);

Fraction* create_fraction(int nominator, int denominator);

Fraction* create_fraction_from_double(double number);

void print_fraction(Fraction* fraction);

void print_fraction_newline(Fraction* fraction);

void print_fraction_as_double(Fraction* fraction);

void print_fraction_newline_as_double(Fraction* fraction);

void destroy_fraction(Fraction* fraction);

Fraction* add_fractions(Fraction* first, Fraction* second);
Fraction* substract_fractions(Fraction* first, Fraction* second);
Fraction* multiply_fractions(Fraction* first, Fraction* second);
Fraction* divide_fractions(Fraction* first, Fraction* second);

// Functions below modify the second fraction instead of returning new object
void add_fractions_modify(Fraction* first, Fraction* second);
void substract_fractions_modify(Fraction* first, Fraction* second);
void divide_fractions_modify(Fraction* first, Fraction* second);
void multiply_fractions_modify(Fraction* first, Fraction* second);

#endif
