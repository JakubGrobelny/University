// Programowanie Obiektowe
// Jakub Grobelny 300481
// Zadanie 2 lista 1

#include "fractions.h"

static int greatest_common_divisor(int a, int b)
{
    if (b == 0)
        return a;
    return greatest_common_divisor(b, a % b);
}

static void make_irreducible(Fraction* fraction)
{
    int gcd = greatest_common_divisor(fraction->denominator, fraction->nominator);

    fraction->nominator /= gcd;
    fraction->denominator /= gcd;

    fix_minus(fraction);
}

static void fix_minus(Fraction* fraction)
{
    if ((fraction->nominator < 0 && fraction->denominator < 0) || fraction->denominator < 0)
    {
        fraction->nominator *= -1;
        fraction->denominator *= -1;
    }
}

Fraction* create_fraction_from_double(double number)
{
    bool negative = false;

    if (number < 0)
    {
        negative = true;
        number *= -1;
    }
 
    int nominator;
    int denominator = 1;

    while ((int)number != number)
    {
        number *= 10;
        denominator *= 10;
    }

    nominator = number;

    Fraction* result = create_fraction(nominator, denominator);
    make_irreducible(result);

    if (negative)
        result->nominator *= -1;

    return result;
}

Fraction* create_fraction(int nominator, int denominator)
{
    Fraction* result = malloc(sizeof(Fraction));

    result->nominator = nominator;
    result->denominator = denominator;

    make_irreducible(result);

    return result;
}   

void print_fraction(Fraction* fraction)
{
    if (fraction->denominator)
        printf("%d/%d", fraction->nominator, fraction->denominator);
    else
        printf("inf");
}

void print_fraction_newline(Fraction* fraction)
{
    print_fraction(fraction);
    putchar('\n');
}

void print_fraction_as_double(Fraction* fraction)
{
    printf("%f", (double)fraction->nominator / (double)fraction->denominator);
}

void print_fraction_newline_as_double(Fraction* fraction)
{
    print_fraction_as_double(fraction);
    putchar('\n');
}

void destroy_fraction(Fraction* fraction)
{
    free(fraction);
}

Fraction* add_fractions(Fraction* first, Fraction* second)
{   
    return create_fraction(first->nominator * second->denominator + second->nominator * first->denominator, first->denominator * second->denominator);
}

Fraction* substract_fractions(Fraction* first, Fraction* second)
{
    return create_fraction(first->nominator * second->denominator - second->nominator * first->denominator, first->denominator * second->denominator);
}

Fraction* multiply_fractions(Fraction* first, Fraction* second)
{
    return create_fraction(first->nominator * second->nominator, first->denominator * second->denominator);
}

Fraction* divide_fractions(Fraction* first, Fraction* second)
{
    return create_fraction(first->nominator * second->denominator, first->denominator * second->nominator);
}

void add_fractions_modify(Fraction* first, Fraction* second)
{
    second->nominator = first->nominator * second->denominator + second->nominator * first->denominator;
    second->denominator = first->denominator * second->denominator;

    make_irreducible(second);
}

void substract_fractions_modify(Fraction* first, Fraction* second)
{
    second->nominator = first->nominator * second->denominator - second->nominator * first->denominator;
    second->denominator = first->denominator * second->denominator;

    make_irreducible(second);    
}

void divide_fractions_modify(Fraction* first, Fraction* second)
{
    int prev_nominator = second->nominator;

    second->nominator = first->nominator * second->denominator;
    second->denominator = first->denominator * prev_nominator;

    make_irreducible(second);
}

void multiply_fractions_modify(Fraction* first, Fraction* second)
{
    second->nominator = first->nominator * second->nominator;
    second->denominator = first->denominator * second->denominator;

    make_irreducible(second);
}
