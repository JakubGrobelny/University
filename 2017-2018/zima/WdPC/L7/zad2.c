#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <limits.h>

char operatory[4] = {'+', '-', '*', '/'};
bool znaleziono_rozwiazanie = false;

void wypisz_wynik(char* sekwencja_operatorow, int* sekwencja_liczb, int rozmiar, int cel)
{
    for (int i = 0; i < rozmiar; i++)
    {
        if (sekwencja_liczb[i] >= 0)
            printf("%d %c ", sekwencja_liczb[i], sekwencja_operatorow[i]);
        else
            printf("(%d) %c ", sekwencja_liczb[i], sekwencja_operatorow[i]);
    }
    printf("%d \n", cel);
}

bool mozna_mnozyc(int a, int b)
{
    if (a < 0)
        a *= -1;
    if (b < 0)
        b *= -1;
    if (a == 0 || b == 0)
        return true;
    if (INT_MAX / a < b)
        return false;
    if (INT_MAX / b < a)
        return false;
    return true;
}

bool mozna_dodawac(int a, int b)
{
    if (a < 0 && b < 0)
    {
        if (a < INT_MIN - b)
            return false;
        else if (b < INT_MIN - a)
            return false;
    }
    else if (a >= 0 && b >= 0)
    {
        if (INT_MAX - a < b)
            return false;
        if (INT_MAX - b < a)
            return false;
    }
    return true;
}

bool mozna_odejmowac(int a, int b)
{
    if (a < 0 && b >= 0)
    {
        if (INT_MIN + b > a)
            return false;
    }
    else if (a < 0 && b < 0)
    {
        return true;
    }
    else if (a >= 0 && b < 0)
    {
        if (-b > INT_MAX - a)
            return false; // if -b > INT_MAX - a
    }
    else if (a >= 0 && b >= 0)
    {
        return true;
    }
    return true;
}

bool mozna_dzielic(int a, int b)
{
    if (a == INT_MIN && b == -1)
        return false;
    return true;
}

void usuwaj_mnozenie_i_dzielenie(char* sekwencja_operatorow, int* sekwencja_liczb, int indeks, char wczesniejszy)
{
    if (sekwencja_operatorow[indeks] == '*')
    {
        if (mozna_mnozyc(sekwencja_operatorow[indeks], sekwencja_operatorow[indeks+1]))
        {
            sekwencja_operatorow[indeks] = wczesniejszy;
            sekwencja_liczb[indeks+1] *= sekwencja_liczb[indeks];
            sekwencja_liczb[indeks] = 0;
            usuwaj_mnozenie_i_dzielenie(sekwencja_operatorow, sekwencja_liczb, indeks+1, wczesniejszy);
        }
        else
            sekwencja_operatorow[indeks] = '!'; // ! oznacza blad, nalezy odrzucic wynik
    }
    else if (sekwencja_operatorow[indeks] == '/')
    {
        if (mozna_dzielic(sekwencja_operatorow[indeks], sekwencja_operatorow[indeks+1]))
        {
            sekwencja_operatorow[indeks] = wczesniejszy;
            sekwencja_liczb[indeks+1] = sekwencja_liczb[indeks] / sekwencja_liczb[indeks+1];
            sekwencja_liczb[indeks] = 0;
            usuwaj_mnozenie_i_dzielenie(sekwencja_operatorow, sekwencja_liczb, indeks+1, wczesniejszy);
        }
        else
            sekwencja_operatorow[indeks] = '!';
    }
    else
        return;
}

bool sprawdz_sekwencje(char* sekwencja_operatorow, int* sekwencja_liczb, int rozmiar, int cel)
{
    // tworzenie kopii tablic na ktorych beda wykonywane operacje
    char* sekwencja_operatorow_temp = malloc(sizeof(char) * rozmiar);
    int* sekwencja_liczb_temp = malloc(sizeof(int) * rozmiar);

    for (int i = 0; i < rozmiar; i++)
    {
        sekwencja_liczb_temp[i] = sekwencja_liczb[i];
        sekwencja_operatorow_temp[i] = sekwencja_operatorow[i];
    }

    for (int i = 0; i < rozmiar-1; i++)
    {
        if (sekwencja_operatorow_temp[i] == '*' || sekwencja_operatorow_temp[i] == '/')
        {
            if (i != 0)
                usuwaj_mnozenie_i_dzielenie(sekwencja_operatorow_temp, sekwencja_liczb_temp, i, sekwencja_operatorow_temp[i-1]);
            else
                usuwaj_mnozenie_i_dzielenie(sekwencja_operatorow_temp, sekwencja_liczb_temp, i, '+');
        }
    }

    int wynik = sekwencja_liczb_temp[0];

    for (int i = 0; i < rozmiar-1; i++)
    {
        if (sekwencja_operatorow_temp[i] == '!')
            return false;

        if (sekwencja_operatorow_temp[i] == '+')
        {
            if (mozna_dodawac(wynik, sekwencja_liczb_temp[i+1]))
            {
                wynik += sekwencja_liczb_temp[i+1];
            }
            else
                return false;
        }
        else if (sekwencja_operatorow_temp[i] == '-')
        {
            if (mozna_odejmowac(wynik, sekwencja_liczb_temp[i+1]))
            {
                wynik -= sekwencja_liczb_temp[i+1];
            }
            else
                return false;
        }
    }

    //wypisz_wynik(sekwencja_operatorow, sekwencja_liczb, rozmiar, wynik);
    //wypisz_wynik(sekwencja_operatorow_temp, sekwencja_liczb_temp, rozmiar, wynik);
    //printf("\n");

    free(sekwencja_operatorow_temp);
    free(sekwencja_liczb_temp);

    if (wynik == cel)
        return true;
    else
        return false;
}

void szukaj(char* sekwencja_operatorow, int* sekwencja, int rozmiar, int dlugosc, int cel)
{
    if (znaleziono_rozwiazanie)
        return;

    if (dlugosc == rozmiar - 1)
    {
        if (sprawdz_sekwencje(sekwencja_operatorow, sekwencja, rozmiar, cel))
        {
            if (!znaleziono_rozwiazanie)
            {
                sekwencja_operatorow[dlugosc] = '=';
                wypisz_wynik(sekwencja_operatorow, sekwencja, rozmiar, cel);
            }
            znaleziono_rozwiazanie = true;
            return;
        }
        else
            return;
    }
    else
    {
        for (int i = 0; i < 4; i++)
        {
            if (i != 3 || sekwencja[dlugosc+1] != 0)
            {
                sekwencja_operatorow[dlugosc] = operatory[i];
                szukaj(sekwencja_operatorow, sekwencja, rozmiar, dlugosc+1, cel);
            }
        }
    }
}

int main()
{
    int M;
    unsigned int N;

    scanf("%d %d", &M, &N);

    int* sekwencja = malloc(sizeof(int) * N);
    char* sekwencja_operatorow = malloc(sizeof(char) * N);

    for (unsigned int i = 0; i < N; i++)
        scanf("%d", &sekwencja[i]);

    szukaj(sekwencja_operatorow, sekwencja, N, 0, M);

    if(!znaleziono_rozwiazanie)
        printf("Nie znaleziono zadnej mozliwej kombinacji operatorow.\n");

    free(sekwencja);
    free(sekwencja_operatorow);
    return 0;
}
