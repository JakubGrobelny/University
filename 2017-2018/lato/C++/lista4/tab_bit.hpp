#ifndef tab_bit_hpp
#define tab_bit_hpp

#include <iostream>
#include <initializer_list>
#include <string>
#include <utility>
#include <cmath>
#include <stdexcept>

class tab_bit
{
    typedef uint64_t slowo; // komorka w tablicy
    static const int rozmiar_slowa = 8 * sizeof(slowo); // rozmiar slowa w bitach

    friend auto operator>>(std::istream& we, tab_bit& tb) -> std::istream&;
    friend auto operator<<(std::ostream& wy, const tab_bit& tb) -> std::ostream&;
    friend auto operator|(const tab_bit& tb1, const tab_bit& tb2) -> tab_bit;
    friend auto operator&(const tab_bit& tb1, const tab_bit& tb2) -> tab_bit;
    friend auto operator^(const tab_bit& tb1, const tab_bit& tb2) -> tab_bit;
    friend auto operator~(const tab_bit& tb) -> tab_bit;

    class ref // klasa pomocnicza do adresowania bitów
    {
        slowo* wsk;
        int i;

    public:
        ref(int i, slowo* s);
        auto operator=(bool wartosc) -> ref&;
        auto operator=(const ref& bit) -> ref&;
        operator bool() const;
    };

protected:

    int dl; // liczba bitów
    slowo* tab; // tablica bitów
    
public:

    explicit tab_bit(int rozm); // wyzerowana tablica bitow [0...rozm]
    explicit tab_bit(slowo tb); // tablica bitów [0...rozmiar_slowa]
    tab_bit(const std::initializer_list<int>& lista); // konstruktor z listą inicjalizacyjną
    tab_bit(const tab_bit& tb); // konstruktor kopiujący
    tab_bit(tab_bit&& tb); // konstruktor przenoszący
    auto operator=(const tab_bit& tb) -> tab_bit&; // przypisanie kopiujące
    auto operator=(tab_bit&& tb) -> tab_bit&; // przypisanie przenoszące
    //auto operator=(const std::initializer_list<int>& lista) -> tab_bit&;
    ~tab_bit(); // destruktor

public:

    auto operator[](int i) const -> bool; // indeksowanie dla stałych tablic bitowych
    auto operator[](int i) -> ref; // indeksowanie dla zwykłych tablic bitowych
    inline auto rozmiar() const -> int {return this->dl;}; // rozmiar tablicy w bitach

public:

    auto operator|=(const tab_bit& tb) -> tab_bit&;
    auto operator&=(const tab_bit& tb) -> tab_bit&;
    auto operator^=(const tab_bit& tb) -> tab_bit&;
};

auto operator|(const tab_bit& tb1, const tab_bit& tb2) -> tab_bit;
auto operator&(const tab_bit& tb1, const tab_bit& tb2) -> tab_bit;
auto operator^(const tab_bit& tb1, const tab_bit& tb2) -> tab_bit;
auto operator~(const tab_bit& tb) -> tab_bit;

auto operator>>(std::istream& we, tab_bit& tb) -> std::istream&;
auto operator<<(std::ostream& wy, const tab_bit& tb) -> std::ostream&;

#endif