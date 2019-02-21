#include "tab_bit.hpp"

auto operator>>(std::istream& we, tab_bit& tb) -> std::istream&
{
    std::string wejscie;
    we >> wejscie;

    delete[] tb.tab;

    tb.dl = wejscie.size();
    int ilosc_slow = std::ceil((double)wejscie.size() / (double)tab_bit::rozmiar_slowa);
    tb.tab = new tab_bit::slowo[ilosc_slow];

    int i = 0;
    for (auto& bit : wejscie)
    {
        if (bit != '0' and bit != '1')
            throw std::invalid_argument("Podany napis zawiera symbole inne niz 0 i 1");

        bool wartosc = (bool)(bit - '0');
        tb[i++] = wartosc;
    }

    return we;
}

auto operator<<(std::ostream& wy, const tab_bit& tb) -> std::ostream&
{
    for (int index = 0; index < tb.dl; index++)
        wy << tb[index];

    return wy;
}

auto tab_bit::ref::operator=(bool wartosc) -> tab_bit::ref&
{
    tab_bit::slowo maska = uint64_t(1) << i;
    *(this->wsk) &= ~maska;
    *(this->wsk) |= (uint64_t)wartosc << i;

    return *this;
}

tab_bit::ref::operator bool() const
{
    return (bool)(*(this->wsk) & (uint64_t(1) << this->i));
}

auto tab_bit::ref::operator=(const ref& bit) -> tab_bit::ref&
{
    bool wartosc = (bool)bit;
    *this = wartosc;

    return *this;
}

tab_bit::ref::ref(int i, slowo* s)
{
    this->wsk = s;
    this->i = i;
}

auto tab_bit::operator[](int i) const -> bool
{
    if (i >= this->dl || i < 0)
        throw std::invalid_argument("Podany indeks bitu jest wiekszy niz dlugosc tablicy!");

    uint64_t indeks_slowa = i / rozmiar_slowa;
    uint64_t indeks_bitu = i % rozmiar_slowa;

    return (bool)ref(indeks_bitu, &(this->tab[indeks_slowa]));//(this->tab[indeks_slowa] & (uint64_t(1) << indeks_bitu));
}

auto tab_bit::operator[](int i) -> ref
{
    if (i >= this->dl || i < 0)
        throw std::invalid_argument("Podany indeks bitu jest wiekszy niz dlugosc tablicy!");

    int indeks_slowa = i / rozmiar_slowa;
    int indeks_bitu = i % rozmiar_slowa;

    return ref(indeks_bitu, &(this->tab[indeks_slowa]));
}

tab_bit::tab_bit(int rozm)
{
    if (rozm <= 0)
        throw std::invalid_argument("Nie mozna utworzyc tablicy o ujemnej pojemnosci!");

    this->dl = rozm;
    int ilosc_slow = std::ceil((double)this->dl / (double)rozmiar_slowa);

    this->tab = new slowo[ilosc_slow];

    for (int i = 0; i < ilosc_slow; i++)
        this->tab[i] = 0;
}

tab_bit::tab_bit(slowo tb)
{
    this->dl = rozmiar_slowa;
    this->tab = new slowo[1];
    this->tab[0] = tb;
}

tab_bit::tab_bit(const std::initializer_list<int>& lista) : tab_bit((int)lista.size())
{
    if (!lista.size())
        throw std::invalid_argument("Nie mozna utworzyc tablicy z pustej listy inicjalizacyjnej!");

    int i = 0;
    for (int bit : lista)
    {
        if (bit == 0 or bit == 1)
            (*this)[i++] = (bool)bit;
        else
            throw std::invalid_argument("Lista inicjalizacyjna zawiera niedozwolone wartości!");
    }
}

tab_bit::tab_bit(const tab_bit& tb)
{
    this->dl = tb.dl;
    this->tab = new slowo[(int)(sizeof(tb.tab) / 8)];

    for (int i = 0; i < (int)(sizeof(tb.tab) / 8); i++)
        this->tab[i] = tb.tab[i];
}

tab_bit::tab_bit(tab_bit&& tb)
{
    this->dl = tb.dl;
    this-> tab = tb.tab;

    tb.dl = 0;
    tb.tab = nullptr;
}

auto tab_bit::operator=(const tab_bit& tb) -> tab_bit&
{
    if (this != &tb)
    {
        delete[] this->tab;

        this->dl = tb.dl;
        int ilosc_slow = std::ceil((double)tb.dl / (double)rozmiar_slowa);
        this->tab = new slowo[ilosc_slow];

        for (int i = 0; i < ilosc_slow; i++)
            this->tab[i] = tb.tab[i];
    }

    return *this;
}

auto operator|(const tab_bit& tb1, const tab_bit& tb2) -> tab_bit
{
    tab_bit wynik(tb1.dl > tb2.dl ? tb1 : tb2);
    int min = tb1.dl > tb2.dl ? tb2.dl : tb1.dl;

    for (int i = 0; i < min; i++)
        wynik[i] = tb1[i] | tb2[i];

    return wynik;
}

auto operator&(const tab_bit& tb1, const tab_bit& tb2) -> tab_bit
{
    tab_bit wynik(tb1.dl > tb2.dl ? tb1 : tb2);
    int min = tb1.dl > tb2.dl ? tb2.dl : tb1.dl;

    for (int i = 0; i < min; i++)
        wynik[i] = tb1[i] & tb2[i];

    return wynik;
}

auto operator^(const tab_bit& tb1, const tab_bit& tb2) -> tab_bit
{
    tab_bit wynik(tb1.dl > tb2.dl ? tb1 : tb2);
    int min = tb1.dl > tb2.dl ? tb2.dl : tb1.dl;

    for (int i = 0; i < min; i++)
        wynik[i] = tb1[i] ^ tb2[i];

    return wynik;
}

auto operator~(const tab_bit& tb) -> tab_bit
{
    tab_bit wynik(tb);

    int ilosc_slow = std::ceil((double)tb.dl / (double)tab_bit::rozmiar_slowa);

    for (int i = 0; i < ilosc_slow; i++)
        wynik.tab[i] = ~wynik.tab[i];

    return wynik;
}

auto tab_bit::operator=(tab_bit&& tb) -> tab_bit&
{
    if (this != &tb)
    {
        delete[] this->tab;

        this->dl = tb.dl;
        this->tab = tb.tab;
        tb.tab = nullptr;
        tb.dl = 0;
    }

    return *this;
}

auto tab_bit::operator|=(const tab_bit& tb) -> tab_bit&
{
    const tab_bit& wynik = (*this | tb);
    *this = std::move(wynik);

    return *this;
}

auto tab_bit::operator&=(const tab_bit& tb) -> tab_bit&
{
    const tab_bit& wynik = (*this & tb);
    *this = std::move(wynik);

    return *this;
}

auto tab_bit::operator^=(const tab_bit& tb) -> tab_bit&
{
    const tab_bit& wynik = (*this ^ tb);
    *this = std::move(wynik);

    return *this;
}

tab_bit::~tab_bit()
{
    delete[] this->tab;
}