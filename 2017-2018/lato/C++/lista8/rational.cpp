#include "rational.hpp"

using namespace calculations;
using limit = std::numeric_limits<int>;

static auto check_multiplication(int a, int b) -> bool
{
    return not ((limit::max() / a < b) or (limit::min() / a > b));
}

static auto check_addition(int a, int b) -> bool
{
    const int result = a + b;
    const int mask = 1 << 31;

    // Jeżeli a i b mają różny znak, to można wykonać dodawanie, w przeciwnym
    // razie wynik dodawania musi mieć taki sam znak jak a i b.
    return ((a & mask) ^ (b & mask)) ||
          (((result & mask) == (a & mask)) && ((result & mask) == (b & mask)));
}

auto calculations::operator<<(std::ostream& out, const Rational& a) noexcept -> std::ostream&
{   
    int denominator = a.denominator;

    // Ułamek ma skończone rozwinięcie dziesiętne, jeżeli jego mianownik
    // rozkłada się wyłącznie na 2 lub 5.
    while (denominator % 2 == 0)
        denominator /= 2;
    while (denominator % 5 == 0)
        denominator /= 5;

    if (denominator == 1) // skończone rozwinięcie dziesiętne
        return out << (double)a;
    else
    {
        int dividend = a.numerator;
        int divisor = a.denominator;

        int integer = dividend / divisor;
        int remainder = dividend % divisor;
        
        out << integer;

        dividend -= integer * divisor;

        std::vector<std::pair<char, int>> states;
        unsigned int repetition_start = 0;

        while (remainder)
        {
            remainder *= 10;
            int r = remainder / divisor;
            char digit = r + '0';
            remainder = remainder - r * divisor;
            auto current = std::make_pair(digit, remainder);
            states.push_back(current);

            for (unsigned int i = 0; i < states.size() - 1; i++)
            {
                if (current == states[i])
                {
                    repetition_start = i;
                    remainder = 0;
                    states.pop_back();
                    break;
                }
            }
        }

        out << ".";
        unsigned int i = 0;

        for (auto& state : states)
        {
            if (i == repetition_start)
                out << "(";
            out << state.first;
            i++;
        }

        out << ")";
        return out;
    }
}

auto calculations::operator+(const Rational& a, const Rational& b) -> Rational
{
    Rational result = a;
    result += b;
    return result;
}

auto calculations::operator-(const Rational& a, const Rational& b) -> Rational
{
    Rational result = a;
    result -= b;
    return result;
}

auto calculations::operator*(const Rational& a, const Rational& b) -> Rational
{
    Rational result = a;
    result *= b;
    return result;
}

auto calculations::operator/(const Rational& a, const Rational& b) -> Rational
{
    Rational result = a;
    result /= b;
    return result;
}

Rational::operator int() const
{
    return numerator / denominator;
}

Rational::operator double() const
{
    return (double)numerator / denominator;
}

auto Rational::gcd(int a, int b) noexcept -> int
{
    if (b == 0)
        return a;
    return gcd(b, a % b);
}

void Rational::normalize()
{
    if (not denominator)
        throw DivisionByZero("normalize(): Denominator set to 0!");

    if (denominator < 0)
    {
        if (numerator   == limit::min() or 
            denominator == limit::min())
            throw IntegerOverflowException("normalize(): Integer overflow!");

        denominator *= -1;
        numerator *= -1;
    }
    
    int divisor = gcd(numerator, denominator);
    
    numerator /= divisor;
    denominator /= divisor;
}

Rational::Rational(int number)
{
    this->denominator = 1;
    this->numerator = number;
}

Rational::Rational(int numerator, int denominator)
{
    this->numerator = numerator;
    this->denominator = denominator;
    this->normalize();
}

auto Rational::get_numerator() const noexcept -> int
{
    return numerator;
}

auto Rational::get_denominator() const noexcept -> int
{
    return denominator;
}

auto calculations::operator-(const Rational& a) -> Rational
{
    if (a.numerator == std::numeric_limits<int>::min())
        throw IntegerOverflowException("operator-(): integer overflow!");

    return Rational(-a.numerator, a.denominator);
}

auto calculations::operator!(const Rational& a) -> Rational
{    
    return Rational(a.denominator, a.numerator);
}

auto Rational::operator+=(const Rational& a) -> Rational&
{
    int divisor = gcd(denominator, a.denominator);

    if (not check_multiplication(denominator / divisor, a.denominator / divisor))
        throw IntegerOverflowException("operator+=(): integer multiplication overflow!");

    int new_denominator = (denominator / divisor) * (a.denominator / divisor);
    int mult1 = new_denominator / denominator;
    int mult2 = new_denominator / a.denominator;

    if (not check_addition(numerator * mult1, a.numerator * mult2))
        throw IntegerOverflowException("operator+=(): integer addition overflow!");
    
    int new_numerator = (numerator * mult1) + (a.numerator * mult2);
    
    this->numerator = new_numerator;
    this->denominator = new_denominator;

    normalize();

    return *this;
}

auto Rational::operator-=(const Rational& a) -> Rational&
{
    this->operator+=(-a);
    normalize();
    return *this;
}

auto Rational::operator*=(const Rational& a) -> Rational&
{
    if ((not check_multiplication(numerator, a.numerator)) or
        (not check_multiplication(denominator, a.denominator)))
        throw IntegerOverflowException("operator*=(): integer overflow!");

    numerator *= a.numerator;
    denominator *= a.denominator;
    normalize();

    return *this;
}

auto Rational::operator/=(const Rational& a) -> Rational&
{
    if (a.numerator == 0)
        throw DivisionByZero("operator/=(): division by zero!");

    this->operator*=(!a);

    normalize();
    return *this;
}