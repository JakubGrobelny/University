#ifndef rational_hpp
#define rational_hpp

#include <iostream>
#include <limits>
#include <cmath>
#include <vector>
#include <utility>

#include "exceptions.hpp"

namespace calculations {

class Rational
{
    int numerator;
    int denominator;

    static auto gcd(int a, int b) noexcept -> int;
    void normalize();

public:

    inline auto get_numerator() const noexcept -> int;
    inline auto get_denominator() const noexcept -> int;
    
    Rational(int number);
    Rational(int numerator, int denominator);
    Rational(const Rational& rational) = default;
    auto operator=(const Rational& rational) -> Rational& = default;

    explicit operator int() const;
    operator double() const;


    auto operator+=(const Rational& a) -> Rational&;
    auto operator-=(const Rational& a) -> Rational&;
    auto operator*=(const Rational& a) -> Rational&;
    auto operator/=(const Rational& a) -> Rational&;

    friend auto operator+(const Rational& a, const Rational& b) -> Rational;
    friend auto operator-(const Rational& a, const Rational& b) -> Rational;
    friend auto operator*(const Rational& a, const Rational& b) -> Rational;
    friend auto operator/(const Rational& a, const Rational& b) -> Rational;
    friend auto operator<<(std::ostream& out, const Rational& a) noexcept -> std::ostream&;

    friend auto operator!(const Rational& a) -> Rational;
    friend auto operator-(const Rational& a) -> Rational;
};

auto operator!(const Rational& a) -> Rational;
auto operator-(const Rational& a) -> Rational;

auto operator<<(std::ostream& out, const Rational& a) noexcept -> std::ostream&;
auto operator+(const Rational& a, const Rational& b) -> Rational;
auto operator-(const Rational& a, const Rational& b) -> Rational;
auto operator*(const Rational& a, const Rational& b) -> Rational;
auto operator/(const Rational& a, const Rational& b) -> Rational;

}
#endif