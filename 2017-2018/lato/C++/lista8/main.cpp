#include <iostream>
#include "rational.hpp"

using namespace calculations;

auto main() -> int
{
    Rational a(2, 7);
    Rational b(3, 4);

    std::cout << Rational(100, 2) << std::endl;
    std::cout << a << std::endl;
    std::cout << b << std::endl;
    std::cout << Rational(7, 99) << std::endl;
    std::cout << Rational(656, 999) << std::endl;
    std::cout << Rational(13, 99) << std::endl;

    auto c = a * b;
    auto d = a + b;
    auto e = a / b;
    auto f = !a;
    auto g = -b;

    std::cout << c << std::endl;
    std::cout << d << std::endl;
    std::cout << e << std::endl;
    std::cout << f << std::endl;
    std::cout << g << std::endl;
    std::cout << (int)a << std::endl;
    std::cout << (int)Rational(7, 2) << std::endl;
    std::cout << (double)b << std::endl;
}