#include <iostream>
#include <cmath>
#include <complex>


void solve_quadratic_equation(int a, int b, int c) {

    enum class ord {
        greater, lesser, equal
    };

    const auto cmp = [](int a, int b) -> ord {
        if (a > b)
            return ord::greater;
        if (a < b)
            return ord::lesser;
        return ord::equal;
    };

    switch (auto delta = b*b - 4*a*c; cmp(delta, 0))
    {
        case ord::greater: {
            double sqrt_delta = std::sqrt(delta);
            double x0 = (-b + sqrt_delta) / (2 * a);
            double x1 = (-b - sqrt_delta) / (2 * a);
            std::cout << "x0 = " << x0 << std::endl 
                      << "x1 = " << x1 << std::endl;
            return;
        }
        case ord::lesser: {
            using namespace std::complex_literals;
            auto sqrt_delta = std::sqrt(-delta) * 1i / (2.0 * a);
            auto x0 = std::complex<double>((-b/(2.0*a))) + sqrt_delta;
            auto x1 = std::complex<double>((-b/(2.0*a))) - sqrt_delta;
            std::cout << "x0 = " << x0 << std::endl 
                      << "x1 = " << x1 << std::endl;
            return;
        }
        default: {
            auto x0 = -b / (2*a);
            std::cout << "x0 = " << x0 << std::endl;
            return;
        }
    }
}

auto main() -> int {
    std::cout << "x^2 - 2x:" << std::endl;
    solve_quadratic_equation(2, -2, 0);

    std::cout << std::endl;

    std::cout << "5x^2:" << std::endl;
    solve_quadratic_equation(5, 0, 0);

    std::cout << std::endl;

    std::cout << "2x^2 + 2x + 1: " << std::endl;
    solve_quadratic_equation(2, 2, 1);

    return 0;
}