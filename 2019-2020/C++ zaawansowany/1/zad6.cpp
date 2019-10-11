#include <iostream>
#include <cmath>
#include <complex>


void solve_quadratic_equation(double a, double b, double c) {

    auto approx_zero = [](double x) -> bool {
        const float epsilon = 0.000001;
        return x > -epsilon && x < epsilon;
    };

    if (double delta = b*b - 4.0 * a * c; approx_zero(delta)) {
        const double x = -b / (2.0 * a);

        std::cout << "x = " << x << std::endl;
    } else if (delta > 0) {
        const double sqrt_delta = std::sqrt(delta);
        const double x0 = (-b + sqrt_delta) / (2*a);
        const double x1 = c / a * x0;

        std::cout << "x0 = " << x0 << std::endl << "x1 = " << x1 << std::endl;
    } else {
        using namespace std::complex_literals;
        const std::complex<double> im = std::sqrt(-delta) * 1i / (2.0*a);
        const double real = -b / (2 * a);

        const std::complex<double> x0 = real + im;
        const std::complex<double> x1 = real - im;
        
        std::cout << "x0 = " << x0 << std::endl << "x1 = " << x1 << std::endl;
    }
}

auto main() -> int {

    std::cout << "x^2 - 2x:" << std::endl;
    solve_quadratic_equation(2, -2, 0);

    std::cout << std::endl;

    std::cout << "x^2 - 2x + 1:" << std::endl;
    solve_quadratic_equation(1, -2, 1);

    std::cout << std::endl;

    std::cout << "2x^2 + 2x + 1: " << std::endl;
    solve_quadratic_equation(2, 2, 1);

    return 0;
}