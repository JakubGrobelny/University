#include <iostream>
#include <complex>


constexpr double euler_mascheroni = 0.5772156649;

auto gamma(std::complex<double> z, uint64_t i) -> std::complex<double> {
    std::complex<double> result = 1.0;
    for (uint64_t n = 1; n <= i; n++) {
        auto nf = static_cast<double>(n);
        result *= std::pow(1.0 + 1.0/nf, z) / (1.0 + z/nf);
    }

    return result / z;
}


auto inv_gamma(std::complex<double> z, uint64_t i) -> std::complex<double> {
    // return 1.0 / gamma(z, n);
    std::complex<double> result = 1.0;
    for (uint64_t n = 1; n <= i; n++) {
        auto nf = static_cast<double>(n);
        result *= ((1.0 + z/nf) * std::exp(-z/nf));
    }
    return result * z * std::exp(euler_mascheroni * z);
}


auto main() -> int {

    uint64_t n;
    double real;
    double imag;

    std::cout << "n: ";
    std::cin >> n;

    std::cout << "punkt: ";
    std::cin >> real >> imag;

    auto point = std::complex<double>(real, imag);

    std::cout << "Γ(" << point << ") = " << gamma(point, n) << std::endl;
    std::cout << "1/Γ(" << point << ") = " << inv_gamma(point, n) << std::endl;
}