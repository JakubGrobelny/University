#include <iostream>
#include <complex>


auto dzeta(std::complex<double> z, uint64_t iters) -> std::complex<double> {
    std::complex<double> result = 0.0;
    for (uint64_t n = 1; n <= iters; n++){
        result += 1.0 / std::pow(n, z);
    }
    return result;
}


auto main() -> int {
    uint64_t scale;
    std::cin >> scale;

    uint64_t iterations;
    std::cin >> iterations;

    double range_start;
    double range_end;

    std::cin >> range_start >> range_end;

    auto dzeta_critical = [&](double imag) {
        return dzeta({0.5, imag}, iterations);
    };

    const double step = 1.0 / scale;
    for (double point = range_start; point <= range_end; point += step) {
        const auto value = dzeta_critical(point);
        std::cout << point        << ", " 
                  << value.real() << ", "
                  << value.imag() << std::endl;
    }
}