#include <iostream>
#include <limits>
#include <cmath>


auto main() -> int {

    constexpr auto max = std::numeric_limits<long long int>::max();
    constexpr auto min = std::numeric_limits<long long int>::min();

    std::cout << "Max long long int: " 
              << max
              << std::endl
              << "Min long long int: "
              << min
              << std::endl
              << "Size in bits: " // at least 64 according to the standard
              << sizeof(long long int) * 8
              << std::endl
              << "Number of decimal digits: "
              << std::floor(std::log10(max)) + 1
              << std::endl;
}