#include <iostream>
#include <limits>


auto main() -> int {
    std::cout << "Smallest float: "
              << std::numeric_limits<float>::denorm_min()
              << std::endl
              << "Smallest double: "
              << std::numeric_limits<double>::denorm_min()
              << std::endl
              << "Max float: "
              << std::numeric_limits<float>::max()
              << std::endl
              << "Max double: "
              << std::numeric_limits<double>::max()
              << std::endl
              << "(float) 1.0f - min: "
              << 1.0f - std::numeric_limits<float>::denorm_min()
              << std::endl
              << "(double) 1.0 - min: "
              << 1.0 - std::numeric_limits<double>::denorm_min()
              << std::endl;
}