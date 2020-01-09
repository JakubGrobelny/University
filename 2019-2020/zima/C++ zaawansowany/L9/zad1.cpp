#include <iostream>
#include <iterator>
#include <vector>
#include <algorithm>
#include <iomanip>


auto main() -> int {
    std::string line;
    std::getline(std::cin, line);
    std::istringstream stream(line);
    
    std::vector<float> numbers(std::istream_iterator<float>(stream), {});

    std::cout << std::setprecision(3);

    std::for_each(numbers.rbegin(), numbers.rend(), [](float x) {
        std::cout << x << ' ';
    });

    std::cout << std::endl;
}
