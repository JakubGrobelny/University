#include <iostream>
#include <algorithm>
#include <random>
#include <vector>


auto main(int argc, char* argv[]) -> int {

    std::vector<char*> strings;

    if (argc < 2) {
        std::cerr << "Error: no input!" << std::endl;
        return EXIT_FAILURE;
    }

    for (int i = 1; i < argc; i++) {
        strings.push_back(argv[i]);
    }

    std::mt19937 gen {std::random_device{}()};

    std::shuffle(strings.begin(), strings.end(), gen);

    for (auto& str : strings) {
        std::cout << str << ' ';
    }
    std::cout << std::endl;

    return EXIT_SUCCESS;
}