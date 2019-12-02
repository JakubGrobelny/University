#include <iostream>
#include <algorithm>
#include <random>
#include <vector>

template <typename T>
void shuffle(std::vector<T>& vec) {
    static std::mt19937 gen {std::random_device{}()};

    for (int i = vec.size() - 1; i >= 0; i--) {
        int j = std::uniform_int_distribution<int>{0, i}(gen);
        std::swap(vec[i], vec[j]);
    }
}


auto main(int argc, char* argv[]) -> int {

    std::vector<char*> strings;

    if (argc < 2) {
        std::cerr << "Error: no input!" << std::endl;
        return EXIT_FAILURE;
    }

    for (int i = 1; i < argc; i++) {
        strings.push_back(argv[i]);
    }

    shuffle(strings);

    for (auto& str : strings) {
        std::cout << str << ' ';
    }
    std::cout << std::endl;

    return EXIT_SUCCESS;
}