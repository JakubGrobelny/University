#include <iostream>
#include <memory>

auto main(int argc, char* argv[]) -> int {

    std::weak_ptr<char> weak;

    {
        auto shared = std::make_shared<char>('A');
        weak = shared;
        std::cout << *weak.lock() << std::endl;
    }

    // segfault
    std::cout << *weak.lock() << std::endl;

    return EXIT_SUCCESS;
}