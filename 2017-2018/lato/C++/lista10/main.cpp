#include "manipulators.hpp"
#include "test.hpp"

auto main(int argc, char** argv) -> int
{
    // std::string str;
    // std::string str0;
    // std::cout << streams::comma << streams::colon << "clearline: " << std::endl;
    // std::cin >> streams::clearline >> str >> str0;
    // std::cout << str << " " << str0 << std::endl;

    std::cout << streams::index(0, 5) 
              << streams::comma 
              << streams::colon 
              << "ignore test: " 
              << std::endl;
              
    std::string str1;
    std::cin >> streams::ignore(4) >> str1;

    std::cout << str1 << std::endl;

    if (argc != 3)
    {
        std::cerr << "Invalid number of arguments!" << std::endl;
        return 1;   
    }

    test_manipulators("./test.txt");

    std::cout << std::endl << "file test: " << std::endl;

    test_io(argv[1], argv[2]);

    return 0;
}