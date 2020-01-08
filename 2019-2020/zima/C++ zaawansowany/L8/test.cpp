#include <iostream>
#include "test.hpp"


void test_regex(bool(*predicate)(const std::string&)) {
    while (!std::cin.eof()) {
        std::string input;
        std::getline(std::cin, input);
    
        if (input == "") {
            continue;
        }
 
        if (predicate(input)) {
            std::cout << "\033[92m" "Valid input" << std::endl;
        } else {
            std::cout << "\033[91m" "Invalid input" << std::endl;
        }

        std::cout << "\u001b[0m" << std::endl;
    }
}