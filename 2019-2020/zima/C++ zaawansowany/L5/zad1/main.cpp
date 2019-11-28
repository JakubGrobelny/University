#include <iostream>
#include <sstream>
#include "lexer.hpp"


auto main(int argc, char* argv[]) -> int {
    if (argc == 1) {
        std::cerr << "No expression specified!" << std::endl;
        return EXIT_FAILURE;
    }

    std::stringstream sstream;
    for (int i = 1; i < argc; i++) {
        sstream << argv[i] << ' ';
    }

    auto tokens = tokenize_string(sstream.str());

    while (!tokens.empty()) {
        auto token = tokens.front();

        if (std::holds_alternative<int>(token)) {
            std::cout << std::get<int>(token);
        } else {
            std::cout << static_cast<char>(std::get<token_type>(token));
        }

        std::cout << std::endl;

        tokens.pop();
    }

    return EXIT_SUCCESS;
}