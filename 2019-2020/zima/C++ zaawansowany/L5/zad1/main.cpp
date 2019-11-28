#include <iostream>
#include <sstream>
#include "parser.hpp"


auto main(int argc, char* argv[]) -> int {
    if (argc == 1) {
        std::cerr << "No expression specified!" << std::endl;
        return EXIT_FAILURE;
    }

    std::stringstream sstream;
    for (int i = 1; i < argc; i++) {
        sstream << argv[i] << ' ';
    }

    std::queue<token> tokens;

    try {
        tokens = tokenize_string(sstream.str());
    } catch (std::exception exc) {
        std::cerr << exc.what() << std::endl;
        return EXIT_FAILURE;
    }

    while (!tokens.empty()) {
        auto token = tokens.front();

        if (std::holds_alternative<int>(token)) {
            std::cout << std::get<int>(token);
        } else if (std::holds_alternative<bracket>(token)) {
            std::cout << static_cast<char>(std::get<bracket>(token));
        } else {
            std::cout << static_cast<char>(std::get<op_token>(token));
        }

        std::cout << std::endl;

        tokens.pop();
    }

    return EXIT_SUCCESS;
}