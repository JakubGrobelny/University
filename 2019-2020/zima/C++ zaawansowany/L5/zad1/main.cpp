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
    } catch (std::exception& exc) {
        std::cerr << exc.what() << std::endl;
        return EXIT_FAILURE;
    }

    std::queue<token> rpn_expr;

    try {
        rpn_expr = parse_to_rpn(std::move(tokens));
    } catch (std::exception& exc) {
        std::cerr << exc.what() << std::endl;
        return EXIT_FAILURE;
    }

    std::cout << "\u001b[31;1m" << "RPN: " << "\u001b[0m";

    while (!rpn_expr.empty()) {
        auto token = rpn_expr.front();

        if (std::holds_alternative<int>(token)) {
            std::cout << std::get<int>(token);
        } else if (std::holds_alternative<bracket_token>(token)) {
            std::cout << static_cast<char>(std::get<bracket_token>(token));
        } else {
            std::cout << static_cast<char>(std::get<op_token>(token));
        }

        std::cout << ' ';

        rpn_expr.pop();
    }

    std::cout << std::endl;

    return EXIT_SUCCESS;
}