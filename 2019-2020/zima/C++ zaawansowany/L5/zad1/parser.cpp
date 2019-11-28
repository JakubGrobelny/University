#include "parser.hpp"
#include <exception>
#include <iostream>


auto parse_to_rpn(std::queue<token>&& tokens) -> std::stack<token> {
    std::stack<token> num_stack;
    std::stack<token> op_stack;
    std::queue<token> out;
    
    // TODO: implement

    while (!tokens.empty()) {
        auto token = tokens.front();
        tokens.pop();

        if (std::holds_alternative<int>(token)) {
            out.push(token);
        }
    }
}
