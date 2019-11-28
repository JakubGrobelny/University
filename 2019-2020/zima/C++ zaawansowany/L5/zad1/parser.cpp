#include "parser.hpp"
#include <exception>
#include <iostream>


static
auto should_remove_top_op(int priority, int top_priority, associativity assoc) {
    switch (assoc) {
        case associativity::left:
            return top_priority >= priority;
        default:
            return top_priority > priority;
    }
}


auto parse_to_rpn(std::queue<token>&& tokens) -> std::queue<token> {
    std::stack<token> stack;
    std::queue<token> out;
    
    while (!tokens.empty()) {
        auto token = tokens.front();
        tokens.pop();

        if (std::holds_alternative<int>(token)) {
            out.push(token);
        } else if (std::holds_alternative<op_token>(token)) {
            auto op = std::get<op_token>(token);
            auto assoc = get_associativity(op);
            auto priority = get_priority(op);

            while (!stack.empty()) {
                if (!std::holds_alternative<op_token>(stack.top())) {
                    break;
                }

                auto top_op = std::get<op_token>(stack.top());
                auto top_priority = get_priority(top_op);

                if (!should_remove_top_op(priority, top_priority, assoc)) {
                    break;
                }

                out.push(stack.top());
                stack.pop();
            }

            stack.push(token);

        } else {
            switch (std::get<bracket_token>(token)) {
                case bracket_token::right: {
                    while (!stack.empty()) {
                        auto& top = stack.top();
                        if (std::holds_alternative<bracket_token>(top)) {
                            break;
                        }

                        out.push(top);
                        stack.pop();
                    }

                    if (stack.empty()) {
                        throw std::invalid_argument("Unbalanced brackets!");
                    }

                    stack.pop();
                    break;
                }
                case bracket_token::left:
                    stack.push(bracket_token::right);
                    break;
            }
        }
    }

    while (!stack.empty()) {
        auto& top = stack.top();
        if (std::holds_alternative<bracket_token>(top)) {
            throw std::invalid_argument("Unbalanced brackets!");
        }

        out.push(top);
        stack.pop();
    }

    return out;
}
