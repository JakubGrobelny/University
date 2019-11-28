#include "lexer.hpp"
#include <optional>
#include <cctype>
#include <exception>


static
auto is_valid_op_token(char c) -> bool {
    switch (c) {
        case static_cast<char>(op_token::plus):
        case static_cast<char>(op_token::minus):
        case static_cast<char>(op_token::div):
        case static_cast<char>(op_token::mult):
        case static_cast<char>(op_token::power):
            return true;
        default:
            return false;
    }
}


static
auto is_valid_bracket(char c) -> bool {
    return c == static_cast<char>(bracket_token::left) ||
           c == static_cast<char>(bracket_token::right);
}


static
void skip_whitespace(std::string_view& str) {
    while (!str.empty() && std::isspace(str.front())) {
        str.remove_prefix(1);
    }
}


static
auto next_token(std::string_view& str) -> std::optional<token> {
    skip_whitespace(str);
    
    if (str.empty()) {
        return {};
    }

    if (std::isdigit(str.front())) {
        int val = str[0] - '0';
        str.remove_prefix(1);
        while (!str.empty() && std::isdigit(str.front())) {
            val = val * 10 + (str.front() - '0');
            str.remove_prefix(1);
        }
        return val;
    }

    char c = str.front();
    str.remove_prefix(1);

    if (is_valid_op_token(c)) {
        return op_token(c);
    }

    if (is_valid_bracket(c)) {
        return bracket_token(c);
    }

    throw std::invalid_argument("Invalid character in an expression!");
}


auto tokenize_string(const std::string& str) -> std::queue<token> {
    std::string_view view = str;
    std::queue<token> tokens;
    std::optional<token> token = {};
    while ((token = next_token(view)).has_value()) {
        tokens.push(*token);
    }

    return tokens;
}


auto get_priority(op_token token) -> int {
    switch (token) {
        case op_token::plus:
        case op_token::minus:
            return 0;
        case op_token::div:
        case op_token::mult:
            return 1;
        case op_token::power:
            return 2;
    }
}


auto get_associativity(op_token token) -> associativity {
    switch (token) {
        case op_token::power:
            return associativity::right;
        default:
            return associativity::left;
    }
}