#ifndef LEXER_HPP
#define LEXER_HPP

#include <variant>
#include <string>
#include <queue>

enum class token_type : char {
    plus          = '+', 
    minus         = '-', 
    div           = '/', 
    mult          = '*', 
    power         = '^',
    left_bracket  = '(', 
    right_bracket = ')'
};

using token = std::variant<std::string, token_type, int>;

auto tokenize_string(const std::string&) -> std::queue<token>;

#endif