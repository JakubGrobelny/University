#ifndef LEXER_HPP
#define LEXER_HPP

#include <variant>
#include <string>
#include <queue>

enum class op_token : char {
    plus          = '+', 
    minus         = '-', 
    div           = '/', 
    mult          = '*', 
    power         = '^'
};

enum class bracket : char {
    left  = '(',
    right = ')'
};

enum class associativity {
    left, right
};

using token = std::variant<op_token, bracket, int>;

auto tokenize_string(const std::string&) -> std::queue<token>;

auto get_priority(op_token) -> int;

auto get_associativity(op_token) -> associativity;

#endif