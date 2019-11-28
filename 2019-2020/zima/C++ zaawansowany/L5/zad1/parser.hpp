#ifndef PARSER_HPP
#define PARSER_HPP

#include "lexer.hpp"
#include <stack>


auto parse_to_rpn(std::queue<token>&&) -> std::queue<token>;

#endif