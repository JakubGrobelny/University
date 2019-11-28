#include "lexer.hpp"
#include <optional>
#include <cctype>
#include <exception>


static
auto is_valid_character(char c) -> bool {
    switch (c) {
        case static_cast<char>(token_type::plus):
        case static_cast<char>(token_type::minus):
        case static_cast<char>(token_type::div):
        case static_cast<char>(token_type::mult):
        case static_cast<char>(token_type::power):
        case static_cast<char>(token_type::left_bracket):
        case static_cast<char>(token_type::right_bracket):
            return true;
        default:
            return false;
    }
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

    if (is_valid_character(str.front())) {
        auto token = token_type(str.front());
        str.remove_prefix(1);
        return token;
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

