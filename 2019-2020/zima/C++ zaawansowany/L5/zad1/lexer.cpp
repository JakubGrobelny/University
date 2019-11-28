#include "lexer.hpp"
#include <optional>


static
auto next_token(std::string_view& str) -> std::optional<token> {
    str.remove_prefix(1);
    return {};
}


auto tokenize_string(const std::string& str) -> std::queue<token> {
    std::string_view view = str;
    std::queue<token> tokens;
    return {};
}

