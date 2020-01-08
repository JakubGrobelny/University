#include <regex>
#include "test.hpp"


auto is_valid_complex_number(const std::string& str) -> bool {
    std::regex complex_regex(
        "\\s*\\(\\s*" // opening parenthesis
            "[0-9]+(\\.[0-9]+)?" // first real number
            "\\s*\\+\\s*" // plus sign
            "[0-9]+(\\.[0-9]+)?" // second real number
            "[iI]" // i or I suffix for imaginary part
        "\\s*\\)\\s*" // closing parenthesis
    );

    return std::regex_match(str, complex_regex);
}


auto main() -> int {
    test_regex(is_valid_complex_number);
}