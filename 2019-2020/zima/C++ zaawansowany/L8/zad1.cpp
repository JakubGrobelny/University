#include <regex>
#include "test.hpp"


auto is_valid_time(const std::string& str) -> bool {
    std::regex time_regex(
        "^(?:[0-1][0-9]|2[0-3])" // 00-23 (two digits)
        ":"
        "(?:[0-5][0-9])" // 00-59 (two digits)
        "(?::[0-5]?[0-9])?" // 0-59 (one or two digits)
    );

    return std::regex_match(str, time_regex);
}

auto main() -> int {
    test_regex(is_valid_time);
}