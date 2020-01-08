#include <iostream>
#include <regex>
#include "test.hpp"


auto valid_time(const std::string& str) -> bool {
    std::regex time_regex(
        "(?:[0-1][0-9]|2[0-3])"
        ":"
        "(?:[0-5][0-9])"
        "(?::[0-5]?[0-9])?"
    );

    return std::regex_match(str, time_regex);
}

auto main() -> int {
    test_regex(valid_time);
}