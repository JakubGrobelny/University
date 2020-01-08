#include <regex>
#include "test.hpp"

auto is_valid_city(const std::string& str) -> bool {

    #define UPPER "([[:upper:]]|Ą|Ć|Ę|Ł|Ń|Ó|Ś|Ź|Ż)"
    #define LOWER "([[:lower:]]|ą|ć|ę|ł|ń|ó|ś|ź|ż)"

    std::regex city_regex(
        "^(" // assert the position is at the begining
            UPPER LOWER "{2,}" // single word
            "(-" UPPER LOWER "{2,})?" // possibly a second word after a hyphen
        "( +(?!$)|$)" // (non-trailing) space separators or last word
        ")+$" // one or more words
    );

    #undef UPPER
    #undef LOWER

    return std::regex_match(str, city_regex);
}


auto main() -> int {
    test_regex(is_valid_city);
}