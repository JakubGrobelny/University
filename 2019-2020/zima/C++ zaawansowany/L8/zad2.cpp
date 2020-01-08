#include <regex>
#include "test.hpp"


auto is_valid_date(const std::string& str) -> bool {
    std::regex date_regex(
        "(?:"
            "31-(?:01|03|07|08|10|12)" // months with 31 days
            "|"
            "30-(?:04|06|09|11)" // months with 30 days
            "|"
            "(?:0[1-9]|[1-2][0-9])-(?:0[1-9]|1[0-2])" // [01-29]-[01-12]
        ")-\\d{4,}" // -YYYY
    );

    return std::regex_match(str, date_regex);
}


auto main(int argc, char* argv[]) -> int {
    test_regex(is_valid_date);
}