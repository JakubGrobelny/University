#include <iostream>
#include <set>


using strings = std::set<std::string>;

auto main() -> int {

    strings strs{"abc", "123", R"(\n)", "c++"};

    for (auto str : strs)
        std::cout << str << std::endl;
    
    return 0;
}