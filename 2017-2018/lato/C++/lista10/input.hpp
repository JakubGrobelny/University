#ifndef INPUT_HPP
#define INPUT_HPP

#include <string>
#include <fstream>

namespace streams {

class input
{
    std::ifstream is;

    friend auto operator>>(input& in, int& target) -> input&;

public:
    auto eof() -> bool;
    input(std::string file_name);

    input(const input& in)          = delete;
    auto operator=(const input& in) = delete;
    input(input&& in)               = delete;
    auto operator=(input&& in)      = delete;

    ~input();
};

class output
{
    std::ofstream os;

    friend auto operator<<(output& out, int source) -> output&;

public:
    output(std::string file_name);
    
    output(const output& out)         = delete;
    auto operator=(const output& out) = delete;
    output(output&& out)              = delete;
    auto operator=(output&& out)      = delete;

    ~output();
};

auto operator>>(input& in, int& target) -> input&;
auto operator<<(output& out, int source) -> output&;

}

#endif