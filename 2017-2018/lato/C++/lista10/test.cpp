#include "test.hpp"
#include "manipulators.hpp"
#include "input.hpp"
#include <fstream>
#include <iostream>
#include <algorithm>

static const int TO_IGNORE = 3;

using namespace streams;

auto load_file(std::string file_name) -> line_t
{
    std::ifstream file;
    file.open(file_name);

    line_t lines;

    if (file.is_open())
    {
        int line_number = 1;
        std::string line;

        while (file.good())
        {
            file >> line;

            lines.push_back(std::make_pair(line_number, line));

            line_number++;
        }
    }
    else
    {
        std::cerr << "Failed to open " << file_name << std::endl;
        exit(1);
    }

    return lines;
}

void test_io(std::string file_in, std::string file_out)
{
    input in(file_in);
    
    std::vector<int> bytes;

    while (!in.eof())
    {
        int byte;
        in >> byte;
        bytes.push_back(byte);
    }

    // wypisywanie
    for (auto byte : bytes)
        std::cout << +byte << std::endl;
    std::cout << std::endl;

    for (uint64_t i = 0; i < bytes.size(); i++)
        bytes[i] = ~bytes[i];

    // zapisywanie
    output out(file_out);

    for (auto byte : bytes)
        out << byte;
}

void test_manipulators(std::string file_name)
{
    auto lines = std::move(load_file(file_name));

    const int line_num_length = std::to_string(lines.size()).length();

    std::sort(lines.begin(), lines.end(), [](std::pair<int, std::string> a,
                                             std::pair<int, std::string> b)
                                             {
                                                 return a.second < b.second;
                                             });

    for (auto line : lines)
        std::cout << index(line.first, line_num_length) << " " << line.second << std::endl;
}