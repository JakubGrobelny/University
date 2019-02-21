#include "manipulators.hpp"
#include <stdexcept>

auto streams::comma(std::ostream& os) -> std::ostream&
{
    os << ", ";
    return os;
}

auto streams::colon(std::ostream& os) -> std::ostream&
{
    os << ": ";
    return os;
}

// Index
auto streams::operator<<(std::ostream& os, const streams::index& idx) -> std::ostream&
{
    os << "[";

    auto number = std::to_string(idx.x);
    int offset = idx.w - number.length();

    for (int i = 0; i < offset; i++)
        os << " ";
    
    os << number;

    os << "]";
    return os;
}

streams::index::index(int x, int w)
{
    if (w < 0)
        throw std::invalid_argument("index(): \"w\" can't be negative!");

    this->x = x;
    this->w = w;
}

// Ignore
auto streams::operator>>(std::istream& is, const streams::ignore& ign) -> std::istream&
{
    char c;
    int counter = 0;

    while (counter < ign.x)
    {
        counter++;
        c = is.get();

        if (c == EOF or c == '\n')
            break;
    }

    return is;
}

auto streams::clearline(std::istream& is) -> std::istream&
{
    std::string str;
    getline(is, str);

    return is;
}

streams::ignore::ignore(int x)
{
    if (x < 0)
        throw std::invalid_argument("ignore(): number of characters can't be negative!");
    this->x = x;
}