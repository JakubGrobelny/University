#ifndef MANIPULATORS_HPP
#define MANIPULATORS_HPP

#include <iostream>

namespace streams {

// Comma

auto comma(std::ostream& os) -> std::ostream&;

// Colon

auto colon(std::ostream& os) -> std::ostream&;

// Index

class index
{
    int x;
    int w;

    friend auto operator<<(std::ostream& os, const index& idx) -> std::ostream&;

public:

    index(int x, int w);
};

auto operator<<(std::ostream& os, const index& idx) -> std::ostream&;

// Ignore

class ignore
{
    int x;

    friend auto operator>>(std::istream& is, const ignore& ign) -> std::istream&;

public:
    ignore(int x);
};

auto operator>>(std::istream& is, const ignore& ign) -> std::istream&;


// Clearline

auto clearline(std::istream& is) -> std::istream&;

}

#endif