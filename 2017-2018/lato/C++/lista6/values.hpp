#ifndef values_hpp
#define values_hpp

#include "expressions.hpp"

class Number : public Expression
{
    double value;

public:

    auto evaluate() -> double;
    auto to_string() -> std::string;
    auto get_priority() -> int;

    Number(double val) : value(val) {};

    virtual ~Number() = default;
};

class Variable : public Expression
{
    static std::vector<std::pair<std::string, double>> variables;

    std::string symbol;

public:

    static void add_variable(std::string symbol, double value);
    static void remove_variable(std::string symbol);
    static void rename_variable(std::string old_name, std::string new_name);
    static void change_value(std::string symbol, double new_value);
    static auto get_value(std::string symbol) -> double;
    static auto does_exist(std::string symbol) -> bool;

    auto evaluate() -> double;
    auto to_string() -> std::string;
    auto get_priority() -> int;

    Variable(std::string symbol) : symbol(symbol) {};

    virtual ~Variable() = default;
};

class Pi : public Constant
{
public:

    auto evaluate() -> double;
    auto to_string() -> std::string;
};

class E : public Constant
{
public:

    auto evaluate() -> double;
    auto to_string() -> std::string;

};

class Phi : public Constant
{
    static constexpr double value = 1.61803398875;

public:

    auto evaluate() -> double;
    auto to_string() -> std::string;
};

#endif
