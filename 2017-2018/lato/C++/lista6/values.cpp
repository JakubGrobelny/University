#include "values.hpp"

std::vector<std::pair<std::string, double>> Variable::variables;

auto Number::get_priority() -> int
{
    return 0;
}

auto Variable::get_priority() -> int
{
    return 0;
}

auto Number::evaluate() -> double
{
    return this->value;
}

auto Number::to_string() -> std::string
{
    return std::to_string(this->value);
}

void Variable::add_variable(std::string symbol, double value)
{
    if (does_exist(symbol))
    {
        for (auto& var : variables)
            if (symbol == var.first)
                var.second = value;
    }
    else
        variables.push_back({symbol, value});
}

void Variable::change_value(std::string symbol, double new_value)
{
    for (auto& var : variables)
    {
        if (symbol == var.first)
        {
            var.second = new_value;
            return;
        }
    }

    throw std::invalid_argument("change_value(): " + symbol + " does not exist!");
}

void Variable::rename_variable(std::string old_name, std::string new_name)
{
    bool exists = false;

    for (auto& var : variables)
    {
        if (old_name == var.first)
        {
            var.first = new_name;
            exists = true;
        }
    }

    if (!exists)
        throw std::invalid_argument("rename_variable(): " + old_name + " does not exist!");
}

auto Variable::get_value(std::string symbol) -> double
{
    for (auto& var : variables)
        if (var.first == symbol)
            return var.second;
    
    throw std::invalid_argument("get_value(): " + symbol + "does not exist!");
}

auto Variable::does_exist(std::string symbol) -> bool
{
    for (auto& var : variables)
        if (symbol == var.first)
            return true;

    return false;
}

auto Variable::evaluate() -> double
{
    return get_value(this->symbol);
}

auto Variable::to_string() -> std::string
{
    return this->symbol;
}

auto Pi::evaluate() -> double
{
    return M_PI;
}

auto Pi::to_string() -> std::string
{
    return "pi";
}


auto E::evaluate() -> double
{
    return M_E;
}

auto E::to_string() -> std::string
{
    return "e";
}


auto Phi::evaluate() -> double
{
    return Phi::value;
}

auto Phi::to_string() -> std::string
{
    return "phi";
}