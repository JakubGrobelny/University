#include "unary.hpp"

auto Sin::evaluate() -> double
{
    return std::sin(this->operand1->evaluate());
}

auto Sin::to_string() -> std::string
{
    return "sin(" + this->operand1->to_string() + ")";
}

auto Cos::evaluate() -> double
{
    return std::cos(this->operand1->evaluate());
}

auto Cos::to_string() -> std::string
{
    return "cos(" + this->operand1->to_string() + ")";
}

auto Abs::evaluate() -> double
{
    double result = this->operand1->evaluate();
    return result < 0 ? -result : result;
}

auto Abs::to_string() -> std::string
{
    return "|" + this->operand1->to_string() + "|";
}

auto Opposite::evaluate() -> double
{
    return -this->operand1->evaluate();
}

auto Opposite::to_string() -> std::string
{
    if (this->operand1->get_priority() > 1)
        return "-(" + this->operand1->to_string() + ")";
    else
        return "-" + this->operand1->to_string();
}

auto Inverse::evaluate() -> double
{
    double result = this->operand1->evaluate();
    if (not result)
        throw std::invalid_argument("Inverse(): division by zero!");
    else
        return 1 / result;
}

auto Inverse::to_string() -> std::string
{
    if (this->operand1->get_priority() > 0)
        return "1/(" + this->operand1->to_string() + ")";
    else
        return "1 / " + this->operand1->to_string();
}

auto Exp::evaluate() -> double
{
    return std::exp(this->operand1->evaluate());
}

auto Exp::to_string() -> std::string
{
    return "exp(" + this->operand1->to_string() + ")";
}

auto Ln::evaluate() -> double
{
    return std::log(this->operand1->evaluate());
}

auto Ln::to_string() -> std::string
{
    return "ln(" + this->operand1->to_string() + ")";
}

