#include "binary.hpp"

auto Add::get_priority() -> int
{
    return ADD;
}

auto Sub::get_priority() -> int
{
    return SUB;
}

auto Mul::get_priority() -> int
{
    return MULT;
}

auto Div::get_priority() -> int
{
    return DIV;
}

auto Mod::get_priority() -> int
{
    return MOD;
}

auto Pow::get_priority() -> int
{
    return POW;
}

auto Log::get_priority() -> int
{
    return 0;
}

auto Add::evaluate() -> double
{
    return this->operand1->evaluate() + this->operand2->evaluate();
}

auto Add::to_string() -> std::string
{    
    std::string str_left = (this->operand1->get_priority() >= SUB) ? 
                                        ("(" + this->operand1->to_string() + ")") :
                                        (this->operand1->to_string());

    std::string str_right = (this->operand2->get_priority() >= SUB) ?
                                        ("(" + this->operand2->to_string() + ")") :
                                        (this->operand2->to_string());

    return str_left + " + " + str_right;
}

auto Sub::evaluate() -> double
{
    return operand1->evaluate() - operand2->evaluate();
}

auto Sub::to_string() -> std::string
{
    std::string str_left = this->operand1->get_priority() >= SUB ? 
                                        "(" + this->operand1->to_string() + ")" :
                                              this->operand1->to_string();

    std::string str_right = this->operand2->get_priority() >= SUB ?
                                        "(" + this->operand2->to_string() + ")" :
                                              this->operand2->to_string();

    return str_left + " - " + str_right;
}

auto Log::evaluate() -> double
{
    double base = this->operand1->evaluate();
    double arg = this->operand2->evaluate();

    try 
    {
        return std::log(arg) / std::log(base);
    }
    catch (std::invalid_argument exc)
    {
        throw exc;
    }
}

auto Log::to_string() -> std::string
{
    return "log[" + this->operand1->to_string() + "](" + this->operand2->to_string() + ")";
}


auto Mul::evaluate() -> double
{
    return this->operand1->evaluate() * this->operand2->evaluate();
}

auto Mul::to_string() -> std::string
{
    std::string str_left = this->operand1->get_priority() >= MULT ? 
                                        "(" + this->operand1->to_string() + ")" :
                                              this->operand1->to_string();

    std::string str_right = this->operand2->get_priority() >= MULT ?
                                        "(" + this->operand2->to_string() + ")" :
                                              this->operand2->to_string();

    return str_left + " * " + str_right;
}

auto Div::evaluate() -> double
{
    double divisor = this->operand2->evaluate();
    if (not divisor)
        throw std::invalid_argument("evaluate(): Division by zero!");
    else
        return this->operand1->evaluate() / divisor;
}

auto Div::to_string() -> std::string
{
    std::string str_left = this->operand1->get_priority() >= MULT ? 
                                        "(" + this->operand1->to_string() + ")" :
                                              this->operand1->to_string();

    std::string str_right = this->operand2->get_priority() >= MULT ?
                                        "(" + this->operand2->to_string() + ")" :
                                              this->operand2->to_string();

    return str_left + " / " + str_right;
}

auto Mod::evaluate() -> double
{
    double divisor = this->operand2->evaluate();
    if (not divisor)
        throw std::invalid_argument("evaluate(): (Mod) Division by zero!");
    else
        return std::fmod(this->operand1->evaluate(), divisor);
}

auto Mod::to_string() -> std::string
{
    std::string str_left = this->operand1->get_priority() >= MULT ? 
                                        "(" + this->operand1->to_string() + ")" :
                                              this->operand1->to_string();

    std::string str_right = this->operand2->get_priority() >= MULT ?
                                        "(" + this->operand2->to_string() + ")" :
                                              this->operand2->to_string();

    return str_left + " mod " + str_right;
}

auto Pow::evaluate() -> double
{
    return std::pow(this->operand1->evaluate(), this->operand2->evaluate());
}

auto Pow::to_string() -> std::string
{
    std::string str_left = this->operand1->get_priority() >= POW ? 
                                        "(" + this->operand1->to_string() + ")" :
                                              this->operand1->to_string();

    std::string str_right = this->operand2->get_priority() > POW ?
                                        "(" + this->operand2->to_string() + ")" :
                                              this->operand2->to_string();

    return str_left + " ^ " + str_right;
}

