#ifndef binary_hpp
#define binary_hpp

#include "expressions.hpp"

class Add : public BinaryOperator
{
public:

    auto evaluate() -> double;
    auto to_string() -> std::string;
    auto get_priority() -> int;

    Add(Expression* expr1, Expression* expr2) : BinaryOperator(expr1, expr2) {};
};

class Log : public BinaryOperator
{
public:

    auto evaluate() -> double;
    auto to_string() -> std::string;
    auto get_priority() -> int;

    Log(Expression* expr1, Expression* expr2) : BinaryOperator(expr1, expr2) {};

};

class Sub : public BinaryOperator
{
public:

    auto evaluate() -> double;
    auto to_string() -> std::string;
    auto get_priority() -> int;

    Sub(Expression* expr1, Expression* expr2) : BinaryOperator(expr1, expr2) {};
    
};

class Mul : public BinaryOperator
{
public:

    auto evaluate() -> double;
    auto to_string() -> std::string;
    auto get_priority() -> int;

    Mul(Expression* expr1, Expression* expr2) : BinaryOperator(expr1, expr2) {};

};

class Div : public BinaryOperator
{
public:

    auto evaluate() -> double;
    auto to_string() -> std::string;
    auto get_priority() -> int;

    Div(Expression* expr1, Expression* expr2) : BinaryOperator(expr1, expr2) {};

};

class Mod : public BinaryOperator
{
public:

    auto evaluate() -> double;
    auto to_string() -> std::string;
    auto get_priority() -> int;

    Mod(Expression* expr1, Expression* expr2) : BinaryOperator(expr1, expr2) {};

};

class Pow : public BinaryOperator
{
public:

    auto evaluate() -> double;
    auto to_string() -> std::string;
    auto get_priority() -> int;

    Pow(Expression* expr1, Expression* expr2) : BinaryOperator(expr1, expr2) {};

};

#endif