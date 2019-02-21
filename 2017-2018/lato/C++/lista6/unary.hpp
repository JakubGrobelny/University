#ifndef unary_hpp
#define unary_hpp

#include "expressions.hpp"

class Sin : public UnaryOperator
{
public:
    
    auto evaluate() -> double;
    auto to_string() -> std::string;

    Sin(Expression* expr) : UnaryOperator(expr) {};

};

class Cos : public UnaryOperator
{
public:
    
    auto evaluate() -> double;
    auto to_string() -> std::string;

    Cos(Expression* expr) : UnaryOperator(expr) {};
};

class Abs : public UnaryOperator
{
public:
    
    auto evaluate() -> double;
    auto to_string() -> std::string;

    Abs(Expression* expr) : UnaryOperator(expr) {};
};

class Exp : public UnaryOperator
{
public:
    
    auto evaluate() -> double;
    auto to_string() -> std::string;

    Exp(Expression* expr) : UnaryOperator(expr) {};
};

class Inverse : public UnaryOperator
{
public:
    
    auto evaluate() -> double;
    auto to_string() -> std::string;
    
    Inverse(Expression* expr) : UnaryOperator(expr) {};
};

class Opposite : public UnaryOperator
{
public:
    
    auto evaluate() -> double;
    auto to_string() -> std::string;

    Opposite(Expression* expr) : UnaryOperator(expr) {};
};

class Ln : public UnaryOperator
{
public:
    
    auto evaluate() -> double;
    auto to_string() -> std::string;

    Ln(Expression* expr) : UnaryOperator(expr) {};
};

#endif