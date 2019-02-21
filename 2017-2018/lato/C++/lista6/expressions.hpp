#ifndef expressions_hpp
#define expressions_hpp

#include <string>
#include <utility>
#include <vector>
#include <cmath>
#include <stdexcept>

enum operatorType
{
    ADD = 6,
    SUB = 5,
    MULT = 4,
    DIV = 3,
    MOD = 2,
    POW = 1
};

class Expression
{
public:

    virtual auto evaluate() -> double = 0;
    virtual auto to_string() -> std::string = 0;
    virtual auto get_priority() -> int = 0;

    virtual ~Expression() = default;
};

class Constant : public Expression
{
    auto get_priority() -> int {return 0;};
public:
    virtual ~Constant() = default;
};

class UnaryOperator : public Expression
{
protected:
    auto get_priority() -> int {return 0;};
    Expression* operand1;
    UnaryOperator(Expression* expr) : operand1(expr) {};

public:
    virtual ~UnaryOperator()
    {
        delete this->operand1;
        delete this;
    }
};

class BinaryOperator : public UnaryOperator
{
protected:
    Expression* operand2;
    BinaryOperator(Expression* expr1, Expression* expr2) : UnaryOperator(expr1), operand2(expr2) {};

public:
    virtual ~BinaryOperator()
    {
        delete this->operand1;
        delete this->operand2;
        delete this;
    }
};

#endif
