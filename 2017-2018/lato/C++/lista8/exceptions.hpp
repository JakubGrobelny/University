#ifndef exceptions_hpp
#define exceptions_hpp

#include <stdexcept>

class RationalException : public std::exception 
{
    const char* str;
public:
    explicit RationalException(const char* what) : str(what) {};
    virtual auto what() const noexcept -> const char*  { return str; };
};

class DivisionByZero : public RationalException
{
public:
    explicit DivisionByZero(const char* what) : RationalException(what) {};
};

class IntegerOverflowException : public RationalException
{
public:
    explicit IntegerOverflowException(const char* what) : RationalException(what) {};
};

#endif