#ifndef exceptions_hpp
#define exceptions_hpp

#include <stdexcept>
#include <string>

class MatrixException : public std::exception
{
public:

    explicit MatrixException() {};
};

class InvalidSingleMatrixIndex : public MatrixException
{
protected:
    int upper_bound;
    int index;

public:

    explicit InvalidSingleMatrixIndex(int bound, int i) 
        : upper_bound(bound), index(i) {};

    virtual const char* what() const throw()
    {
        std::string error = "Invalid index " + 
                            std::to_string(upper_bound) + ", " + 
                            std::to_string(index) + " is max index and 1 is min index!";

        return error.c_str();
    }
};

class InvalidDoubleMatrixIndex final : public InvalidSingleMatrixIndex
{
    int upper_bound2;
    int index2;

public:

    explicit InvalidDoubleMatrixIndex(int b1, int b2, int i, int j)
        : InvalidSingleMatrixIndex(b1, i), upper_bound2(b2), index2(j) {};

    virtual const char* what() const throw()
    {
        std::string error = "Invalid index " + std::to_string(index) +
                            " or " + std::to_string(index2) + ", limits are " + 
                            std::to_string(upper_bound) + ", " + std::to_string(upper_bound2) + " and 1";

        return error.c_str();
    }
};

class InvalidScalar final : public MatrixException
{
    float scalar;
public:
    explicit InvalidScalar(float scalar) : scalar(scalar) {};

    virtual const char* what() const throw()
    {
        return ("Invalid scalar " + std::to_string(scalar)).c_str();
    }
};

class InvalidMatrix final : public MatrixException
{
public:

    explicit InvalidMatrix() {};

    virtual const char* what() const throw()
    {
        return "Given matrix is not square matrix!";
    }
};

class InvalidOperationArguments final : public MatrixException
{
    int r1;
    int r2;
    int c1;
    int c2;

public:
    
    explicit InvalidOperationArguments(int r1, int r2, int c1, int c2)
        : r1(r1), r2(r2), c1(c1), c2(c2) {};
    
    virtual const char* what() const throw()
    {
        return ("Invalid matrix dimensions during operation! " + 
                            std::to_string(r1) + "x" + 
                            std::to_string(c1) + " and " + 
                            std::to_string(r2) + "x" + 
                            std::to_string(c1)).c_str();
    }
};

class InvalidMatrixDimensions final : public MatrixException
{
    int r;
    int c;

public:

    explicit InvalidMatrixDimensions(int rows, int columns) : r(rows), c(columns) {};

    virtual const char* what() const throw()
    {
        return ("Invalid matrix dimensions!: " + 
                            std::to_string(r) + "x" + 
                            std::to_string(c)).c_str();
    }
};

#endif