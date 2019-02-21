#ifndef matrix_hpp
#define matrix_hpp

#include <iostream>
#include <utility>

#include "exceptions.hpp"

namespace calculations 
{

class Matrix
{
    float** a;
    int rows;
    int columns;

public:

    // Gettery.
    
    inline auto operator[] (int index) const -> float*
    {
        if (index < 0 || index >= this->rows)
            throw InvalidSingleMatrixIndex(this->rows, index);

        return this->a[index];
    };
    
    inline void set_row(int i, float* row) {this->a[i] = row;};

    auto get_width() const -> int;
    auto get_height() const -> int;

    // Metody modyfikujące macierz.
    void swap_columns(int i, int j);
    void swap_rows(int i, int j);

    void multiply_columns(int i, float scalar);
    void multiply_rows(int i, float scalar);

    void add_columns(int which, float mult, int to);
    void add_rows(int which, float mult, int to);

    // Metody zwracające nową macierz.
    auto removed_row_and_column(int row, int column) const -> Matrix;
    auto transposed() -> Matrix;

    // Operatory przypisania.
    auto operator= (const Matrix& matrix) -> Matrix&;
    auto operator= (Matrix&& matrix)      -> Matrix&;

    // Operatory modyfikujące macierz.
    auto operator+= (const Matrix& matrix) -> Matrix&;
    auto operator-= (const Matrix& matrix) -> Matrix&;
    auto operator*= (const Matrix& matrix) -> Matrix&;
    auto operator*= (float scalar)         -> Matrix&;

    // Konstruktory.
    Matrix(int size);
    Matrix(int rows, int columns);
    Matrix(const Matrix& matrix);
    Matrix(Matrix&& matrix);

    // Destruktor.
    ~Matrix();

    // Zaprzyjaźnione funkcje wejścia/wyjścia.
    friend auto operator<< (std::ostream& out, const Matrix& A) -> std::ostream&;
    friend auto operator>> (std::istream& in, const Matrix& A) -> std::istream&;
    
    // Zaprzyjaźnione operatory tworzące nowe macierze.1
    friend auto operator+ (const Matrix& A, const Matrix& B) -> Matrix;
    friend auto operator- (const Matrix& A, const Matrix& B) -> Matrix;
    friend auto operator* (const Matrix& A, const Matrix& B) -> Matrix;
    friend auto operator* (const Matrix& A, float scalar)    -> Matrix;
};

auto operator<< (std::ostream& out, const Matrix& A) -> std::ostream&;
auto operator>> (std::istream& in, const Matrix& A) -> std::istream&;

auto operator+ (const Matrix& A, const Matrix& B) -> Matrix;
auto operator- (const Matrix& A, const Matrix& B) -> Matrix;
auto operator* (const Matrix& A, const Matrix& B) -> Matrix;
auto operator* (const Matrix& A, float scalar)    -> Matrix;

}

#endif