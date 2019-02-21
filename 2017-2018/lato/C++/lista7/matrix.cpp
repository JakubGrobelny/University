#include "matrix.hpp"

using namespace calculations;

auto Matrix::get_width() const -> int
{
    return this->columns;
}

auto Matrix::get_height() const -> int
{
    return this->rows;
}

// Zamiana kolumn.
void Matrix::swap_columns(int i, int j)
{
    if (j >= this->columns || i >= this->columns || i < 0 || j < 0)
        throw InvalidDoubleMatrixIndex(this->columns, this->columns, i, j);

    for (int row = 0; row < this->rows; row++)
    {
        float temp = (*this)[row][i];
        (*this)[row][i] = (*this)[row][j];
        (*this)[row][j] = temp;
    }
}

// Zamiana wierszy.
void Matrix::swap_rows(int i, int j)
{
    if (j >= this->rows || i >= this->rows || j < 0 || i < 0)
        throw InvalidDoubleMatrixIndex(this->rows, this->rows, i, j);

    float* temp = (*this)[i];
    this->a[i] = (*this)[j];
    this->a[j] = temp;
}

// Przemnażanie kolumny przez niezerowy skalar.
void Matrix::multiply_columns(int i, float scalar)
{
    if (scalar == 0.0f)
        throw InvalidScalar(scalar);

    for (int row = 0; row < this->rows; row++)
        (*this)[row][i] *= scalar;
}

// Przemnażanie wiersza przez niezerowy skalar.
void Matrix::multiply_rows(int i, float scalar)
{
    if (scalar == 0.0f)
        throw InvalidScalar(scalar);

    for (int column = 0; this->columns; column++)
        (*this)[i][column] *= scalar;
}

// Dodawanie kolumny przemnożonej przez skalar do innej kolumny.
void Matrix::add_columns(int which, float mult, int to)
{
    if (which < 0 || to < 0 || which >= this->columns || to >= this->columns)
        throw InvalidDoubleMatrixIndex(this->columns, this->columns, which, to);

    for (int row = 0; row < this->rows; row++)
        (*this)[row][to] += mult * (*this)[row][which];
}

// Dodawanie wiersza przemnożonego przez skalar do innego wiersza.
void Matrix::add_rows(int which, float mult, int to)
{
    if (which < 0 || to < 0 || which >= this->rows || to >= this->rows)
        throw InvalidDoubleMatrixIndex(this->rows, this->rows, which, to);

    for (int column = 0; column < this->columns; column++)
        (*this)[to][column] += mult * (*this)[which][column];
}

// Tworzenie macierzy z usiniętym wierszem i kolumną.
auto Matrix::removed_row_and_column(int row, int column) const -> Matrix
{
    if (row < 0 || column < 0 || row >= this->rows || column >= this->columns)
        throw InvalidDoubleMatrixIndex(this->rows, this->columns, row, column);

    Matrix result(this->rows - 1, this->columns - 1);

    int offset_col = 0;
    int offset_row = 0;

    for (int r = 0; r < this->rows; r++)
    {   
        if (r == row)
            offset_row = 1;
        else
        {
            for (int c = 0; c < this->columns; c++)
            {
                if (c == column)
                    offset_col = 1;
                else
                    result[r - offset_row][c - offset_col] = (*this)[r][c];
            }
        }

        offset_col = 0;
    }

    return result;
}

// Tworzenie macierzy transponowanej.
auto Matrix::transposed() -> Matrix
{
    Matrix result(this->columns, this->rows);

    for (int row = 0; row < result.rows; row++)
    {
        for (int column = 0; column < result.columns; column++)
        {
            result[row][column] = (*this)[column][row];
            result[column][row] = (*this)[row][column];
        }
    }

    return result;
}

// Dodawanie dwóch macierzy.
auto Matrix::operator+= (const Matrix& matrix) -> Matrix&
{
    if ((this->rows != matrix.rows) || (this->columns != matrix.columns))
        throw InvalidOperationArguments(this->rows, matrix.rows, this->columns, matrix.columns);

    for (int row = 0; row < this->rows; row++)
        for (int column = 0; column < this->columns; column++)
            (*this)[row][column] += matrix[row][column];

    return *this;
}

// Odejmowanie dwóch macierzy.
auto Matrix::operator-= (const Matrix& matrix) -> Matrix&
{
    if ((this->rows != matrix.rows) || (this->columns != matrix.columns))
        throw InvalidOperationArguments(this->rows, matrix.rows, this->columns, matrix.columns);


    for (int row = 0; row < this->rows; row++)
        for (int column = 0; column < this->columns; column++)
            (*this)[row][column] -= matrix[row][column];

    return *this;    
}

// Mnożenie dwóch macierzy.
auto Matrix::operator*= (const Matrix& matrix) -> Matrix&
{
    if (this->columns != matrix.rows)
        throw InvalidOperationArguments(this->rows, matrix.rows, this->columns, matrix.columns);

    int new_rows = this->rows;
    int new_columns = matrix.columns;

    Matrix result(new_rows, new_columns);

    for (int row = 0; row < new_rows; row++)
    {
        for (int column = 0; column < new_columns; column++)
        {
            float sum = 0.0f;
            
            for (int i = 0; i < new_columns; i++)
                sum += (*this)[row][i] * matrix[i][column];

            result[row][column] = sum;
        }
    }

    this->operator=(std::move(result));
    return *this;
}

// Mnożenie macierzy przez niezerowy skalar.
auto Matrix::operator*= (float scalar) -> Matrix&
{
    if (scalar == 0)
        throw InvalidScalar(scalar);

    for (int row = 0; row < this->rows; row++)
        for (int column = 0; column < this->columns; column++)
            (*this)[row][column] *= scalar;

    return *this;
}

// Przypisanie kopiujące.
auto Matrix::operator= (const Matrix& matrix) -> Matrix&
{
    if (&matrix != this)
    {
        // Zwalnianie dotychczasowej tablicy.
        for (int row = 0; row < this->rows; row++)
            delete this->a[row];
        delete this->a;

        // Kopiowanie danych.
        this->rows = matrix.rows;
        this->columns = matrix.columns;
        this->a = new float*[matrix.rows];

        for (int row = 0; row < matrix.rows; row++)
        {
            this->a[row] = new float[matrix.columns];

            for (int column = 0; column < matrix.columns; column++)
                (*this)[row][column] = matrix[row][column];
        }
    }

    return *this;
}

// Przypisanie przenoszące.
auto Matrix::operator= (Matrix&& matrix) -> Matrix&
{
    if (this != &matrix)
    {
        // Przenoszenie danych.
        this->rows = matrix.rows;
        this->columns = matrix.columns;

        matrix.rows = 0;
        matrix.columns = 0;

        std::swap(matrix.a, this->a);
    }

    return *this;
}

// Konstruktor tworzący nową macierz kwadratową jednostkową.
Matrix::Matrix(int size)
{
    if (size < 1)
        throw InvalidMatrixDimensions(size, size);

    this->a = new float*[size];
    this->rows = size;
    this->columns = size;

    for (int row = 0; row < size; row++)
    {
        this->a[row] = new float[size];

        for (int column = 0; column < size; column++)
        {
            if (column == row)
                (*this)[row][column] = 1.0f;
            else
                (*this)[row][column] = 0.0f;
        }
    }
}

// Konstruktor tworzący macierz prostokątną wypełnioną zerami.
Matrix::Matrix(int rows, int columns)
{
    if (rows < 1 || columns < 1)
        throw InvalidMatrixDimensions(rows, columns);

    this->a = new float*[rows];
    this->rows = rows;
    this->columns = columns;

    for (int row = 0; row < rows; row++)
    {
        this->a[row] = new float[columns];

        for (int column = 0; column < columns; column++)
            (*this)[row][column] = 0.0f;
    }
}

// Konstruktor kopiujący.
Matrix::Matrix(const Matrix& matrix)
{
    this->rows = matrix.rows;
    this->columns = matrix.columns;

    this->a = new float*[this->rows];

    for (int row = 0; row < this->rows; row++)
    {
        this->a[row] = new float[columns];
        
        for (int column = 0; column < columns; column++)
            (*this)[row][column] = matrix[row][column];
    }
}

// Konstruktor przenoszący.
Matrix::Matrix(Matrix&& matrix)
{
    this->rows = matrix.rows;
    this->columns = matrix.columns;
    this->a = matrix.a;
    
    matrix.rows = 0;
    matrix.columns = 0;
    matrix.a = nullptr;
}

// Destruktor
Matrix::~Matrix()
{
    for (int row = 0; row < this->rows; row++)
        delete this->a[row];
    delete this->a;
}

// Wypisywanie
auto calculations::operator<< (std::ostream& out, const Matrix& A) -> std::ostream&
{
    for (int row = 0; row < A.rows; row++)
    {
        for (int column = 0; column < A.columns; column++)
            out << A[row][column] << " ";
        out << std::endl;
    }

    return out;
}

// Wczytywanie
auto calculations::operator>> (std::istream& in, const Matrix& A) -> std::istream&
{
    for (int row = 0; row < A.rows; row++)
        for (int column = 0; column < A.columns; column++)
            in >> A[row][column];

    return in;
}

// Suma dwóch macierzy.
auto calculations::operator+ (const Matrix& A, const Matrix& B) -> Matrix
{
    Matrix result(A);
    result += B;
    return result;
}

// Różnica dwóch macierzy.
auto calculations::operator- (const Matrix& A, const Matrix& B) -> Matrix
{
    Matrix result(A);
    result -= B;
    return result;
}

// Iloczyn dwóch macierzy.
auto calculations::operator* (const Matrix& A, const Matrix& B) -> Matrix
{
    Matrix result(A);
    result *= B;
    return result;
}

// Macierz przemnożona przez skalar.
auto calculations::operator* (const Matrix& A, float scalar) -> Matrix
{
    Matrix result(A);
    result *= scalar;
    return result;
}
