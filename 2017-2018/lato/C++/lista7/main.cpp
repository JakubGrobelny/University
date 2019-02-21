#include <iostream>
#include <algorithm>
#include <vector>

#include "matrix.hpp"
#include "exceptions.hpp"

using namespace calculations;

auto determinant(const Matrix& A) -> float
{
    if (A.get_height() == 1)
        return A[0][0];
    else if (A.get_height() == 2)
    {
        return (A[0][0] * A[1][1]) - (A[1][0] * A[0][1]);
    }
    else
    {
        float sum = 0.0f;

        for (int j = 0; j < A.get_height(); j++)
        {   
            if (A[j][0] == 0.0f)
                continue;

            sum += A[j][0] * 
                   determinant(A.removed_row_and_column(j, 0)) * 
                   (j % 2 ? -1.0f : 1.0f);
        }

        return sum;
    }
}

auto inverse_matrix(const Matrix& A) -> Matrix
{
    if (A.get_height() != A.get_width())
        throw InvalidMatrix();

    float inverse_det = 1 / determinant(A);

    Matrix adjugate(A.get_height());

    for (int i = 0; i < A.get_height(); i++)
    {
        for (int e = 0; e < A.get_width(); e++)
        {
            float minor_det = determinant(A.removed_row_and_column(i, e));
            
            if ((i + e) % 2)
                minor_det *= -1.0f;
            minor_det *= inverse_det;

            adjugate[i][e] = minor_det;
        }
    }

    return adjugate.transposed();
}

auto main() -> int
{
    int size;
    std::cout << "square matrix size: ";
    std::cin >> size;
    
    Matrix A(size);
    std::cout << "Enter the array: " << std::endl;
    std::cin >> A;

    std::cout << std::endl << "det = " << determinant(A) << std::endl;
    
    std::cout << std::endl << "Inverse matrix: " << std::endl;
    std::cout << inverse_matrix(A) << std::endl;

    std::cout << "Multiplied: " << std::endl;
    std::cout << inverse_matrix(A) * A << std::endl;

    std::cout << "Sum: " << std::endl << A + Matrix(A.get_height()) << std::endl;
    std::cout << "Substraction: " << std::endl << A - (A * 2.0f) << std::endl;

    Matrix B(4, 6);
    Matrix C(6, 3);

    std::cout << "Enter 4x6 matrix: " << std::endl;
    std::cin >> B;

    std::cout << "Enter 6x3 matrix: " << std::endl;
    std::cin >> C;

    std::cout << "Multiplication: " << std::endl << B * C << std::endl;
    std::cout << "Enter scalar: ";
    
    float scalar;
    std::cin >> scalar;
    std::cout << "Multiplication: " << std::endl << B * scalar << std::endl;
    
    std::cout << "Enter row and column to remove/swap: ";
    int row;
    int column;
    std::cin >> row >> column;

    std::cout << "Removed " << row << ", " << column << ":" << std::endl;
    std::cout << B.removed_row_and_column(row, column);

    B.swap_columns(row, column);
    std::cout << "Swapped -||- columns: " << std::endl << B << std::endl;

    B.swap_rows(row, column);
    std::cout << "-||- rows: " << std::endl << B << std::endl;

    A.add_columns(row, scalar, column);
    std::cout << "Added column * " << scalar << row << " to " << column << ": " << std::endl;
    std::cout << A << std::endl;

    A.add_rows(row, scalar, column);
    std::cout << "Added row * " << scalar << row << " to " << column << ": " << std::endl;
    std::cout << A << std::endl;

    return 0;
}