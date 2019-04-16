#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>


int64_t powers_of_7[10];


void populate_powers_of_7()
{
    int64_t pow = 1;
    for (int i = 0; i < 10; i++)
    {
        powers_of_7[i] = pow;
        pow *= 7;
    }
}


inline int64_t get_power_of_7(char n)
{
    return powers_of_7[n - '0'];
}


inline int64_t max(int64_t a, int64_t b)
{
    return a > b ? a : b;
}


int64_t best_up(int64_t col, int64_t maxcols, int64_t* collected[3])
{
    const int64_t left = col - 2;
    const int64_t right = col + 2;
    const int64_t row_up = 2;

    int64_t lval = INT64_MIN;
    int64_t rval = INT64_MIN;

    if (left >= 0)
        lval = collected[row_up][left];

    if (right < maxcols)
        rval = collected[row_up][right];

    return max(rval, lval);
}


int64_t best_down(int64_t col, int64_t maxcols, int64_t* collected[3])
{
    const int64_t left = col - 1;
    const int64_t right = col + 1;
    const int64_t row_down = 0;

    int64_t lval = INT64_MIN;
    int64_t rval = INT64_MIN;

    if (left >= 0)
        lval = collected[row_down][left];

    if (right < maxcols)
        rval = collected[row_down][right];

    return max(rval, lval);
}


void go_up(int64_t cols, char* board[3], int64_t* collected[3])
{
    const int64_t row = 1;

    for (int64_t col = 0; col < cols; col++)
    {
        int64_t best_below = best_up(col, cols, collected);
     
        if (best_below <= 0)
            continue;
     
        int64_t pow = get_power_of_7(board[row][col]);
     
        // if (!collected[row][col])
        //     collected[row][col] = pow;

        collected[row][col] = max(best_below + pow, collected[row][col]);
    }
}


void go_down(int64_t cols, char* board[3], int64_t* collected[3])
{
    const int64_t row = 2;

    for (int64_t col = 0; col < cols; col++)
    {
        int64_t best_above = best_down(col, cols, collected);

        if (best_above <= 0)
            continue;

        collected[row][col] = best_above + get_power_of_7(board[row][col]);
    }
}


void swap_and_read(int64_t cols, char* board[3], int64_t* collected[3])
{
    void* temp = board[0];

    board[0] = board[1];
    board[1] = board[2];
    board[2] = temp;
    scanf("%s", board[2]);
    getchar();

    temp = collected[0];
    collected[0] = collected[1];
    collected[1] = collected[2];
    collected[2] = temp;

    for (int64_t col = 0; col < cols; col++)
        collected[2][col] = 0;
}


int64_t max_elem(int64_t* array, int64_t len)
{
    int64_t max = INT64_MIN;

    for (int64_t i = 0; i < len; i++)
    {
        int64_t elem = array[i];
        if (elem > max)
            max = elem;
    }

    return max;
}


void solve(int64_t rows, int64_t cols, char* board[3], int64_t* collected[3])
{
    for (int64_t row = 3; row < rows; row++)
    {
        go_down(cols, board, collected);
        go_up(cols, board, collected);
        
        swap_and_read(cols, board, collected);
    }

    go_down(cols, board, collected);

    printf("%lu\n", max_elem(collected[2], cols));
}


int main()
{
    populate_powers_of_7();

    int64_t rows;
    int64_t cols;

    scanf("%ld %ld", &rows, &cols);
    getchar();

    char* board[3] = {
        malloc(sizeof(char) * cols),
        malloc(sizeof(char) * cols),
        malloc(sizeof(char) * cols),
    };
    
    int64_t* collected[3] = {
        malloc(sizeof(int64_t) * cols),
        calloc(cols, sizeof(int64_t)),
        calloc(cols, sizeof(int64_t)),
    };

    for (int i = 0; i < 3; i++)
    {
        scanf("%s", board[i]);
        getchar();
    }


    for (int64_t i = 0; i < cols; i++)
        collected[0][i] = get_power_of_7(board[0][i]);

    solve(rows, cols, board, collected);

    for (int i = 0; i < 3; i++)
    {
        free(board[i]);
        free(collected[i]);
    }

    return 0;
}