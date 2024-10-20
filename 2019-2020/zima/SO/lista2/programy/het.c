#include "../../lib/include/csapp.h"


static int conflict(int x1, int y1, int x2, int y2) 
{
    return x1 == x2
        || y1 == y2
        || x1 + y1 == x2 + y2
        || x1 - y1 == x2 - y2;
}

static void print_line_sep(int size) 
{
    for (int i = 0; i < size; ++i) 
        printf("+---");
    printf("+\n");
}

static void print_board(int size, int board[size]) 
{
    for (int i = 0; i < size; ++i) 
    {
        print_line_sep(size);
        for (int j = 0; j < size; ++j)
            printf("|%s", board[i] == j ? " Q " : "   ");
        printf("|\n");
    }
    print_line_sep(size);
    printf("\n");
}

static int ndselect(int n) 
{
    if (n == 0)
        return 0;
    else if (Fork())
        return n;
    else
        return ndselect(n-1);
}

int main(int argc, char **argv) 
{
    if (argc != 2)
        app_error("Usage: %s [SIZE]", argv[0]);

    const int size = atoi(argv[1]);

    if (size < 3 || size > 9)
        app_error("Give board size in range from 4 to 9!");

    int board[size];

    for (int i = 0; i < size; i++) 
    {
        board[i] = ndselect(size-1);

        if (board[i])
            Waitpid(-1, NULL, 0);

        for (int prev = 0; prev < i; prev++)
            if (conflict(i, board[i], prev, board[prev]))
                exit(0);
    }

    printf("\n[%d]: \n", getpid());
    print_board(size, board);

    return 0;
}
