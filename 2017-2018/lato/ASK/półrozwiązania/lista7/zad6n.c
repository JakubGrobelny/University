#include <stdio.h>

//int array[100];

typedef struct
{
    char a[3];
    int b;
    long c;
    float pi;
} str;

extern str baz;
//str* getptr();

int main()
{
    //str* x = getptr();
    //printf("%s %d %ld %f\n", x->a, x->b, x->c, x->pi);
    printf("%s %d %ld %f\n", baz.a, baz.b, baz.c, baz.pi);
    
    return 0;
}
